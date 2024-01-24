# https://github.com/afsc-gap-products/gapindex/blob/main/vignettes/ex_agecomps_ebs_standard.Rmd

sql_channel <- gapindex::get_connected()

spp_start_year <-
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.SPECIES_YEAR")

production_data <- gapindex::get_data(
  year_set = 1982:2023,
  survey_set = "EBS",
  spp_codes = 10130,
  pull_lengths = TRUE, 
  haul_type = 3, 
  abundance_haul = "Y",
  sql_channel = sql_channel)

# save(production_data, file = here::here(year, 'data','raw','gapindex_production_data.rdata'))

## Remove hauls and data associated with hauls in strata 82 and 90
ebs_standard_hauls <- 
  with(production_data$haul, HAULJOIN[!(STRATUM %in% c(82, 90))])

production_data$haul <- 
  subset(x = production_data$haul, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$catch <- 
  subset(x = production_data$catch, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$size <- 
  subset(x = production_data$size, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$specimen <- 
  subset(x = production_data$specimen, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$strata <- 
  subset(x = production_data$strata, 
         subset = !(STRATUM %in% c(82, 90)))

## Remove subareas associated with the EBS + NW region
production_data$subarea <- 
  subset(x = production_data$subarea, 
         subset = !(AREA_ID %in% c(7, 8, 9, 100, 200, 300, 99900)))

## Calculate and zero-fill CPUE
production_cpue <-
  gapindex::calc_cpue(racebase_tables = production_data)

## Calculate biomass/abundance (w/variance), mean/variance CPUE across strata
production_biomass_stratum <-
  gapindex::calc_biomass_stratum(
    racebase_tables = production_data,
    cpue = production_cpue)

## Calculate size composition by stratum. Since the two regions have
## different functions, sizecomp_fn toggles which function to use
## and then it is called in the do.call function.
production_sizecomp_stratum <- 
  gapindex::calc_sizecomp_stratum(
    racebase_tables = production_data,
    racebase_cpue = production_cpue,
    racebase_stratum_popn = production_biomass_stratum,
    spatial_level = "stratum",
    fill_NA_method = "BS")

# Calculate regional ALK only including hauls in the EBS Standard Region
production_alk <- 
  subset(x = gapindex::calc_alk(
    racebase_tables = production_data,
    unsex = c("all", "unsex")[1], 
    global = FALSE),
    subset = AGE_FRAC > 0)

## Calculate age composition by stratum
production_agecomp_stratum <- 
  gapindex::calc_agecomp_stratum(
    racebase_tables = production_data,
    alk = production_alk,
    size_comp = production_sizecomp_stratum)

# write.csv(production_agecomp_stratum$length_at_age, 
#           file = here('2024','data','raw','production_agecomp_stratum_laa.csv'),
#           row.names = FALSE)
## Aggregate `production_agecomp_stratum` to subareas and regions
production_agecomp_region <- 
  gapindex::calc_agecomp_region(
    racebase_tables = production_data,
    age_comps_stratum = production_agecomp_stratum)

# Change "STRATUM" field name to "AREA_ID"
names(x = production_agecomp_stratum$age_comp)[
  names(x = production_agecomp_stratum$age_comp) == "STRATUM"] <- "AREA_ID"

production_agecomp <- 
  rbind(production_agecomp_region,
        production_agecomp_stratum$age_comp[, names(x = production_agecomp_region)]) %>%
  left_join(., production_data$specimen, by = c('YEAR','SEX','AGE','LENGTH','CRUISE'))

write.csv(production_agecomp, file =here('2024','data','raw','production_agecomp.csv'), row.names = FALSE)

## Convert CRUISE to YEAR
production_data$specimen$YEAR <- floor(x = production_data$specimen$CRUISE / 100)
production_data$size$YEAR <- floor(x = production_data$size$CRUISE / 100)
production_data$haul
## Calculate unique number of hauls with otolith and age data for each year (these are for CAALs) 
nsamp_age <- aggregate(HAULJOIN ~ YEAR,
          data = production_data$specimen,
          FUN = function(x) length(x = unique(x = x)))

production_data$size <- merge(production_data$size, production_data$haul[,c('HAULJOIN','CRUISE')], by = 'HAULJOIN') %>%
  mutate(YEAR = floor(x = CRUISE / 100))

nsamp_len <- aggregate(HAULJOIN ~ YEAR,
                       data = production_data$size,
                       FUN = function(x) length(x = unique(x = x)))
## marginal lengths ----




production_sizecomp_stratum %>%
  filter(!is.na(LENGTH_MM) & SEX != 3) %>%
  mutate(
    length_grp = cut(round(LENGTH_MM/10,0) ,
                     breaks = seq(6,58,2),
                     right = FALSE,
                     labels = FALSE),
    length_bin_use = seq(6,58,2)[length_grp]) %>%
  group_by(YEAR, SEX, length_bin_use) %>%
  summarise(Num_Fish = n(), .groups='drop') %>%
  mutate(tot = sum(Num_Fish), .by = c('YEAR','SEX')) %>%
  mutate(freq = Num_Fish/tot) %>%
  select(-Num_Fish, -tot) %>%
  tidytable::left_join(expand.grid(SEX = unique(.$SEX),
                                   YEAR = unique(.$YEAR),
                                   length_bin_use = lengths), .) %>%
  tidytable::replace_na(list(freq = 0)) %>%
  # tidytable::mutate(AA_Index = 1, n_s = mean(n_s, na.rm = T),
  #                   n_h = mean(n_h, na.rm = T), .by = year) %>%
  # tidytable::select(-age_tot) %>%
  tidytable::pivot_wider(names_from = length_bin_use, values_from = freq, values_fill = 0) %>%
  left_join(., nsamp_len, by = 'YEAR') %>%
    mutate(Yr = YEAR, Seas = 7, Fleet=2,
           Sex = SEX, Part = 0, 
           Nsamp = HAULJOIN) %>%
    select(Yr, Seas, Fleet, Sex, Part,  Nsamp, 5:58) %>%
    bind_cols(., select(., 10:30)) %>%
  filter(!is.na(Lbin_lo)) %>%
  arrange(Yr, Sex, Lbin_lo) %>% View()

## CAALS----
caal <- production_agecomp %>% 
  select(-SURVEY, - SURVEY_DEFINITION_ID, -AREA_ID, -POPULATION_COUNT ) %>%
  filter(!is.na(AGE) & AGE>0 & !is.na(LENGTH_MM_MEAN) & SEX!=3) %>%
  mutate(AGE = ifelse(AGE > plus_age,plus_age,AGE)) %>%
  mutate(
    length_grp = cut(round(LENGTH_MM_MEAN/10,0) ,
                     breaks = seq(6,58,2),
                     right = FALSE,
                     labels = FALSE),
    length_bin_use = seq(6,58,2)[length_grp]) %>%
  group_by(YEAR, SEX, AGE, length_bin_use) %>%
  summarise(Num_Fish = n(), .groups='drop') %>%
  # summarize(Num_Fish=length(SPECIMENID), .groups='drop') %>%
  ## arange by AGE so bins are in right order below
  # rename(AGE=aBIN) %>% 
  arrange(AGE) %>%
  group_by(SEX, YEAR, length_bin_use) %>%
  mutate(Nsamp=sum(Num_Fish)) %>%
  tidyr::pivot_wider(names_from=AGE, values_from=Num_Fish,
              names_prefix='a', values_fill=0)

SS_caal_survey <-
  data.frame(year=caal$YEAR, Month=7, Fleet=2,
             sex=ifelse(caal$SEX==1,2,1), ## SS sex is reversed
             Part=0, Ageerr=1,
             Lbin_lo=caal$length_bin_use, Lbin_hi=caal$length_bin_use,
             Nsamp=caal$Nsamp,
             ## Double up b/c SS needs dummy columns
             caal[,-(1:4)], caal[,-(1:4)]) %>%
  arrange(sex, year, Lbin_lo)

write.csv(SS_caal_survey, file = here::here(year,'data','output','srv_caal_ss3-gapindex.csv'), row.names = FALSE)
message("Saved survey CAAL data in ", here::here(year,'data','output','srv_caal_ss3-gapindex.csv'))
# 
# production_agecomp %>% 
#   select(-SURVEY, - SURVEY_DEFINITION_ID, -AREA_ID, -POPULATION_COUNT ) %>%
#   filter(AGE > 0 & !is.na(LENGTH_MM_MEAN) & SEX != 3) %>%
#   mutate(
#     length_grp = cut(round(LENGTH_MM_MEAN/10,0) ,
#                      breaks = seq(10,52,2),
#                      right = FALSE,
#                      labels = FALSE),
#     length_bin_use = seq(10,52,2)[length_grp]) %>%
#   tidytable::mutate(tot = tidytable::n(),   .by = c(YEAR,SEX,length_bin_use)) %>% 
#   # tidytable::mutate(age_tot = tidytable::n(),   .by = c(YEAR,SEX,AGE)) %>%
#   # tidytable::mutate(prop = age_tot/tot) %>% 
#   # tidytable::filter(tot > 49) %>% 
#   # tidytable::mutate(n_h = length(unique(na.omit(haul_join))) + length(unique(na.omit(port_join))), .by = YEAR) %>% 
#   tidytable::summarise(tot = mean(tot),
#                        # n_h = mean(n_h),
#                        age_tot = tidytable::n(), .by = c(YEAR,SEX,AGE,length_bin_use)) %>%
#   # tidytable::mutate(prop = age_tot/tot) %>%
#   tidytable::left_join(expand.grid(SEX = unique(.$SEX), 
#                                    YEAR = unique(.$YEAR),    
#                                    AGE = 1:plus_age), .) %>% 
#   tidytable::replace_na(list(age_tot = 0)) %>% 
#   # tidytable::mutate(AA_Index = 1, n_s = mean(n_s, na.rm = T), 
#   #                   n_h = mean(n_h, na.rm = T), .by = year) %>% 
#   # tidytable::select(-age_tot) %>% 
#   tidytable::pivot_wider(names_from = AGE, values_from = age_tot, values_fill = 0) %>%
#   left_join(., nsamp_age, by = 'YEAR') %>%
#     mutate(Yr = YEAR, Seas = 7, Fleet=2,
#            Sex = SEX, Part = 0, Ageerr = 1,
#            Lbin_lo = length_bin_use, Lbin_hi = length_bin_use,
#            Nsamp = tot) %>%
#     select(Yr, Seas, Fleet, Sex, Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp, 5:25) %>%
#     bind_cols(., select(., 10:30)) %>%
#   filter(!is.na(Lbin_lo)) %>%
#   arrange(Yr, Sex, Lbin_lo) %>% View()
#     
#  
#     data.frame(year=caal$YEAR, Month=7, Fleet=2,
#                sex=ifelse(caal$SEX==1,2,1), ## SS sex is reversed
#                Part=0, Ageerr=1,
#                Lbin_lo=caal$BIN, Lbin_hi=caal$BIN,
#                Nsamp=caal$Nsamp,
#                ## Double up b/c SS needs dummy columns
#                caal[,-(1:4)], caal[,-(1:4)]) %>%
#     arrange(sex, year, Lbin_lo)
# 
# fac0 %>% filter(sex == 'F') %>% 
#   merge(., fac0 %>% 
#           filter(sex == 'M') %>% 
#           select(-sex, -n_s,-n_h, - AA_Index), 
#         by = 'year', all.y = FALSE) %>%
#   arrange(year) %>%
#   ## drop the years before 2000 since Lcomps are available
#   mutate(Seas = 7, FltSvy = ifelse(year < 2000, -1, 1), Gender = 3, 
#          Part = 0, Ageerr = 1, LbinLo = -1, LbinHi = -1, Nsamp = n_h) %>%
#   select(Yr = year, Seas, FltSvy, Gender, Part, Ageerr, LbinLo, LbinHi, Nsamp, everything(), -sex, -n_s, -n_h, -AA_Index) %>%
#   write.csv(., file = here::here(year,'data','output','fsh_age_comp_ss3.csv'), row.names = FALSE)
# 
#   merge(., nsamp_age, by = 'YEAR')
# 
# production_data$specimen %>% filter(SPECIES_CODE == 10130 & !is.na(AGE) & !is.na(LENGTH) & SEX !=3) %>%
#   group_by(YEAR) %>%
#   summarise(n_h = length(unique(HAUL)))




## For the SPECIES_CODE values in spp_start_year, constrain the agecomp tables 
## to only the years when we feel confident about their taxonomic accuracy, 
## e.g., remove northern rock sole values prior to 1996.
for (irow in 1:nrow(x = spp_start_year)) { ## Loop over species -- start
  production_agecomp <- subset(
    x = production_agecomp,
    subset = !(SPECIES_CODE == spp_start_year$SPECIES_CODE[irow] & 
                 YEAR < spp_start_year$YEAR_STARTED[irow]))
} ## Loop over species -- end