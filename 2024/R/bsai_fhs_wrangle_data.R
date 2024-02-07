## bsai_fishery_data_to_ss3
## maia.kapur@noaa.gov
## Jan 2024

require(dplyr)
require(gapindex)

## connect for gapindex
sql_channel <- gapindex::get_connected()

# Reformat Fishery Data (nothing new to dwnld)
## Fishery Catches ----
# (note: this automates the in-year estimation)
## output/yld_ratio.csv has the expansion factor ($ratio) and the 3-year catch/TAC ratio ($yld)
## which are used for in-year and next-two-year catches, respectively
suppressWarnings(afscassess::clean_catch(year = year, 
                                         species = species, 
                                         TAC = TAC,
                                         fixed_catch = 'bsai_fhs_catch_1964_1994.csv'))

## reformat catches
afscdata::catch_to_ss3(year, seas = 1, fleet = 1)

message('reformatted and saved fishery catch data to output/')

## Fishery Ages ----
# expand fishery age comps by sex expanded in years with obs catch data); 
## adapted from afscassess::fish_age_comp function
fac0 <- vroom::vroom(here::here(year, "data", "raw", "fsh_specimen_data.txt"), delim = ",", 
                     col_type = c(join_key = "c", 
                                  haul_join = "c", port_join = "c")) %>% 
  tidytable::filter(age >= rec_age,  
                    !is.na(length), 
                    !is.na(sex),
                    sex != 'U',
                    !is.na(performance)) %>% 
  tidytable::mutate(age = ifelse(age > plus_age, plus_age, 
                                 age)) %>% 
  tidytable::mutate(tot = tidytable::n(),   .by = year) %>% 
  # tidytable::filter(tot > 49) %>% 
  tidytable::mutate(n_h = length(unique(na.omit(haul_join))) + length(unique(na.omit(port_join))), .by = year) %>% 
  tidytable::summarise(n_s = mean(tot), 
                       n_h = mean(n_h), 
                       age_tot = tidytable::n(), .by = c(sex,year,age)) %>% 
  tidytable::mutate(prop = age_tot/n_s) %>% 
  tidytable::left_join(expand.grid(sex = unique(.$sex), 
                                   year = unique(.$year),    
                                   age = 1:plus_age), .) %>% 
  tidytable::replace_na(list(prop = 0)) %>% 
  tidytable::mutate(AA_Index = 1, n_s = mean(n_s, na.rm = T), 
                    n_h = mean(n_h, na.rm = T), .by = year) %>% 
  tidytable::select(-age_tot) %>% 
  tidytable::pivot_wider(names_from = age, values_from = prop)

fac0 %>% filter(sex == 'F') %>% 
  merge(., fac0 %>% 
          filter(sex == 'M') %>% 
          select(-sex, -n_s,-n_h, - AA_Index), 
        by = 'year', all.y = FALSE) %>%
  arrange(year) %>%
  ## drop the years before 2000 since Lcomps are available
  mutate(Seas = 7, FltSvy = ifelse(year < 2000, -1, 1), Gender = 3, 
         Part = 0, Ageerr = 1, LbinLo = -1, LbinHi = -1, Nsamp = n_h) %>%
  select(Yr = year, Seas, FltSvy, Gender, Part, Ageerr, LbinLo, LbinHi, Nsamp, everything(), -sex, -n_s, -n_h, -AA_Index) %>%
  write.csv(., file = here::here(year,'data','output','fsh_age_comp_ss3.csv'), row.names = FALSE)

message('reformatted and saved fishery age comp data to output/')


#Fishery Lengths ----

ages <- vroom::vroom(here::here(year, "data", "raw", "fsh_specimen_data.txt"), delim = ",", col_type = c(join_key = "c", 
                                                                                                         haul_join = "c", port_join = "c")) %>% tidytable::filter(!is.na(age), 
                                                                                                                                                                  age >= rec_age) %>% tidytable::group_by(year) %>% tidytable::tally(name = "age") %>% 
  tidytable::filter(age >= 50) %>% 
  tidytable::ungroup()
flc0 <- vroom::vroom(here::here(year, "data", "raw", "fsh_length_data.txt"), 
                     delim = ",", 
                     col_type = c(haul_join = "c", 
                                  port_join = "c")) %>% 
  tidytable::mutate(tot = sum(frequency),                                                                                                                                                                                         length(unique(na.omit(port_join))), .by = year) %>% 
  tidytable::summarise(n_s = mean(tot), n_h = mean(n_h), 
                       length_tot = sum(frequency), .by = c(sex,year, length)) %>% 
  tidytable::mutate(prop = length_tot/n_s) %>% tidytable::left_join(expand.grid(sex = unique(.$sex), 
                                                                                year = unique(.$year), 
                                                                                length = lenbins), .) %>% tidytable::replace_na(list(prop = 0)) %>% 
  tidytable::mutate(SA_Index = 1, n_s = mean(n_s, na.rm = T), 
                    n_h = mean(n_h, na.rm = T), .by = year) %>% tidytable::select(-length_tot) %>% 
  tidytable::pivot_wider(names_from = length, values_from = prop)


flc0 %>% filter(sex == 'F') %>% 
  merge(., flc0 %>% 
          filter(sex == 'M') %>% 
          select(-sex, -n_s,-n_h), 
        by = 'year', all.y = FALSE) %>%
  arrange(year) %>%
  ## drop the years before 2000 since Lcomps are available
  mutate(Seas = 7, 
         FltSvy = ifelse(year %in% unique(fac0$year[which(fac0$year >= 2000)]), -1, 1),  
         Gender = 3, 
         Part = 0, 
         Nsamp = n_h) %>%
  select(Yr = year, Seas, FltSvy, Gender, Part, Ageerr, LbinLo, LbinHi, Nsamp, everything(), -sex, -n_s, -n_h) %>% 
  write.csv(., file = here::here(year,'data','output','fsh_len_comp_ss3.csv'), row.names = FALSE)

message('reformatted and saved fishery length comp data to output/')

# Survey Data
## Survey Biomass: interpolation ----

bts_sparse  <- read.csv(here::here(year,'data','raw','bsai_total_bts_biomass_data.csv'))  %>% 
  dplyr::group_by(year, survey) %>%
  dplyr::summarise(biomass= sum(total_biomass), ## to MT
                   sd = sqrt(sum(biomass_var ))) %>%
  tidyr::pivot_wider(names_from=survey, values_from=c(biomass, sd)) 


z1 <- subset(bts_sparse, !is.na(biomass_AI))
z2 <- subset(bts_sparse, is.na(biomass_AI))
## linear model to interpolate AI biomass & sd in off years
lm_bio <-lm(biomass_AI~biomass_EBS, data = z1)
lm_var <- lm(sd_AI~sd_EBS, data = z1)
## backfill all AI values
z2$biomass_AI <- as.numeric(predict(lm_bio, newdata=z2))
z2$sd <- as.numeric(predict(lm_var, newdata=z2))

## Survey Biomass: reformatting ----
index <- rbind(z1,z2) %>% 
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(biomass=round(biomass_AI+biomass_EBS,5),
                   variance=sum(sd_AI^2,sd_EBS^2,na.rm = TRUE),
                   .groups='drop') %>%
  ## SE on log scale, which SS requires, is sqrt(log(1+CV^2))
  dplyr::mutate(se_log=round(sqrt(log(1+(variance/biomass)^2)),5)) %>%
  dplyr::select(-variance)

SS_index <- data.frame(year=index$YEAR, seas=7, index=2, 
                       obs=round(index$biomass,0), se_log=index$se_log)

write.csv(x=SS_index, file= here(year,'data','output','srv_bio_ss3.csv'),
          row.names=FALSE)

message('reformatted and saved survey biomass data to output/')

## Download Survey Data from gapindex ----
### Initial download ----
production_data <- gapindex::get_data(
  year_set = 1982:2023,
  survey_set = "EBS",
  spp_codes = 10130,
  pull_lengths = TRUE, 
  haul_type = 3, 
  abundance_haul = "Y",
  sql_channel = sql_channel)

## Remove hauls and data associated with hauls in strata 82 and 90 (applicable to EBS only)
ebs_standard_hauls <- with(production_data$haul, HAULJOIN[!(STRATUM %in% c(82, 90))])

production_data$haul <- subset(x = production_data$haul, subset = HAULJOIN %in% ebs_standard_hauls)
production_data$catch <- subset(x = production_data$catch, subset = HAULJOIN %in% ebs_standard_hauls)
production_data$size <- subset(x = production_data$size, subset = HAULJOIN %in% ebs_standard_hauls)
production_data$specimen <-  subset(x = production_data$specimen, subset = HAULJOIN %in% ebs_standard_hauls)
production_data$strata <- subset(x = production_data$strata, subset = !(STRATUM %in% c(82, 90)))

## Remove subareas associated with the EBS + NW region
## these are not in the standard pull to being with
production_data$subarea <- subset(x = production_data$subarea, subset = !(AREA_ID %in% c(7, 8, 9, 100, 200, 300, 99900)))
save(production_data, file = here::here(year, 'data','raw','gapindex_production_data.rdata'))
message('saved cleaned gapindex production_data to raw/')

### Gapindex comp data cleanup ----
## Calculate and zero-fill CPUE
production_cpue <- gapindex::calc_cpue(racebase_tables = production_data)

## Calculate biomass/abundance (w/variance), mean/variance CPUE across strata
production_biomass_stratum <- gapindex::calc_biomass_stratum(racebase_tables = production_data,
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
production_alk <- subset(x = gapindex::calc_alk(
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


## Aggregate `production_agecomp_stratum` to subareas and regions
production_agecomp_region <-  gapindex::calc_agecomp_region(
  racebase_tables = production_data,
  age_comps_stratum = production_agecomp_stratum)

# Change "STRATUM" field name to "AREA_ID"
names(x = production_agecomp_stratum$age_comp)[
  names(x = production_agecomp_stratum$age_comp) == "STRATUM"] <- "AREA_ID"

production_agecomp <- rbind(production_agecomp_region,
                            production_agecomp_stratum$age_comp[, names(x = production_agecomp_region)]) %>%
  left_join(., production_data$specimen, by = c('YEAR','SEX','AGE','LENGTH','CRUISE'))


## Convert CRUISE to YEAR
production_data$specimen$YEAR <- floor(x = production_data$specimen$CRUISE / 100)
production_data$size$YEAR <- floor(x = production_data$size$CRUISE / 100)
production_data$size <- merge(production_data$size, production_data$haul[,c('HAULJOIN','CRUISE')], by = 'HAULJOIN') %>%
  dplyr::mutate(YEAR = floor(x = CRUISE / 100))

## save these 

write.csv( production_data$size, file = here::here(year, 'data','raw','production_data_size.csv'), row.names = FALSE)
write.csv( production_data$specimen, file = here::here(year, 'data','raw','production_data_specimen.csv'), row.names = FALSE)


## Reformat Survey CAALs ----
## Calculate unique number of hauls with otolith and age data for each year  
nsamp_age <- aggregate(HAULJOIN ~ YEAR,
                       data = production_data$specimen,
                       FUN = function(x) length(x = unique(x = x)))
write.csv(nsamp_age, file = here::here(year, 'data','raw','nsamp_age.csv'), row.names = FALSE)

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

## Reformat Survey Lengths ----
nsamp_len <- aggregate(HAULJOIN ~ YEAR,
                       data = production_data$size,
                       FUN = function(x) length(x = unique(x = x)))

write.csv(nsamp_len, file = here::here(year, 'data','raw','nsamp_len.csv'), row.names = FALSE)




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
  arrange(Yr, Sex, Lbin_lo) 

## Reformat Survey Ages (ghost) ----

tmp <- read.csv(here::here(year, 'data','raw','bsai_ts_length_specimen_data.csv'))  %>%
  filter(sex != 3 & !is.na(age) & !is.na(length) & age > 0 & age<plus_age, species_code == 10130) %>%
  mutate(
    # sex = ifelse(sex == 1,2,1),  ## SS sex is inverse of this dataset
    length_grp = cut(round(length/10,0) ,
                     breaks = seq(0,60,2),
                     right = FALSE,
                     labels = FALSE),
    length_bin_use = seq(0,60,2)[length_grp])
inputN_total <- tmp %>%
  group_by(year,sex) %>%
  dplyr::summarise(Nsamp = n())
inputN_h <- tmp %>% 
  group_by(year, sex) %>% 
  summarise(n_h = length(unique(hauljoin)))

srvage0 <- tmp %>% 
  group_by(year, sex, age) %>%
  summarise(n_combo = n()) %>%
  ungroup()%>%
  left_join(., inputN_total, by = c('year','sex')) %>%
  mutate(freq = n_combo/Nsamp) %>%
  select(-n_combo) %>%
  tidyr::pivot_wider(names_from = age, values_from = freq, values_fill = 0, names_expand = TRUE) 

## note: cole had these all set to nsamp = 200
srvage0 %>% 
  filter(sex == 1) %>% 
  merge(., srvage0 %>% 
          filter(sex == 2) %>% 
          select(-sex, -Nsamp), 
        by = c('year'), all.y = FALSE) %>%
  filter(year %in%  unique(mod_2020$condbase$Yr)) %>%
  arrange(year) %>%
  mutate(Seas = 7, FltSvy = -2, Gender = 3, Part = 0, Ageerr = 1, Lbin_lo = -1,
         Lbin_hi = -1) %>%
  select(year, Seas, FltSvy, Gender, Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp, everything(),
         -sex) %>%
  write.csv(., file = here::here(year,'data','output','srv_age_ghost_ss3.csv'), row.names = FALSE)















