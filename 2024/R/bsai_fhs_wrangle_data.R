## Format BSAI FHS data for SS3
## This script is meant to be source()d from 2024_analysis.R
## It does involve a live call to the gapindex package, which requires a connection to the AFSC Oracle database.
## Otherwise, it assumes you have already run afscdata::bsai_fhs() and saved the fishery catch & comp data to the data/raw folder.
## The script will output the formatted data to the data/output folder.
## maia.kapur@noaa.gov
## Jan 2024

## setup ----
require(dplyr)
require(gapindex)

## connect for gapindex
message('enter your AFSC credentials')
sql_channel <- gapindex::get_connected()

# Reformat Fishery Data (nothing new to dwnld)
## Fishery Catches ----
# (note: this automates the in-year estimation)
## output/yld_ratio.csv has the expansion factor ($ratio) and the 3-year catch/TAC ratio ($yld)
## which are used for in-year and next-two-year catches, respectively
suppressWarnings(afscassess::clean_catch(year = year, 
                                         species = species, 
                                         TAC = TAC))

## reformat catches
afscdata::catch_to_ss3(year, seas = 1, fleet = 1, yld_rat = F) # yld_rat should be FALSE

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
  write.csv(., file = here::here(year,'data','output','fsh_age_ss3.csv'), row.names = FALSE)

message('reformatted and saved fishery age comp data to output/')


#Fishery Lengths ----
## downloads and formats everything
source(here::here(year,'r','old_fishery_lcomp_routine.R'))

message('reformatted and saved fishery length comp data to output/')

# Survey Data----

## Download Survey Data from gapindex ----
## Note: this assessment uses slightly different data sources for the composition vs biomass data.
## The survey biomass data includes Bering Flounder and FHS as a complex from EBS Standard ("Bering Sea Shelf") and the NW,
## combined in a linear model with the AI values. The AI is technically flathead only even though the species complex was originally queried.
## whereas the comps (CAAL/lengths) are flathead sole only, from the standard area only.
## That is why there are three steps under "Initial download": two to get the survey biomass data (AI and EBS), and one for the compositions.
## In the future we might be able to get everything via a direct SQL call, but we're using
## the R package this year because the vignette is the only place you can get age comp values
## conditioned on the ALK from JUST the EBS -- what's on Oracle includes the NW region.

### Initial download ----
#### survey biomass download, ebs ----
production_data_forbio <- gapindex::get_data(
  year_set = 1982:2024,
  survey_set = "EBS",
  spp_codes = data.frame(GROUP = 10129, SPECIES_CODE = 10130:10140),
  pull_lengths = FALSE,
  haul_type = 3,
  abundance_haul = "Y",
  sql_channel = sql_channel)

production_cpue <- gapindex::calc_cpue(racebase_tables = production_data_forbio)
production_biomass_stratum <-
  gapindex::calc_biomass_stratum(racebase_tables = production_data_forbio,
                                 cpue = production_cpue)

# Aggregate Biomass to subareas and region
production_biomass_subarea <-
  gapindex::calc_biomass_subarea(racebase_tables = production_data_forbio,
                                 biomass_strata = production_biomass_stratum)

production_biomass_subarea_standard <- subset(x = production_biomass_subarea,
                                              subset = AREA_ID == 99901, ## 99900 == EBS Standard + NW region, AREA_ID == 99901 for EBS Standard Region
                                              select = c(YEAR, BIOMASS_MT, BIOMASS_VAR)) %>%
  mutate(SURVEY = 'EBS')



write.csv(production_biomass_subarea_standard, 
          file = here::here(year, 'data','raw','gapindex_survey_biomass_ebs_standard.csv'),
          row.names = FALSE)

message('saved ebs survey biomass data to raw/')

#### survey biomass download, ai ----
production_data_forbio_ai <- gapindex::get_data(
  year_set = 1982:2024,
  survey_set = "AI", 
  spp_codes = 10130, ## FHS only from AI (you can query the complex but nothing is returned)
  pull_lengths = FALSE,
  haul_type = 3,
  abundance_haul = "Y",
  sql_channel = sql_channel)

production_cpue_ai <- gapindex::calc_cpue(racebase_tables = production_data_forbio_ai)

production_biomass_stratum_ai <-
  gapindex::calc_biomass_stratum(racebase_tables = production_data_forbio_ai,
                                 cpue = production_cpue_ai)

# Aggregate Biomass to subareas and region
production_biomass_subarea <-
  gapindex::calc_biomass_subarea(racebase_tables = production_data_forbio_ai,
                                 biomass_strata = production_biomass_stratum_ai)  

production_biomass_subarea_ai <- subset(x = production_biomass_subarea,
                                        subset = AREA_ID == 99904, ## AI
                                        select = c(YEAR, BIOMASS_MT, BIOMASS_VAR)) %>%
  mutate(SURVEY = 'AI')



write.csv(production_biomass_subarea_ai, 
          file = here::here(year, 'data','raw','gapindex_survey_biomass_ai.csv'),
          row.names = FALSE)
message('saved ai survey biomass data to raw/')

#### survey comps download ----
production_data <- gapindex::get_data(
  year_set = 1982:2024,
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
## these are not in the standard pull to begin with
production_data$subarea <- subset(x = production_data$subarea, 
                                  subset = !(AREA_ID %in% c(7, 8, 9, 100, 200, 300, 99900)))
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
                            production_agecomp_stratum$age_comp[, names(x = production_agecomp_region)]) #%>%
# left_join(., production_data$specimen, by = c('YEAR','SEX','AGE','LENGTH','CRUISE'))


## Convert CRUISE to YEAR
production_data$specimen$YEAR <- floor(x = production_data$specimen$CRUISE / 100)
production_data$size$YEAR <- floor(x = production_data$size$CRUISE / 100)
production_data$size <- merge(production_data$size, production_data$haul[,c('HAULJOIN','CRUISE')], 
                              by = 'HAULJOIN') %>%
  dplyr::mutate(YEAR = floor(x = CRUISE / 100))

## save these 
write.csv( production_data$size, file = here::here(year, 'data','raw','production_data_size.csv'), row.names = FALSE)
write.csv( production_data$specimen, file = here::here(year, 'data','raw','production_data_specimen.csv'), row.names = FALSE)
message('saved raw production_data compositions to raw/')

## Construct Survey Biomass Index ----
### Linear Model ---- 

index_raw <- rbind(production_biomass_subarea_standard, 
                   production_biomass_subarea_ai) %>%
  tidyr::pivot_wider(names_from=SURVEY, 
                     values_from=c(BIOMASS_MT, BIOMASS_VAR)) %>%
  mutate(sd_EBS=sqrt(BIOMASS_VAR_EBS), 
         sd_AI=sqrt(BIOMASS_VAR_AI)) %>%
  select(year=YEAR, biomass_EBS = BIOMASS_MT_EBS, biomass_AI = BIOMASS_MT_AI, sd_EBS, sd_AI)

## Do a linear regression to get missing AI years
## note that we only interpolate in years with at least one survey,
## thus 2020 will be empty no matter what.
z1 <- subset(index_raw, !is.na(biomass_AI))
z2 <- subset(index_raw, is.na(biomass_AI))
lmbio <- lm(biomass_AI~biomass_EBS, data=z1)
z2$biomass_AI <- as.numeric(predict(lmbio, newdata=z2))
lmvar <- lm(sd_AI~sd_EBS, data=z1)
z2$sd_AI <- as.numeric(predict(lmvar, newdata=z2))
## Recombine and add together biomass and variances
index <- rbind(z1,z2) %>% group_by(year) %>%
  summarize(biomass=round(biomass_AI+biomass_EBS,5),
            variance=sd_AI^2+sd_EBS^2,
            .groups='drop') %>%
  ## SE on log scale, which SS requires, is sqrt(log(1+CV^2))
  mutate(se_log=round(sqrt(log(1+variance/biomass^2)),5)) %>%
  select(-variance)

### SS3 format ----
SS_index <- data.frame(year=index$year, 
                       seas=7, index=2, 
                       obs= round(index$biomass), se_log=index$se_log)
write.csv(x=SS_index, file=here::here(year,'data','output','srv_bio_ss3.csv'), row.names=FALSE)


## Reformat Survey CAALs ----
## Calculate unique number of hauls with otolith and age data for each year  
nsamp_age <- aggregate(HAULJOIN ~ YEAR,
                       data = production_data$specimen,
                       FUN = function(x) length(x = unique(x = x)))
write.csv(nsamp_age, file = here::here(year, 'data','raw','nsamp_age.csv'), row.names = FALSE)
 

## The length bins need to match the population length bins
# lbins <- c(seq(from = 6, to = 40, by = 2),seq(from = 43, to = 58, by = 3))
lbins <- seq(6,58,2)
caal00 <-   production_data$specimen %>%
  ## filter out bering flounder and unsexed
  filter(!is.na(AGE) & !is.na(LENGTH) & SEX != 3 & AGE > 0 & SPECIES_CODE == 10130 ) %>%
  ## deal with plus groups
  mutate(YEAR =  floor(x = CRUISE / 100),
         SEX = ifelse(SEX == 1, 'males','females'), 
         AGE = ifelse(AGE  >= 21, 21, AGE ),
         LENGTH_MM  = ifelse(LENGTH   < 60, 60.5,LENGTH ),
         LENGTH_MM = ifelse(LENGTH   >= 580, 580, LENGTH ))  %>%
  mutate( length_grp0 = cut(round(LENGTH_MM /10,0),
                            right = F,
                            breaks =  lbins))  %>%
  ## make integer-based length bin
  tidyr::separate(length_grp0, c("first", "second"), sep = ",") %>% 
  mutate(LENGTH_BIN = as.numeric(substr(first,2,nchar(first)))) %>%
  group_by(YEAR, SEX, AGE, LENGTH_BIN) %>%
  ## calculate number of samples in each age-length bin (Obs)
  summarize(Num_Fish=length(SPECIMENID), .groups='drop') %>%
  arrange(AGE, SEX) %>%
  ## calculate the number of samples in each length bin (inputN)
  group_by(SEX, YEAR, LENGTH_BIN) %>%
  mutate(value=sum(Num_Fish)) 

## reshape to ss3 format
caal0 <- caal00 %>% ## raw number of individuals
  tidyr::pivot_wider(names_from=AGE, values_from=Num_Fish,
                     names_prefix='a', values_fill=0) %>%
  arrange(YEAR, SEX, LENGTH_BIN)

bind_cols(caal0,caal0[,-(1:4)]) %>%
  mutate(Seas = 7, Fleet = 2, SEX = ifelse(SEX == 'males',2,1),
         Part = 0, Ageerr = 1) %>%  
  select(Yr = YEAR, Seas, Fleet, Sex =SEX, Part, Ageerr, Lbin_lo = LENGTH_BIN,
         Lbin_hi = LENGTH_BIN, Nsamp=value,everything()) %>%
  write.csv(., file = here::here(year,'data','output','srv_caal_ss3-gapindex.csv'), 
            row.names = FALSE)

message('saved survey CAAL data to output/')

## Reformat Survey Lengths ----
nsamp_len <- aggregate(HAULJOIN ~ YEAR,
                       data = production_data$size,
                       FUN = function(x) length(x = unique(x = x)))

write.csv(nsamp_len, file = here::here(year, 'data','raw','nsamp_len.csv'), row.names = FALSE)
 
survlen <- production_sizecomp_stratum %>% 
  filter(SEX != 3) %>%
  mutate(SEX = ifelse(SEX == 1, 'males','females'),
         LENGTH_MM  = ifelse(LENGTH_MM < 60, 60.5,LENGTH_MM),
         LENGTH_MM = ifelse(LENGTH_MM >= 580, 580, LENGTH_MM))  %>%
  mutate( length_grp0 = cut(round(LENGTH_MM/10,0),
                            right = F,
                            breaks =  c(seq(6,40,2),seq(43,61,3))))  %>%
  tidyr::separate(length_grp0, c("first", "second"), sep = ",") %>% 
  mutate(LENGTH_BIN = as.numeric(substr(first,2,nchar(first)))) %>%
  select(-first, -second) %>%
  group_by(YEAR, LENGTH_BIN, SEX) %>%
  summarise(value = sum(POPULATION_COUNT))   %>%  
  ungroup()%>%
  mutate(tot = sum(value), .by = c(YEAR)) %>%
  mutate(freq = value/tot) 
srvlen0 <- survlen %>%
  tidyr::pivot_wider(names_from = LENGTH_BIN, 
                     values_from = freq, 
                     id_cols = c(YEAR, SEX), values_fill = 0) %>%
  mutate(SEX = ifelse(SEX == 'males',2,1)) %>%
  merge(., nsamp_len, by = 'YEAR')
srvlen_save <- srvlen0 %>% 
  filter(SEX == 1) %>% 
  merge(., srvlen0 %>% 
          filter(SEX == 2) %>% 
          select(-SEX, -HAULJOIN), 
        by = c('YEAR'), all.y = FALSE) %>%
  mutate(Seas = 7, FltSvy = 2, Gender = 3, Part = 0,) %>%
  select(Yr = YEAR, Seas, FltSvy, Gender, Part, Nsamp=HAULJOIN, everything(),
         -SEX) %>%
  arrange(Yr) #%>%
  
## sanity check that the order is right (females then males)
## compare to raw-ish production data
survlen %>% filter(SEX == 'females' & YEAR == 1984 & LENGTH_BIN ==8)
survlen %>% filter(SEX == 'males' & YEAR == 1984 & LENGTH_BIN ==8)
## compare to wrangled file
(srvlen0 %>% filter(SEX==1 & YEAR == 1984))[,'8'] ## bin 8 females
(srvlen0 %>% filter(SEX==2 & YEAR == 1984))[,'8'] ## bin 8 males
(srvlen_save %>% filter(Yr == 1984))[,c('8.x','8.y')]

write.csv(srvlen_save, 
          file = here::here(year,'data','output','srv_len_ss3-gapindex.csv'), 
            row.names = FALSE)


message('saved survey length comp data to output/')

## Reformat Survey Ages (ghost) ----

survage <- production_agecomp %>% 
  filter(SEX != 3 & AGE > 0) %>%
  mutate(SEX = ifelse(SEX == 1, 'males','females'), 
         AGE = ifelse(AGE  >= 21, 21, AGE ),
         LENGTH_MM  = ifelse(LENGTH_MM_MEAN  < 60, 60.5,LENGTH_MM_MEAN ),
         LENGTH_MM = ifelse(LENGTH_MM_MEAN  >= 580, 580, LENGTH_MM_MEAN ))  %>%
  mutate( length_grp0 = cut(round(LENGTH_MM_MEAN /10,0),
                            right = F,
                            breaks =  c(seq(6,40,2),seq(43,61,3))))  %>%
  tidyr::separate(length_grp0, c("first", "second"), sep = ",") %>% 
  mutate(LENGTH_BIN = as.numeric(substr(first,2,nchar(first)))) %>%
  select(-first, -second) %>% 
  summarise(value = sum(POPULATION_COUNT), .by = c(YEAR,AGE,SEX))   %>%  
  mutate(tot = sum(value), .by = c(YEAR)) %>%
  mutate(freq = value/tot) 

srvage0 <- survage %>%
  mutate(AGE = as.numeric(AGE)) %>%
  tidyr::pivot_wider(names_from = AGE, values_from = freq, id_cols = c(YEAR, SEX), values_fill = 0) %>%
  mutate(SEX = ifelse(SEX == 'males',2,1)) %>%
  merge(., nsamp_age, by = 'YEAR') %>%
  select(YEAR, SEX, HAULJOIN, paste0(1:21)) ## ensure correct order


srvage0 %>% 
  filter(SEX == 1) %>% 
  merge(., srvage0 %>% 
          filter(SEX == 2) %>% 
          select(-SEX, -HAULJOIN), 
        by = c('YEAR'), all.y = FALSE) %>%
  mutate(Seas = 7, FltSvy = -2, Gender = 3, Part = 0,Ageerr = 1, Lbin_lo = -1, Lbin_hi = -1) %>%
  select(Yr = YEAR, Seas, FltSvy, Gender, Part, Ageerr, Lbin_lo = Lbin_lo, Lbin_hi = Lbin_hi, Nsamp=HAULJOIN, everything(),
         -SEX) %>%
  arrange(Yr) %>%
  write.csv(., file = here::here(year,'data','output','srv_age_ss3-gapindex-ghost.csv'), 
            row.names = FALSE)

message('saved survey marginal ages (ghosted) to output/')




