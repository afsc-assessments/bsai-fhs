## Survey CAAL/Comp Workup
## Using the afscdata package + my intuition resulted in very different CAALs than 
## the pre-existing model. This adapts a script passed from CCM called process_agecomps_survey
## to see if starting from the raw-ish data pull we can reproduce the dataset.
## The key challenges is that the original script started from "AL.df", which was a csv
## entitled ages_survey_ebs.csv, which itself was pulled from the haehnr schema...which is now deprecated.

## Code here comes from the 2020 assessment folder, variously from inputs.R and process_agecoms_survey.R

## setup ----
require(here)
require(dplyr)
require(reshape2)
require(tidyr)

year = 2024
rec_age = 0 ## this is default for SS3
plus_age = 21
lengths = c(seq(6,40,2),seq(43,58,3))
ages = age_bins <- rec_age:plus_age
caal_len_bins <- seq(6, 58, by=2)
## source functions ----
## these are mainly from newsbss
source("C:/Users/maia.kapur/Work/assessments/newsbss/functions/BIN_AGE_DATA.R")
source("C:/Users/maia.kapur/Work/assessments/newsbss/functions/BIN_LEN_DATA.R")

## Survey CAALs----
message("Processing survey CAAL data...")
AL.df <- read.csv(here::here(year, 'data','raw','bts_specimen_data.csv')) ## result of afscdata::q_bts_specimen
names(AL.df) <- toupper(names(AL.df)) ## in the original script they are uppercase

## filtration steps (clunky)
# AL.df <- AL.df[AL.df$SUBAREA <=6,]
AL.df<-AL.df[AL.df$SPECIES_CODE==species1,]

## Bin data into length and age bins. There is an age of 0 that
## results in NA bin so drop it, also drop unsexed fish
AL.df <- filter(AL.df, !is.na(AGE) & AGE>0 & !is.na(LENGTH) & SEX!=3)
AL.df <- AL.df[AL.df$AGE>0 & !is.na(AL.df$AGE) & !is.na(AL.df$LENGTH),]
AL.df <- BIN_AGE_DATA(AL.df,age_bins)
AL.df$LENGTH <- round(AL.df$LENGTH/10,0) # mm to cm
## Note that teh caal length bins are not the same as those used
## in the length compositions
AL.df <- BIN_LEN_DATA(AL.df,len_bins=caal_len_bins)
AL.df$age_bin <- AL.df$aBIN

## Calculate conditional age-at-length separately by sex
caal <- AL.df %>%
  group_by(YEAR, SEX, aBIN, BIN) %>%
  summarize(Num_Fish=length(SPECIMENID), .groups='drop') %>%
  ## arange by AGE so bins are in right order below
  rename(AGE=aBIN) %>% arrange(AGE) %>%
  group_by(SEX, YEAR, BIN) %>%
  mutate(Nsamp=sum(Num_Fish)) %>%
  pivot_wider(names_from=AGE, values_from=Num_Fish,
              names_prefix='a', values_fill=0)
## Just being careful the columns are in right order
stopifnot(all(names(caal)[-(1:4)] == paste0('a', age_bins[2:22])))

## Build SS structures
SS_caal_survey <-
  data.frame(year=caal$YEAR, Month=7, Fleet=2,
             sex=ifelse(caal$SEX==1,2,1), ## SS sex is reversed
             Part=0, Ageerr=1,
             Lbin_lo=caal$BIN, Lbin_hi=caal$BIN,
             Nsamp=caal$Nsamp,
             ## Double up b/c SS needs dummy columns
             caal[,-(1:4)], caal[,-(1:4)]) %>%
  arrange(sex, year, Lbin_lo)
names(SS_caal_survey) <- names(SS_dat$agecomp)

write.csv(SS_caal_survey, file = here::here(year,'data','output','srv_caal_ss3.csv'), row.names = FALSE)
message("Saved survey CAAL data in ", here::here(year,'data','output','srv_caal_ss3.csv'))
# stopifnot(ncol(SS_caal_survey)==9+length(age_bins)*2)


## Survey length comps ----
message("Processing survey length data...")
species <- "10130" #flathead sole only, no BF
tmp <-  read.csv(here::here(year,'data','raw','bsai_ts_length_data.csv')) 
names(tmp) <- toupper(names(tmp))
tmp <- tmp %>%
  filter(SPECIES_CODE %in% afsc_species & 
           SEX != 3 &
           REGION == 'BS' &
           YEAR > 1980 &
           # grepl('Eastern Bering Sea', SURVEY_NAME),
           # STRATUM==999999 & 
           LENGTH>0) %>%
  # mm to cm
  droplevels() %>% 
  mutate(LENGTH=LENGTH/10)

lcomp_raw <- tmp %>% 
  group_by(YEAR, LENGTH, SEX) %>%
  summarise(FREQUENCY=sum(FREQUENCY)) %>%
  tidyr::pivot_wider(names_from = SEX, values_from = FREQUENCY, id_cols = c(YEAR, LENGTH), values_fill = 0)
names(lcomp_raw)[3:4] <- c('FEMALES','MALES')


lcomp <- BIN_LEN_DATA(data=lcomp_raw, len_bins=len_bins) %>%
  select(year=YEAR, males=MALES, females=FEMALES, bin=BIN) %>%
  ## collapse different lengths into bins by year
  group_by(year, bin) %>%
  summarize(males=sum(males), females=sum(females), .groups='drop') %>%
  ## don't arrange by year first or it breaks order of wide
  ## columns below, b/c first year is missing some bins
  arrange(bin, year)
## Calculate annual totals, turn into proportions, and format for SS
lcomp <- group_by(lcomp, year) %>%
  mutate(M_PROP=males/sum(males+females),
         F_PROP=females/sum(males+females)) %>% ungroup() %>%
  select(-males, -females)
lcomp_fem <- select(lcomp, -M_PROP) %>%
  pivot_wider(names_from=bin, names_prefix='f',
              values_from='F_PROP', values_fill=0) %>%
  arrange(year)
lcomp_male <- select(lcomp, -F_PROP) %>%
  pivot_wider(names_from=bin, names_prefix='m',
              values_from='M_PROP', values_fill=0) %>%
  arrange(year)
stopifnot(all.equal(lcomp_fem$year, lcomp_male$year))
## The sample sizes come from the number of hauls with lengths
## which is in this file.
# lcomp_Nsamp <- read.csv('data/lengths_survey_hauls.csv')
lcomp_Nsamp <- tmp %>%
  group_by(YEAR) %>%
  summarise(HAULS_W_LENGTH=length(unique(HAULJOIN)))


stopifnot(all(lcomp_Nsamp$YEAR == lcomp_fem$year))
stopifnot(all(!is.na(lcomp_Nsamp$HAULS_W_LENGTH)))
SS_lcomp_survey <- data.frame(year=lcomp_fem$year, 
                              month=7,
                              fleet=2, sex=3, part=0, 
                              Nsamp=lcomp_Nsamp$HAULS_W_LENGTH,
                              lcomp_fem[,-1], lcomp_male[,-1])
# names(SS_lcomp_survey)[1:6] <- names(SS_dat$lencomp)[1:6]
stopifnot(all(1==rowSums(SS_lcomp_survey[,-(1:6)])))
write.csv(SS_lcomp_survey, here::here(year,'data','output','srv_lengths_ss3.csv'), row.names=FALSE)
message("Saved survey CAAL data in ", here::here(year,'data','output','srv_lengths_ss3.csv'))


## MK's attempt -----
# This version was based on me trying to calculate things from scratch. But the results are very different from the above.
# ## munge into correct bins, calculate freq(age|len)
# 
# tmp <- read.csv(here::here(year, 'data','raw','bsai_ts_length_specimen_data.csv'))  %>%
#   filter(sex != 3 & !is.na(age) & !is.na(length) & age > 0 & age<plus_age, species_code == 10130) %>%
#   mutate(
#     # sex = ifelse(sex == 1,2,1),  ## SS sex is inverse of this dataset
#     length_grp = cut(round(length/10,0) , 
#                      breaks = seq(0,60,2), 
#                      right = FALSE,
#                      labels = FALSE),
#     length_bin_use = seq(0,60,2)[length_grp]) 
# 
# tmp$length_bin_use[tmp$length_bin_use<=10] <- 10 ## avoid NAs
# tmp$length_bin_use[tmp$length_bin_use>=52] <- 52 ## avoid NAs
# 
# 
# inputN_total <- tmp %>% 
#   group_by(year,sex) %>%
#   dplyr::summarise(Nsamp = n())
# 
# 
# inputN_length <- tmp %>% 
#   group_by(year,sex,length_bin_use) %>%
#   dplyr::summarise(Nsamp = n())
# 
# caal0 <- tmp %>% 
#   group_by(year, sex, age, length_bin_use) %>%
#   summarise(n_combo = n()) %>%
#   ungroup()%>%
#   left_join(., inputN_length, by = c('year','sex','length_bin_use')) %>%
#   tidytable::left_join(expand.grid(sex = unique(.$sex), 
#                                    year = unique(.$year),    
#                                    age = 1:plus_age), .) %>%
#   
#   # mutate(freq = n_combo/Nsamp) %>%
#   tidyr::pivot_wider(names_from = age, values_from = n_combo, values_fill = 0, names_expand = TRUE)
# 
# 
# bind_cols(caal0, caal0[,5:ncol(caal0)]) %>%
#   filter(year %in%  unique(mod_2020$condbase$Yr) | year > 2019) %>%
#   filter(!is.na(length_bin_use)) %>%
#   arrange(year,sex) %>%
#   mutate(Seas = 7, FltSvy = 2,Gender = sex, Part = 0, Ageerr = 1, Lbin_lo = length_bin_use,
#          Lbin_hi = length_bin_use) %>%
#   select(year, Seas, FltSvy, Gender,Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp, everything(),
#          -length_bin_use,-sex) %>%
#   write.csv(., file = here::here(year,'data','output','srv_caal_ss3.csv'), row.names = FALSE)
# 

