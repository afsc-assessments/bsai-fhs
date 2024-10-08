## exploration of new ISS values
## this uses the afscISS package https://afsc-assessments.github.io/afscISS/articles/get_output.html
## According to the Vignette these values could replace what is served by gapindex
## given this is a potentailly large change from the original input going to examine carefully before deciding
## what do do; likely will either not use or wholesale replace (i.e. I won't only update the sample sizes)
# devtools::install_github("BenWilliams-NOAA/surveyISS")
library(surveyISS)
library(dplyr)
## NOTES
## Comps are FHS only and BS  only (AI is only used for biomass) (10130)
## Will only have data thru 2023 for caal and marginal lengths
## recall marginal ages are ghosted
## length comps are female then male, l to rate
## caals are entered as sex specific with dummy values
## ages are 1:21; lengths are in weird bins unclear if we can back them out

## download data  
data <- surveyISS::query_data(survey = 98, ## bs shelf only
                              region = 'ebs',  
                              species = 10130) ## fhs only


save(data, file = here::here(year, 'data','output','ebs','surveyISS_data.Rdata'))

## get marginal comps @ bins saved in data/output/ebs
## saves to here(); I moved them to data/output
surveyISS::srvy_iss(iters = 10, 
                    bin = c(seq(6,40,2),seq(43,58,3)), ## length databins
                    lfreq_data = data$lfreq,
                    specimen_data = data$specimen, 
                    cpue_data = data$cpue, 
                    strata_data = data$strata,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ebs', save= 'marginal_length' )


marginal_comps <- surveyISS::srvy_comps(
  lfreq_data = data$lfreq,
  specimen_data = data$specimen, 
  cpue_data = data$cpue, 
  strata_data = data$strata, 
  r_t = surveyISS::read_test,
  yrs = NULL,
  bin = unique(mod18.2c_2024$lendbase$Bin), ## length databins
  boot_hauls = TRUE, 
  boot_lengths = TRUE, 
  boot_ages = TRUE, 
  al_var = TRUE, 
  al_var_ann = TRUE, 
  age_err = TRUE, 
  plus_len = NULL,
  plus_age = NULL,
  by_strata = FALSE,
  global = FALSE
)


 
#get caals @ bins 
surveyISS::srvy_iss_caal(iters = 10,
                         bin = sort(unique(mod18.2c_2024$condbase$Lbin_hi)), ## distinct bins for caal data
                         plus_age = max(mod18.2c_2024$agebins),
                         specimen_data = data$specimen, 
                         cpue_data = data$cpue,  
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ebs', 
                         save = 'caal')

caal_iss <- read.csv(here::here(year,'data','output','ebs','caal_iss_caal.csv'))
 

caal00 <- surveyISS::srvy_comps_caal(
  specimen_data = data$specimen, 
  cpue_data = data$cpue, 
  r_t = surveyISS::read_test,
  bin = sort(unique(mod18.2c_2024$condbase$Lbin_hi)), ## length databins
  boot_hauls = TRUE,  
  boot_ages = TRUE, 
  al_var = TRUE, 
  al_var_ann = TRUE, 
  age_err = TRUE, 
  plus_len = max(mod18.2c_2024$condbase$Lbin_hi),
  plus_age = max(mod18.2c_2024$agebins)) %>% 
  as.data.frame() %>%
  select(-caal.species_code) 

names(caal00) <- gsub('caal.','',names(caal00))

## wrangle data ----
#* format sex, drop years, munge into SS3 format


caal_iss_merge <- caal_iss %>%
  filter(sex != 0) %>% 
  filter(!is.na(iss)) %>%
  select(year, sex, length, iss) %>%
  merge(.,caal00, by = c('year','sex','length')) 

caal00$sex <- ifelse(caal00$sex  == 1,'males','females')

caal0<-caal_iss_merge %>% ## raw number of individuals
  filter(sex != 0) %>% 
  arrange(age) %>%
  tidyr::pivot_wider(names_from=age, 
                     values_from=caal,
                     # names_prefix='a', 
                     values_fill=0) %>%
  arrange(year, sex, length   )

bind_cols(caal0,caal0[,-(1:4)]) %>%
  mutate(Seas = 7, Fleet = 2, sex = ifelse(sex == 'males',2,1),
         Part = 0, Ageerr = 1) %>%  
  select(Yr = year, Seas, Fleet, sex, Part, Ageerr, Lbin_lo = length,
         Lbin_hi = length , Nsamp=iss,everything()) %>%
  write.csv(., file = here::here(year,'data','output','srv_caal_ss3-surveyISS.csv'), 
            row.names = FALSE)
