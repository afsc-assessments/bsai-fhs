## exploration of new ISS values using surveyISS package
## I am not using afscISS as we need custom bin widths
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

data_lbins <- c(seq(6,40,2),seq(43,58,3))
caal_lbins <- seq(6,58,2) 
amax <- 21
lmax <- 58

## download data   ----
data <- surveyISS::query_data(survey = 98, ## bs shelf only
                              region = 'ebs',  
                              species = 10130) ## fhs only


save(data, file = here::here(year, 'data','output','ebs','surveyISS_data.Rdata'))

#* ISS ----
surveyISS::srvy_iss(iters = 10, 
                    bin = data_lbins,  
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
                    region = 'ebs', save= 'marginal' )

surveyISS::srvy_iss_caal(iters = 10,
                         bin = caal_lbins, ## distinct bins for caal data
                         plus_age = amax,
                         specimen_data = data$specimen, 
                         cpue_data = data$cpue,  
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ebs', 
                         save = 'caal')

#* observed comps ----
marl00 <- surveyISS::srvy_comps(
  lfreq_data = data$lfreq,
  specimen_data = data$specimen, 
  cpue_data = data$cpue, 
  strata_data = data$strata, 
  r_t = surveyISS::read_test,
  yrs = NULL,
  bin = data_lbins, ## length databins
  boot_hauls = TRUE, 
  boot_lengths = TRUE, 
  boot_ages = TRUE, 
  al_var = TRUE, 
  al_var_ann = TRUE, 
  age_err = TRUE, 
  plus_len = lmax,
  plus_age = amax,
  by_strata = FALSE,
  global = FALSE
)  

marage00 <-marl00$age%>%
  mutate(totN = sum(agepop), .by = c(year,sex)) %>%
  mutate(freq = agepop/totN)
marlen00 <- marl00$length %>%
  mutate(totN = sum(abund), .by = c(year,sex)) %>%
  mutate(freq = abund/totN)

write.csv(marage00, 
          file = here::here(year, 'data','output','ebs','marginal_age_surveyISS_raw.csv'),
          row.names = FALSE)

write.csv(marlen00, 
          file = here::here(year, 'data','output','ebs','marginal_length_surveyISS_raw.csv'),
          row.names = FALSE)

caal00 <- surveyISS::srvy_comps_caal(
  specimen_data = data$specimen, 
  cpue_data = data$cpue, 
  r_t = surveyISS::read_test,
  bin = caal_lbins, ## length databins
  boot_hauls = TRUE,  
  boot_ages = TRUE, 
  al_var = TRUE, 
  al_var_ann = TRUE, 
  age_err = TRUE, 
  plus_len = lmax,
  plus_age = amax) %>% 
  as.data.frame() %>%
  select(-caal.species_code) 
names(caal00) <- gsub('caal.','',names(caal00))


write.csv(caal00, file = here::here(year, 'data','output','ebs','caal_surveyISS_raw.csv'),
          row.names = FALSE)


## wrangle data ----

#* marginal lcomps ----
lcomp_iss <- read.csv(here::here(year,'data','output','ebs','marginal_iss_ln.csv'))
marlen00 <- read.csv(here::here(year,'data','output','ebs','marginal_length_surveyISS_raw.csv'))

## to avoid confusion with SS3 syntax
marlen00$sex <- ifelse(marlen00$sex==1,'males','females') 
lcomp_iss$sex <- ifelse(lcomp_iss$sex==1,'males','females')

lcomp0 <- lcomp_iss %>%
  filter(sex != 0) %>% 
  filter(!is.na(iss)) %>%
  select(year, sex, iss) %>%
  merge(.,marlen00, by = c('year','sex')) %>%
  select(-species_code, -abund, -totN) %>%
  # distinct() %>%
  arrange(length) %>% ## ensure column order is correct
  tidyr::pivot_wider(names_from=length, 
                     values_from=freq, 
                     values_fill=0) %>%
  arrange(year, sex ) %>%
  mutate(SEX = ifelse(SEX == 'males',2,1)) ## overwrite SS3 syntax

filter(SEX == 1) %>% 
  merge(., srvlen0 %>% 
          filter(SEX == 2) %>% 
          select(-SEX, -HAULJOIN), 
        by = c('YEAR'), all.y = FALSE) %>%
  mutate(Seas = 7, FltSvy = 2, Gender = 3, Part = 0,) %>%
  select(Yr = YEAR, Seas, FltSvy, Gender, Part, Nsamp=HAULJOIN, everything(),
         -SEX) %>%
  arrange(Yr) #%>%

bind_cols(lcomp0,lcomp0[,-(1:4)]) %>%
  mutate(Seas = 7, Fleet = 2, sex = ifelse(sex == 'males',2,1),
         Part = 0, Ageerr = 1) %>%  
  select(Yr = year, Seas, Fleet, sex, Part, Ageerr, Lbin_lo = length,
         Lbin_hi = length , Nsamp=iss,everything()) %>%
  write.csv(., file = here::here(year,'data','output','srv_caal_ss3-surveyISS.csv'), 
            row.names = FALSE)
#* marginal acomps (ghosted) ----

#* caals ----

#* format sex, drop years, munge into SS3 format

caal_iss <- read.csv(here::here(year,'data','output','ebs','caal_iss_caal.csv'))






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
