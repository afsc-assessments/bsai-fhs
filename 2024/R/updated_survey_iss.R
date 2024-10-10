## 2024 October
## exploration of new ISS values using surveyISS package
## I am not using afscISS as we need custom bin widths

# devtools::install_github("BenWilliams-NOAA/surveyISS")
library(surveyISS)
library(dplyr)
library(ggplot2)
require(patchwork)
## NOTES
## Comps are FHS only and BS  only (AI is only used for biomass) (10130)
## Will only have data thru 2023 for caal and marginal lengths
## recall marginal ages are ghosted
## length comps are female then male, l to rate
## caals are entered as sex specific with dummy values
## ages are 1:21; lengths are in weird bins unclear if we can back them out
year <- 2024
data_lbins <- c(seq(6,40,2),seq(43,58,3))
caal_lbins <- seq(6,58,2) 
amax <- 21
lmax <- 58

## download data   ----
data <- surveyISS::query_data(survey = 98, ## bs shelf only
                              region = 'ebs',  
                              species = 10130) ## fhs only

save(data, 
     file = here::here(year, 'data','output','ebs','surveyISS_data.Rdata'))

#* ISS ----
surveyISS::srvy_iss(iters = 500, 
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

surveyISS::srvy_iss_caal(iters = 500,
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
                         save = 'conditional')





## wrangle data ----
#* read in gapindex ss3 files ----
list.files(here::here(year,'data','output'), pattern = '*-gapindex', full.names = T) %>%
  lapply(., FUN = function(x){assign(substr(basename(x),1,11), 
                                     read.csv(x), 
                                     .GlobalEnv)})


lcomp_iss <- read.csv(here::here(year,'data','output','ebs','marginal_iss_ln.csv')) %>%
  filter(sex %in% c(1,2)) %>%
  select(year, sex = sex_desc, iss) %>%
  summarize(nsamp = sum(iss), .by = c(year))

acomp_iss <- read.csv(here::here(year,'data','output','ebs','marginal_iss_ag.csv')) %>%
  filter(sex %in% c(1,2)) %>%
  select(year, sex = sex_desc, iss) %>%
  summarize(nsamp = sum(iss), .by = c(year))

caal_iss <- read.csv(here::here(year,'data','output','ebs','conditional_iss_caal.csv')) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(sex = ifelse(sex == 1, 'male','female')) %>%
  mutate(sex = ifelse(sex == 'female',1,2)) %>%
  select(year, length, sex, iss) ## get it back to ss3 syntax

caal_iss[is.na(caal_iss$iss),]

## viz compare sample sizes ----
p1 <- lcomp_iss %>% 
  mutate(src = 'surveyISS') %>%
  bind_rows(mod18.2c_2024$lendbase %>% 
              filter(Fleet == 2) %>%
              mutate(src = 'gapindex (current method)') %>%
              select(year=Yr, nsamp = Nsamp_in    , src),.) %>%
  ggplot(., aes(x = year, y = nsamp, color = src)) +
  geom_line() +
  theme(legend.position = 'none')+
  labs(title = 'marginal lengths')


p2 <- acomp_iss %>% 
  mutate(src = 'surveyISS') %>% 
  bind_rows(mod18.2c_2024$ghostagedbase %>% 
              filter(Fleet == 2) %>%
              mutate(src = 'gapindex (current method)') %>%
              select(year=Yr, nsamp = Nsamp_in    , src),.) %>%
  ggplot(., aes(x = year, y = nsamp, color = src)) +
  geom_line() +
  labs(title = 'marginal ages (ghosted)', color = '')


p3 <- caal_iss %>% 
  mutate(src = 'surveyISS') %>%
  select(year, nsamp = iss,length, sex, src) %>%
  bind_rows(mod18.2c_2024$condbase %>% 
              filter(Fleet == 2) %>%
              mutate(src = 'gapindex (current method)') %>%
              select(year=Yr, nsamp = Nsamp_in, 
                     length = Lbin_lo,sex  , src) %>%
              distinct(),.) %>%
  ggplot(., aes(x = year, y = length, 
                size = nsamp, color = src)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~src) +
  labs(title = 'CAAL',
       color = '',
       y = 'length bin (cm)')



(p1  | p2 )/p3

ggsave(last_plot(),
       file = here::here(year,'figs','comp_iss_compare.png'))

## overwrite NSAMP with ISS values and save ----
caal_iss$iss[is.na(caal_iss$iss)] <- 1
srv_len_ss3$Nsamp <- lcomp_iss$nsamp
srv_age_ss3$Nsamp <- acomp_iss$nsamp
srv_caal_ss1 <- merge(srv_caal_ss, caal_iss,
                      by.x = c('Yr','Sex','Lbin_lo'), 
                      by.y = c('year','sex', 'length'), all.x = TRUE)  %>%
  select(Yr, Seas, Fleet , Sex, Part, Ageerr, Lbin_lo , 
         Lbin_hi , iss, everything(),-Nsamp)  

write.csv(srv_len_ss3, file = here::here(year,'data','output','srv_len_ss3-surveyISS.csv'),
          row.names = FALSE)

write.csv(srv_age_ss3, file = here::here(year,'data','output','srv_age_ss3-surveyISS-ghost.csv'),
          row.names = FALSE)

write.csv(srv_caal_ss1, 
          file = here::here(year,'data','output','srv_caal_ss3-surveyISS.csv'),
          row.names = FALSE)

 




lcomp_iss %>% 
  mutate(src = 'surveyISS') %>%
  bind_rows(mod18.2c_2024$lendbase %>% 
              filter(Fleet == 2) %>%
              mutate(src = 'gapindex') %>%
              select(year=Yr, nsamp = Nsamp_in    , src),.) %>%
  ggplot(., aes(x = year, y = nsamp, color = src)) +
  geom_line() +
  labs(title = 'marginal length composition input sample sizes')


# deprecated ----
#* caals ----

#* format sex, drop years, munge into SS3 format

caal_iss <- read.csv(here::here(year,'data','output','ebs','conditional_iss_caal.csv'))
caal_comp00 <- read.csv(here::here(year, 'data','output','ebs','caal_surveyISS_raw.csv'))

caal_comp00$sex <- ifelse(caal_comp00$sex  == 1,'males','females')


caal_comp00 %>% 
  arrange(age) %>% ## ensure column order is correct
  tidyr::pivot_wider(names_from=age, 
                     values_from=caal, 
                     id_cols = c(year,sex),
                     values_fill=0) %>%
  arrange(year, sex ) %>%
  mutate(sex = ifelse(sex == 'males',2,1)) %>%
  merge(., lcomp_iss, by = 'year') ## overwrite SS3 syntax

caal_iss_merge <- caal_iss %>%
  filter(sex != 0) %>% 
  filter(!is.na(iss)) %>%
  select(year, sex, length, iss) %>%
  merge(.,caal00, by = c('year','sex','length')) 


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
#* marginal lcomps ----

marlen00 <- read.csv(here::here(year,'data','output','ebs','marginal_length_surveyISS_raw.csv'))

## to avoid confusion with SS3 syntax
marlen00$sex <- ifelse(marlen00$sex==1,'male','female') 

lcomp0 <-  
  marlen00 %>%
  select(-species_code,-abund,-totN) %>%
  arrange(length) %>% ## ensure column order is correct
  tidyr::pivot_wider(names_from=length, 
                     values_from=freq, 
                     id_cols = c(year,sex),
                     values_fill=0) %>%
  arrange(year, sex ) %>%
  merge(., lcomp_iss, by = c('year')) %>%
  mutate(sex = ifelse(sex == 'male',2,1))## overwrite SS3 syntax

srvlen_save<-lcomp0 %>%
  filter(sex == 1) %>% 
  merge(., lcomp0 %>% 
          filter(sex == 2) %>% 
          select(-sex,-nsamp), 
        by = c('year'), all.y = FALSE) %>%
  mutate(Seas = 7, FltSvy = 2, Gender = 3, Part = 0,) %>%
  select(Yr = year, Seas, FltSvy, Gender, Part, nsamp, everything(),
         -sex) %>%
  arrange(Yr)  

write.csv(srvlen_save, 
          file = here::here(year,'data','output','srv_len_ss3-surveyISS.csv'), 
          row.names = FALSE)

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


## calculate frequencies
marage00 <-marl00$age%>%
  filter(sex %in% c(1,2)) %>%
  mutate(totN = sum(agepop), .by = c(year,sex)) %>%
  mutate(freq = agepop/totN)

marlen00 <- marl00$length %>%
  filter(sex %in% c(1,2)) %>%
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
