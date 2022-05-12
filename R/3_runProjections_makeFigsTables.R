## Code to produce results 
require(dplyr)
require(tidyr)
require(ggplot2)
require(here)
## Execute proj module ----
## once spp_catch is set up you can run the projection module
setwd(here('projection','Projections')) 
shell('main')


## compile tables - see Cole's report.R ----
### The projection model results
## ## Use R to process output into easy file to create the harvest
## ## table in report.xlsx.
this_year = 2021
rec_table1 <-
  read.table('percentdb.out') %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year+1:2) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table1[3:6,3:4] <- 1000*rec_table1[3:6,3:4]

rec_table2 <-
  read.table('alt2_proj.out', header=TRUE) %>%
  filter(Year %in% (this_year+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table2[,2:3] <- 1000*rec_table2[,2:3]
rec_table <- bind_rows(rec_table1, rec_table2)

## change order to match SAFE format & magnitudes
rec_table <- rec_table[c(11,6,3,5,4,2,1,1,9,8,8),] 

write.csv(rec_table, 'rec_table.csv', row.names=FALSE)





### --------------------------------------------------
### Area apportionment
## Run the RE model for each area-specific survey to get this
## year's estimates and use that to get proportions. I didn't
## actually run this in 2020 because there was no survey. For
## 2021 need to rework this chunk. CCM -10/2020

## MSK WAITING FOR SURVEY DATA AS OF 09-21-2021
index_by_area <- read.csv('data/index_by_area.csv') %>%
  mutate(CV=sqrt(POPVAR)/POP)



## plots ----

## Projection plots ----
## notes from CMM
## ### Jim says to use the means from the bigfile. But I don't
## think this works it's missing a bunch of stuff.
## bigout <- read.table('projection/2019_Projections/bigfile.out', header=TRUE) %>%
##   filter(Alternative==1 & Yr %in% (this_year+1:2)) %>%
##   select(-Spp, -Alternative) %>% group_by(Yr) %>%
##   summarize_all(mean) %>% pivot_longer(c(-Yr), names_to='metric') %>% pivot_wider(names_from=Yr)
## ## It's missing B0/B40/B35 so get that from this file. I think if
## ## if I update proj this will not be necessary. Try that next year
## B0 <- strsplit(readLines('projection/2019_Projections/percentiles.out',
##   n=3)[3], ' ')[[1]][1:3] %>% as.numeric()
## rec_table <- rbind(bigout, data.frame(metric=c('SB0', 'SB40', 'SB35'),
##                                    '2020'=B0, '2021'=B0, check.names=FALSE))
pdt <- data.frame(read.table("bigfile.out", header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
g <- ggplot(pdt.long, aes(Yr,  med, ymin=lwr, ymax=upr, fill=Alternative, color=Alternative)) +
  facet_wrap('metric', scales='free_y') + ylim(0,NA) +
  geom_ribbon(alpha=.4) + theme_bw() +
  labs(x='Year', y='Estimated 80% CI')


## load previous BASE model
remotes::install_github("r4ss/r4ss", ref = 'e588b878c06f3a60fe661e5d6e0a6d096d19d57a' )
## getting morph error otherwise
base20mod <- r4ss::SS_output(here('2020_files','model_runs','Run06_francis_tuning'))
# SSplotComparisons(SSsummarize(list(base17,base20mod) ) )

#* Fig 1 catch/totbio plot ----
## per report.xlsx/Fig1, looks like biomass is from the assessment thru 2016 then values from proj
## the figure caption indicates these are for 3+ but the model was run using age 0 as the summary biomass
## had to rerun with the summary age updated
fig1a <- base20mod$timeseries %>% select(Yr, Bio_smry) %>%
  merge(.,base20mod$catch %>% select(Yr, Obs), by = 'Yr') %>%
  filter(Yr != 2020) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)

## projection catch values in spp_catch, use Tot_biom from pdt

## summarise values in pdt

sppcatch <- read.table(here('projection','projections','spp_catch.dat'),
                       skip = 24) %>%
  data.frame() %>% select(Yr = V1, catch = V2)


fig1b <- data.frame(Yr = seq(2020,2023,1),
                    Bio_smry = pdt %>% filter(Yr < 2024) %>% group_by(Yr) %>%
                      summarise(Bio_smry = 1000*round(mean(Tot_biom),2)) %>% select(Bio_smry) ,
                    Obs = sppcatch$catch) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)


fig1 <- rbind(fig1a, fig1b)


## plot with diff colors for extrapolated and forecasted catches
ggplot(subset(fig1, Yr < 2021), aes(x = Yr, y = catch_over_biomass)) +
  geom_line(lwd = 1, col = 'dodgerblue2') +
  # geom_point(data = subset(fig1, Yr==2020),
  #           size = 3, col = 'grey44') +
  geom_line(data = subset(fig1, Yr > 2019),
            lwd = 1, linetype = 'dotted',  col = 'grey44') +
  scale_x_continuous(labels = seq(1960,2025,5), 
                     breaks = seq(1960,2025,5))+
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(0,0.08,0.01), 
                     labels = seq(0,0.08,0.01))+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 3+)')+
  ggsidekick::theme_sleek()

ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-Fig1_catchvsbio.png')))

#* index plot ----
# index <- read.csv(here('data','2021-09-15-index.csv'))
index <- read.csv(here('data','2021-09-22-ss_survey_index.csv')) %>%
  mutate(lci = obs-se_log*obs, uci = se_log*obs+obs)
# index %>% filter(  YEAR != 2019) %>% summarise(mb=mean(BIOM), sdb = sd(BIOM)) %>% mutate(mb+sdb)
ggplot(index, aes(x = year, y = obs/1000)) +
  geom_line(lwd = 1, col = 'grey77') +
  # geom_point() +
  geom_point(data = subset(index, year == 2021), color = 'blue') +
  scale_x_continuous(labels = seq(1983,2021,2),
                     breaks = seq(1983,2021,2))+
  # scale_x_continuous(labels = seq(1980,2025,5), 
  #                    breaks = seq(1980,2025,5))+
  scale_y_continuous(limits = c(0,1000) ) +
  labs(x = 'Year', y = 'Survey Biomass (1000 mt)')+
  geom_ribbon(aes(ymin =lci/1000, ymax = uci/1000 ),alpha = 0.2)+

  ggsidekick::theme_sleek()

ggsave(last_plot(), height = 6, width = 10, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_wCVs.png')))


#* catches vs TAC ----

## NOTE: there seemed to be some input errors with TACs in years 2016 and 2019
## based on the data downloadable on AKFIN under "BSAI groundfish specifications".
## This came to my attention because the catches used in the base 2020 model were above the TACs for those years.
## instead

cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73" ,"#F0E442", "#0072B2", "#D55E00" )
cbpal <- c("#E69F00", "#56B4E9", "#009E73" ,'black','grey66' )
cbpal <- c("#E69F00", "#56B4E9", 'black','grey66' )

catch <- read.csv(here('data','2021-10-29-catch.csv'))
mgmt0 <- read.csv(here('data','2021-11-12-BSAI_harvest_specs_1986_2021new.csv'), header = F)[,-1]
mgmt1 <- rbind(mgmt0[1:2,],as.numeric(gsub(",", "", mgmt0[3,]))) ## make harvest specs numeric (remove comma)
mgmt<- mgmt1%>%
  t() %>%  data.frame(.) %>%
  pivot_wider(.,names_from = 'X2', 
              values_from = 'X3') %>%
  mutate(Yr = X1) %>%
  select(Yr, TAC, ABC, OFL)

base20mod$catch %>%filter(Yr == 2019)

## extrapolated catches
xtrayrs <- data.frame(Yr = sppcatch$Yr,
                      Catch = sppcatch$catch)

merge(mgmt, base20mod$catch %>% 
        select(Yr, Catch = Obs),
      by ='Yr', all.x = TRUE)  %>%
  merge(., xtrayrs, by = 'Yr',all = T) %>%
  mutate(c1= Catch.x, c2= Catch.y) %>%
  data.frame(.) %>%
  select(-Catch.x, -Catch.y) %>%
  reshape2::melt(., id = 'Yr') %>%
  mutate(value = as.numeric(value), Yr = as.numeric(Yr)) %>%
  filter(variable != 'OFL') %>%
  filter(Yr >2016)
  
  ggplot(., aes(x = Yr,y = value, color = variable, group = variable)) +
  geom_line(lwd = 1.1) +
  ggsidekick::theme_sleek() +
  theme(legend.text = element_text(size = 14), legend.position = 'bottom',
        axis.text = element_text(size = 14),axis.title  = element_text(size = 14)) +
  scale_x_continuous(limits = c(1995,2025),
                     breaks =  seq(1995,2025,5),
                     labels = seq(1995,2025,5)) +
  # scale_color_manual(values = cbpal,
  #                    labels = c('TAC','ABC','OFL',"Catches in Base Model", "Catches used in Projections" ))+
  # annotate('text', x = rep(2020.5,4), y = c(47500,40000,30000, 4500),
  #          label = c('OFL','ABC','TAC','Catches'),
  #          color = c(cbpal[3:1],'grey44'), size = 6)+
  scale_color_manual(values = cbpal,
                     labels = c('TAC','ABC',"Catches in Base Model", "Catches used in Projections" ))+
  annotate('text', x = rep(2020.5,3), 
           y = c(75000,30000, 4500),
  label = c('ABC','TAC','Catches'),
  color = c(cbpal[2:1],'grey44'), size = 6)+
  labs(x = 'Year', y = 'Catch or Harvest Spex (t)', color = '')

ggsave(last_plot(), file = here("figs","harvest_spex_vs_catch.png"),
       height = 7, width  = 12, unit = 'in', dpi = 520)



## 2020 ssb plot with current b40 ----

png(here("figs","SSB_2020_vsB40_2022.png"), width = 7, height = 5, unit = 'in', res = 520)
SSplotTimeseries(base20mod, subplot = 7)
abline(h = 81463, lty = 'dashed')
text(x = 2015, y = 86000, label = expression('B'[40]*'=81,463 mt'))
dev.off()


## Tables ----



#* Table 1 catches by spp, using the observer data ----
## Observer data on species-specific extrapolated weight in each haul was summed
## over hauls within each year and used to calculate the proportion of the total 
## Hippoglossoides spp. catch that was flathead sole or Bering flounder.
## Proportions were multiplied by the total Hippoglossoides spp. (flathead sole
## and Bering flounder combined) catches reported by AKFIN to obtain total catch 
## of flathead sole separately from that of Bering flounder.


## MSK: Not sure where CCM got the values for < 1992. I copied in his table from
## the last BSAI assessment and only updated the last few years here.

totals <- read.csv(here('data','2021-10-29-catch.csv')) %>%
  group_by(year=YEAR) %>%
  summarize(catch=sum(TONS), .groups='drop')

## manually add the projected catch

catch_proportions <- readRDS(here('data','2021-10-29-catches_observer.RDS')) %>%
  group_by(year, species) %>%
  summarize(weight=sum(weight)/1000, .groups='drop') %>%
  pivot_wider(names_from=species, values_from=weight,values_fill=0) %>%
  mutate(prop_bf=Bering_flounder/(Bering_flounder+flathead_sole),
         prop_fs=1-prop_bf
  ) %>% 
  filter(year>=1995 | year==1992) %>%
  merge(.,totals, by='year', all = T) %>% 
  mutate(total = round(catch),
         FHS = round(prop_fs*catch),
         BF = round(prop_bf*catch)) %>%
  
  select(year, total, FHS, BF)

write.csv(catch_proportions, file = here('report',paste0(Sys.Date(),'-catch_proportions.csv')), row.names=FALSE)


#* Table 2 ----
## EBS/AI survey data, CV by SPP.
## total (all regions, both spp), AI (both spp), EBS combo ("hippo spp"), EBS flathead, EBS flounder, w CVs
## note that for this table in 2020 cole did NOT show any imputed AI values; the "totals" are as used in assessment

index_ai <- read.csv('data/2021-09-22-biomass_survey_ai.csv') %>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'AI', cv = BIOMASS_VAR /TOTAL_BIOMASS) %>%
  select(year=YEAR, species, biomass=TOTAL_BIOMASS,
         variance = BIOMASS_VAR, survey) %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))

index_ebs_spp <- read.csv('data/2021-09-28-biomass_survey_ebs_by_species.csv')%>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'ebs') %>%
  select(year=YEAR,species, biomass=BIOMASS,
         variance = VARBIO, survey) %>%mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))

index_ebs <-  read.csv("data/2021-09-22-biomass_survey_ebs.csv") %>%
  select(year=YEAR, biomass=BIOMASS,
         variance=VARBIO) %>% cbind(survey='ebs') %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5)) %>%
  select(year, ebs_total = biomass, ebs_total_cv = cv)


ebs2 <- index_ebs_spp %>% select(year, species, biomass, cv) %>%
  pivot_wider(names_from=species, values_from=c(biomass, cv)) %>% 
  select(year, ebs_flathead = biomass_flathead_sole, ebs_flathead_cv = cv_flathead_sole, 
         ebs_bering = biomass_Bering_flounder, ebs_bering_cv=cv_Bering_flounder)

index_ai %>%
  pivot_wider(names_from=survey, values_from=c(biomass, cv)) %>%
  select(-variance, -species) %>%
  merge(., SS_index %>% select(year, obs, se_log), by = 'year',all = T) %>%
  select(year, total = obs, cv_total = se_log, biomass_AI, cv_AI) %>%
  merge(., index_ebs) %>%
  merge(., ebs2) %>%

  write.csv(., file = here('report',paste0(Sys.Date(),'-survey_by_spp.csv')), row.names=FALSE)



#* Table 3 NBS ----

index_nbs <- read.csv('data/2021-10-05-biomass_survey_NBS_by_species.csv') %>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'NBS', cv = VARBIO /BIOMASS) %>%
  select(year=YEAR, species, biomass=BIOMASS,
         variance = VARBIO, survey) %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))
  

## make a total column, tot var is sum of squares
nbs2 <- index_nbs %>% 
  group_by(year) %>%
  summarise(totbio = sum(biomass),
            var2 = sum(variance),
            cv=round(sqrt(log(1+var2/totbio^2)),5)) %>%
  select(year, nbs_total = totbio, nbs_total_cv = cv)



index_nbs %>% select(year, species, biomass, cv) %>%
  pivot_wider(names_from=species, values_from=c(biomass, cv)) %>%
  merge(., nbs2, by = 'year') %>%
  select(year, nbs_total, nbs_total_cv, biomass_flathead_sole, cv_flathead_sole, biomass_Bering_flounder, cv_Bering_flounder) %>%
  
  write.csv(., file = here('report',paste0(Sys.Date(),'-NBS_survey_by_spp.csv')), row.names=FALSE)

