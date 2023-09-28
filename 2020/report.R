library(tidyverse) # '1.3.0'
library(lubridate)

## What data are new?
datold <- SS_readdat('inputs/2018_BSAI_FHS_1.dat', verbose=FALSE)
datnew <- SS_readdat('inputs/2020_BSAI_FHS.dat', verbose=FALSE)
setdiff(datnew$catch[,c('year', 'fleet')], datold$catch[,c('year', 'fleet')])
setdiff(datnew$CPUE[,c('year')], datold$CPUE[,c('year')])
setdiff(datnew$lencomp[,c('Yr', 'FltSvy')], datold$lencomp[,c('Yr', 'FltSvy')])
setdiff(datnew$agecomp[,c('Yr', 'FltSvy')], datold$agecomp[,c('Yr', 'FltSvy')])


### Get weekly catches to figure out how to extrapolate through
### this year for proj
### --------------------------------------------------
this_year <- 2020
## For projection model need to predict total catches in this
## year. Use weekly catches from from previous years to get
## proportion of catch by week to estimate terminal year catch.
##
files <- list.files('data/weekly_catches/', full.names=TRUE)
test <- lapply(1:length(files), function(i){
  skip <- grep('ACCOUNT.NAME', readLines(files[i]))-1
  data.frame(read.table(files[i], skip=skip, header=TRUE, sep=',',
             stringsAsFactors=FALSE))
})
weekly_catches <- do.call(rbind, test)
names(weekly_catches) <- c('species', 'date', 'catch')
weekly_catches <- weekly_catches %>%
  ## No species for Bering flounder, probably in FHS already
  filter(grepl("Flathead", x=species)) %>%
  mutate(date=mdy(date), week=week(date),  year=year(date))
catch_this_year <- weekly_catches %>% filter(year==this_year) %>%
  pull(catch) %>% sum
## Get average catch between now and end of year for previous 5
## years
catch_to_add <- weekly_catches %>% filter(year>=this_year-5 & week > week(today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Precited ", this_year, " catch= ", round(catch_this_year + catch_to_add,0))
## message('Predicted ',this_year, ' catch= ', round(catch_this_year/avg_proportion,0))
##
## The averages for this_year+1 and +2 are calculated in the
## report spreadsheet under catch



## Table 9.1 catches by species. Modified from Carey's file
## AssessmentTables/Get_Fishery_Stats_with_NORPAC_and_BF_FHS_Split_Catches.R
## For each year get the proportion of extrapolated weight that
## was FHS vs BF, then take AKFIN weightes and multiply to split
## these.
##
totals <- read.csv('data/catch.csv') %>% group_by(year=YEAR) %>%
  summarize(catch=sum(TONS), .groups='drop')
catch_proportions <- readRDS('data/catches_observer.RDS') %>%
  group_by(year, species) %>%
  summarize(weight=sum(weight)/1000, .groups='drop') %>%
  pivot_wider(names_from=species, values_from=weight, values_fill=0) %>%
  mutate(prop_bf=Bering_flounder/(Bering_flounder+flathead_sole),
         prop_fs=1-prop_bf) %>% filter(year>=1995 | year==1992) %>%
  merge(., totals, by='year')
write.csv(catch_proportions, file = 'report/catch_proportions.csv', row.names=FALSE)
## ## My catch file contains species but also has some NAs, I
## ## presume this is what observer data was used before so I
## ## continue donig that here. Also it is only FHS in early years.
## catch_species <- read.csv('data/catch.csv') %>%
##   filter(!is.na(SPECIES)) %>%
##   group_by(year=YEAR, species=SPECIES) %>%
##   summarize(catch=sum(TONS), .groups='drop') %>%
##   pivot_wider(names_from=species, values_from=catch, values_fill=0)

## Table 9.2 CDQ catches
catch_cdq <- read.csv('data/catch.csv') %>%
  filter(YEAR>1992)  %>%
  group_by(year=YEAR, species=CDQ) %>%
  summarize(catch=sum(TONS), .groups='drop') %>%
  pivot_wider(names_from=species, values_from=catch,
              values_fill=0) %>%
  mutate(total=Y+N, proportion_cdq=Y/total) %>%
  select(year, total=total, CDQ=Y, 'non-CDQ'=N, proportion_cdq)
write.csv(catch_cdq, file = 'report/catch_cdq.csv', row.names=FALSE)

## Table 9.3 catches by gear
catch_by_gear <- readRDS('data/catches_observer.RDS') %>%
  filter(year>1991) %>% group_by(year, gear) %>%
  summarize(catch=sum(weight)) %>%
  group_by(year) %>% mutate(total=sum(catch)) %>%
  ungroup() %>%
  mutate(pct=catch/total,
         ## from page 273 of FMA manual it has gear codes:
         gear=case_when(gear==1~"non_pelagic_trawl",
                        gear==2~"pelagic_trawl",
                        gear==4~"pair_trawl",
                        gear==5~"shrimp_trawl",
                        gear==6~"pot",
                        gear==8~"longline",
                        TRUE~NA_character_))
if(any(is.na(catch_by_gear$gear)))
  stop("Some gears failed to convert")
catch_by_gear <- catch_by_gear %>%
  pivot_wider(c(-catch, -total), names_from=gear,
              values_from=pct, values_fill=0)
write.csv(catch_by_gear, file='report/catch_by_gear.csv', row.names=FALSE)

## Table 9.4: Catch by area
catch_by_area <- readRDS('data/catches_observer.RDS') %>%
  filter(year>=1992) %>% group_by(year, area) %>%
  summarize(catch=sum(weight), .groups='drop') %>%
  group_by(year) %>% mutate(total=sum(catch)) %>%
  mutate(pct=catch/total) %>% arrange(area) %>%
  pivot_wider(c(-catch, -total), names_from=area,
              values_from=pct, values_fill=0) %>%
  ungroup %>% arrange(year)
write.csv(catch_by_area, file='report/catch_by_area.csv', row.names=FALSE)
## ### Why does't it work to get this from the AKFIN catches?
## catch_by_area <- read.csv('data/catch.csv') %>%
##   group_by(year=YEAR, area=NMFS_AREA) %>%
##   summarize(catch=sum(TONS), .groups='drop') %>%
##   group_by(year) %>%
##   mutate(total=sum(catch), pct=catch/total) %>%  arrange(area,year)  %>%
##   pivot_wider( names_from=area, values_from=catch, values_fill=0) %>% ungroup()
## write.csv(catch_by_area, file='report/catch_by_area.csv', row.names=FALSE)

## Table 9.5 comes from
## https://alaskafisheries.noaa.gov/status-of-fisheries/
## See report.xlsx for instructions how to complete.

## Table 9.6 catch retained, TAC, OFL etc
catch_retained <- read.csv('data/catch.csv') %>%
  filter(YEAR>=1995) %>%
  group_by(year=YEAR, retained=TYPE) %>%
  summarize(catch=sum(TONS), .groups='drop') %>%
  pivot_wider(names_from=retained, values_from=catch,
              values_fill=0) %>%
  mutate(total=R+D, proportion_retained=R/total) %>%
  select(year, total=total, retained=R, discard=D, proportion_retained)
write.csv(catch_retained, file = 'report/catch_retained.csv', row.names=FALSE)

## Table 9.9. Survey biomass results broken down in several ways
bio_by_species <- read.csv('report/biomass_survey_ebs_by_species.csv') %>%
  mutate(CV=sqrt(VARBIO)/BIOMASS, species=gsub(" ", "_", COMMON_NAME)) %>%
  select(year=YEAR, bio=BIOMASS, species, CV) %>%
  pivot_wider(names_from=species, values_from=c(bio,CV))
bio_ebs_grouped <- read.csv('data/biomass_survey_ebs.csv') %>%
  mutate(CV_ebs_grouped=sqrt(VARBIO)/BIOMASS) %>%
  select(bio_ebs_grouped=BIOMASS, year=YEAR, CV_ebs_grouped)
bio_ai <- read.csv('data/biomass_survey_ai.csv') %>%
  mutate(CV_ai=sqrt(BIOMASS_VAR)/TOTAL_BIOMASS, species=gsub(" ", "_",COMMON_NAME)) %>%
  select(year=YEAR, bio_ai=TOTAL_BIOMASS, CV_ai)
bio_combined <- read.csv('inputs/SS_survey_index.csv') %>%
  ## SE on log scale, which SS requires, is SE=sqrt(log(1+CV^2)) so
  ## convert back to CV
  mutate(CV_total= sqrt(exp(se_log^2)-1)) %>%
           select(year, bio_total=obs, CV_total)
## Merge all together to make sure lined up right
bio <- bio_by_species %>%
  full_join(bio_ebs_grouped, by='year') %>%
  full_join(bio_ai, by='year') %>%
  full_join(bio_combined, by='year') %>%
  ## select into right order for easier table making
  select(year, bio_total, CV_total, bio_ai, CV_ai,
         bio_ebs_grouped, CV_ebs_grouped, bio_flathead_sole,
         CV_flathead_sole, bio_Bering_flounder, CV_Bering_flounder)
write.csv(bio, 'report/biomass_surveys_table.csv', row.names=FALSE)



#### Actually fund a spreadsheet from Carey that does this a bit easier
## ## Projection tables
## proj <- read.table('projection/Projections/bigfile.out', skip=1)
## names(proj) <- c('Alternative','Spp', 'Yr', 'ABC', 'OFL',
##                  'Catch', 'SSB', 'F', 'Tot_biom', 'SPR_Implied', 'Ntot', 'SexRatio')
## proj <- proj %>% select(-Spp) %>% group_by(Alternative, Yr) %>%
##   summarize_all(mean) %>%
##   pivot_longer(-(1:2)) %>%
##   pivot_wider(names_from=Alternative, names_prefix='Scenario ') %>%
##   arrange(desc(name), Yr) %>% filter(name %in% c('SSB', 'F', 'Catch'))
## write.csv(proj, file='report/projection_tables.csv', row.names=FALSE)
