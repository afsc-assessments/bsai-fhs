## download the raw data from Oracle and save
## based on data.R from 2020 assessment by Cole Monnahan

#  SETUP ----
#* Packages & Oracle ----
require(RODBC)
require(dplyr)
require(here)
require(ggplot2)
require(r4ss)
library(tidyverse)
require(rstudioapi) ## enables masking of RODBC name, password

options(digits=22) #Must have for reading in hauljoin

username_AFSC <- showPrompt(title="Username", message="Enter your AFSC username:", default="")
password_AFSC <- askForPassword(prompt="Enter your AFSC password:")
AFSC <- odbcConnect("AFSC",username_AFSC,password_AFSC,  believeNRows = FALSE)

username_AKFIN <- showPrompt(title="Username", message="Enter your AKFIN username:", default="")
password_AKFIN <- askForPassword(prompt="Enter your AKFIN password:")
AKFIN <- odbcConnect("AKFIN",username_AKFIN,password_AKFIN,  believeNRows = FALSE)

#* Query spex ----
final_year <- 2022
fsh_sp_area <- "'BS','AI'"              # FMP
fsh_sp_label <- "'FSOL'"                # AKFIN group species label
fsh_sp_str <- "103"                     # AKFIN species code
fsh_start_yr <- 1977                    # start year
sp_area <- "'BS'"                       #
## length bins to use for fsh and srv length comp data
max_size <- 40 ##65 = rex ##70 = Dover and Flathead
min_size <- 6
bin_width <- 2
len_bins <- c(seq(min_size,max_size,bin_width),43,46,49,52,55,58)
lapply(list.files(here('sql'), full.names = T), source) ## load all sql queries

# DATA DOWNLOAD ---- 
#* AKFIN Catches ---- 
message("Querying AKFIN to get catch...")
catch <- sqlQuery(AKFIN,qcatch) %>% arrange(YEAR, ZONE, GEAR)
write.csv(catch, file=here('data',paste0(Sys.Date(),'-catch.csv') ), row.names=FALSE)

#* AFSC observer catches ---- 
message("Querying haul-level catch from observer DB...")
catches_observer <- sqlQuery(AFSC,qcatch_obs)
catches_observer$ID <- paste(catches_observer$CRUISE, catches_observer$PERMIT,catches_observer$HAUL, sep="_")
catches_observer$SPECIES <- ifelse(catches_observer$SPECIES==103, 'flathead_sole', 'Bering_flounder')
catches_observer <- select(catches_observer, -CRUISE, -PERMIT, -HAUL) %>%
  rename(species=SPECIES, year=YEAR, weight=EXTRAPOLATED_WEIGHT,
         gear=GEAR_TYPE, area=NMFS_AREA, pct.retained=PERCENT_RETAINED)
saveRDS(catches_observer, file = here('data',paste0(Sys.Date(),'-catches_observer.RDS') ))
write.csv(catches_observer, file=here('data',paste0(Sys.Date(),'-catches_observer.csv') ), row.names=FALSE)

#* Weekly catches ---
message("Make sure to manually copy the weekly catch data")
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#bsai-groundfish
## Use "Catch by week" subheading and manually copied into the data
## folder. This is used for the tables which report catch. 
## For this year I copied in the xlsx files downloaded in 2021, then overwrite years 2020-2022


#* Survey biomass (EBS Shelf) ----
message("Querying EBS survey biomass data...")
test <- sqlQuery(AFSC, qsurv)
if(!is.data.frame(test)) stop("Failed to query EBS survey data")
write.csv(test, file=here('data',paste0(Sys.Date(),'-biomass_survey_ebs.csv') ), row.names=FALSE)

test <- sqlQuery(AFSC, qsurvspp)
if(!is.data.frame(test)) stop("Failed to query EBS survey data")
write.csv(test, here('data',paste0(Sys.Date(),'-biomass_survey_ebs_by_species.csv')), row.names=FALSE)


#* Survey biomass (AI) ----
## This only includes FHS b/c no BF found there
message("Querying AI survey biomass data...")
ai <- sqlQuery(AFSC, qsurvAI)
if(!is.data.frame(ai)) stop("Failed to query AI survey data")
write.csv(ai, file=here('data',paste0(Sys.Date(),'-biomass_survey_ai.csv') ), row.names=FALSE)

#* Survey biomass (NBS, for illustration only)  ----
test <- sqlQuery(AFSC, qnbs)
if(!is.data.frame(test)) stop("Failed to query NBS survey data")
write.csv(test, here('data',paste0(Sys.Date(),'-biomass_survey_nbs_by_species.csv')), row.names=FALSE)


# REFORMATTING ----
date_use <- "2022-05-31" ## dwnld date
data_folder = here('data','/')

#* Survey biomass -----
index_ebs <-  read.csv(paste0(data_folder,date_use,"-biomass_survey_ebs.csv")) %>%
  select(year=YEAR, biomass=BIOMASS,
         variance=VARBIO) %>% cbind(survey='ebs')
index_ai <- read.csv(paste0(data_folder,date_use,'-biomass_survey_ai.csv')) %>%
  mutate(species=gsub(" ", "_",COMMON_NAME)) %>%
  select(year=YEAR, biomass=TOTAL_BIOMASS,
         variance=BIOMASS_VAR) %>% cbind(survey='AI')
index_raw <- rbind(index_ebs, index_ai) %>%
  pivot_wider(names_from=survey, values_from=c(biomass, variance))

## Do a linear regression to get missing AI years
index_raw  <- index_raw %>% mutate(sd_ebs=sqrt(variance_ebs), sd_AI=sqrt(variance_AI))
z1 <- subset(index_raw, !is.na(biomass_AI))
z2 <- subset(index_raw, is.na(biomass_AI))
lmbio <- lm(biomass_AI~biomass_ebs, data=z1)
z2$biomass_AI <- as.numeric(predict(lmbio, newdata=z2))
lmvar <- lm(sd_AI~sd_ebs, data=z1)
z2$sd_AI <- as.numeric(predict(lmvar, newdata=z2))

## Recombine and add together biomass and variances
index <- rbind(z1,z2) %>% group_by(year) %>%
  summarize(biomass=round(biomass_AI+biomass_ebs,5),
            variance=sd_AI^2+sd_ebs^2,
            .groups='drop') %>%
  ## SE on log scale, which SS requires, is sqrt(log(1+CV^2))
  mutate(se_log=round(sqrt(log(1+variance/biomass^2)),5)) %>%
  select(-variance)

SS_index <- data.frame(year=index$year, seas=7, index=2, obs=index$biomass, se_log=index$se_log)
write.csv(x=SS_index, file= here('data',paste0(Sys.Date(),'-SS_survey_index.csv')) , row.names=FALSE)

# UNUSED ----
## Code below this line was not revisited for the update assessment.

# message("Querying EBS age-length...")
# ## Query written by Rebecca Haehn in 2020 based on a really old
# ## version by Dan. This replaces the need to use her dropbox .csv
# ## file.
# query <- "SELECT a.hauljoin, d.year, a.region, a.specimenid, a.biostratum, a.species_code, a.length, a.weight, a.sex,
#     a.age, b.start_time, b.bottom_depth, b. stratum, b.gear_temperature, b.bottom_type, b.gear_depth, b.performance,
#     b.duration, b.distance_fished, b.net_width, b.net_height, b.net_measured, b.start_latitude, b.end_latitude,
#     b.start_longitude, b.end_longitude, b.surface_temperature, b.gear, b.abundance_haul, c.nmfs_area,
#     decode(b.stratum,10,1,20,2,31,3,32,3,41,4,42,4,43,4,50,5,61,6,62,6,82,8,90,9,-9) subarea
#     from RACEBASE.SPECIMEN a
#     inner join RACEBASE.HAUL b
#     on a.hauljoin = b.hauljoin
#     inner join haehnr.ebs_nmfs_areas c
#     on b.stationid = c.stationid
#     left join HAULNAME d
#     on a.hauljoin = d.hauljoin
#     where a.region = 'BS' and
#     species_code in (10130, 10140) and
#     b.abundance_haul = 'Y';
# "
# test <- sqlQuery(AFSC, query)
# if(!is.data.frame(test)) stop("Failed to query age-length survey data")
# write.csv(test,file= here('data',paste0(Sys.Date(),'-ages_survey_ebs.csv')), test, row.names=FALSE)

#* survey size composition data -----
# message("Querying sizecomp EBS survey hauls...")
# query <- "
# select * from haehnr.samplesize_ebs_standard
# where species_code = 10130
# order by year;
# "
# test <- sqlQuery(AFSC, query)
# if(!is.data.frame(test)) stop("Query failed")
# write.csv(test, 'data/lengths_survey_hauls.csv', row.names=FALSE)
# 
# message("Querying sizecomp EBS survey...")
# query <- "
# select a.species_code,b.species_name, b.common_name,  year, stratum, length,
# round(males) males, round(females) females, round(unsexed) unsexed, round(total) total
# from haehnr. sizecomp_ebs_standard_stratum a, racebase.species b
# where a.species_code=b.species_code
# and a.species_code in (10130,10140)
# order by b.species_code, year, stratum, length;
# "
# test <- sqlQuery(AFSC, query)
# if(!is.data.frame(test)) stop("Query failed")
# write.csv(test, 'data/lengths_survey_ebs.csv', row.names=FALSE)
# 
# query <- "select species_code, year, total_hauls, hauls_w_length, num_lengths, num_lengths_males, num_lengths_females,
# hauls_w_otoliths, hauls_w_ages, num_otoliths, num_ages, num_ages_males, num_ages_females
#  from samplesize_ebs_standard2 where species_code=10130 order by  year;
# "
# test <- sqlQuery(AFSC, query)
# if(!is.data.frame(test)) stop("Query failed")



### ----- Get fishery length compositions data  -----
## Flathead sole only = 103, Rex = 105, Dover = 107, Bering flounder = 145, GT = 102, Deepsea sole = 110
## NMFS_AREA between 500 and 544 for BSAI, above 600 for GOA
# species <- 103 
# low.nmfs.area <- "'500'" #"'600'" #500
# hi.nmfs.area <- "'544'" #"'699'"  #544
# SpeciesCode = "103"
# ## Potentially unbury the choices for FmpArea for max flexibility.
# FmpArea <- "'BS'" #Options are 'AI' = 539-544 'GOA' = 600 to 699 'BS' = 500 to 539
# source('data/get_lcomps_fishery.R')

### ----- Get fishery age composition data  -----
# message("Querying fishery ages files...")
# SpeciesCode <- "103" #105 is rex sole
# FmpArea <- "500 and 544"  ##Typical options are AI = 539-544, GOA = 600 to 699 (600-650 incl. all the management areas and 690 is outside the EEZ), BS = 500 to 539
# source('~/assessments/2021/BSAI-flathead/2020_files/data/get_agecomps_fishery.R')



### Taken from Carey's
### AssessmentTables/NonTarget_Bycatch_Tables.R script
##
### I really think SQL should do the aggregating here, both of
### these queries are really slow and then ust get grouped
### anyway. SOmething to try for next time.
# message("Querying bycatch from AKFIN...")

# #User specifies:
# myspecies <- "Flathead Sole"
# FMParea = "'BSAI'"
# #MyDir = "\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2018\\BSAI_Flathead\\Writeups\\Main_Tables\\"
# ## MyDir = "\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2019\\Dover\\Writeups\\Main Tables\\"
# StartYr = 2009 #2008
# MyDir <- getwd()

# query<-paste0("SELECT council.comprehensive_nontarget.year,\n ",
#               "council.comprehensive_nontarget.nontarget_group_name,\n ",
#               "council.comprehensive_nontarget.fmp_area,\n ",
#               "council.comprehensive_nontarget.nontarget_estimate_weight,\n ",
#               "council.comprehensive_nontarget.nontarget_estimate_count,\n ",
#               "council.comprehensive_nontarget.trip_target_name\n ",
#               "FROM council.comprehensive_nontarget\n ",
#               "WHERE council.comprehensive_nontarget.year > ",StartYr,"\n ",
#               "AND   council.comprehensive_nontarget.fmp_area = ",FMParea)
# thed<-sqlQuery(AKFIN,query)
# long.df<-data.frame(Year = thed$YEAR,
#                     Target = thed$TRIP_TARGET_NAME,
#                     Bycatch = thed$NONTARGET_GROUP_NAME,
#                     Bycatch_Weight = thed$NONTARGET_ESTIMATE_WEIGHT)
# all.df<-aggregate(data= long.df,Bycatch_Weight ~ Year + Bycatch,FUN = sum)
# all.df<-all.df[complete.cases(all.df$Year) & complete.cases(all.df$Bycatch_Weight),]
# subd<-thed[thed$TRIP_TARGET_NAME==myspecies,]
# long.species.df<-data.frame(Year = subd$YEAR,
#                             Bycatch = subd$NONTARGET_GROUP_NAME,
#                             Bycatch_Weight_Sp = subd$NONTARGET_ESTIMATE_WEIGHT)
# species.df<-aggregate(data = long.species.df,Bycatch_Weight_Sp ~ Year + Bycatch,FUN = sum)
# species.df<-species.df[complete.cases(species.df$Year),]
# stuff.df<-merge(all.df,species.df,all = T)
# ## Contains % of bycatch for all non-target species for FHS. Drop
# ## those without any in any year
# stuff.df$Bycatch_Prop<-stuff.df$Bycatch_Weight_Sp/stuff.df$Bycatch_Weight
# stuff.df$Bycatch_Prop[is.na(stuff.df$Bycatch_Prop)]<-0
# Flip.df<-reshape2::dcast(data = stuff.df,Bycatch ~ Year,FUN = sum,var.value = Bycatch_Prop)
# ## Contains % of bycatch for all non-target species for FHS. Drop
# ## those without any in any year
# write.csv(Flip.df,'report/NonTarget_Bycatch_Table.csv', row.names=FALSE)
# ## Re-do for seabirds, NA if no trips at all caught them, 0 means
# ## none for FHS.
# ## !! This only works if there are literally no bycatch seabirds
# ## in FHS
# test <- filter(thed, grepl('Bird', NONTARGET_GROUP_NAME),
#                !is.na(NONTARGET_ESTIMATE_COUNT)) %>%
#   group_by(YEAR, NONTARGET_GROUP_NAME) %>%
#   ## Basically if there were some, put in a zero.
#   summarize(catch=0) %>%
#   pivot_wider(names_from=YEAR, values_from=catch) %>% arrange(NONTARGET_GROUP_NAME)
# write.csv(test, 'report/bycatch_birds.csv', row.names=FALSE)

##PSC and halibut mortality proportions
# message('Querying PSC bycatch from AKFIN...')
# query<-paste0("SELECT council.comprehensive_psc.year,\n ",
#               "council.comprehensive_psc.pscnq_estimate,\n ",
#               "council.comprehensive_psc.halibut_mortality_tons,\n ",
#               "council.comprehensive_psc.fmp_area,\n ",
#               "council.comprehensive_psc.trip_target_name,\n ",
#               "council.comprehensive_psc.species_group_name\n ",
#               "FROM council.comprehensive_psc\n ",
#               "WHERE council.comprehensive_psc.year > ",StartYr,"\n ",
#               "    AND   council.comprehensive_psc.fmp_area = ",FMParea)
# thePSC<-sqlQuery(AKFIN,query)
# 
# All.PSC.df<-aggregate(data=thePSC,PSCNQ_ESTIMATE ~ YEAR + SPECIES_GROUP_NAME,FUN=sum)
# spPSC<-thePSC[thePSC$TRIP_TARGET_NAME==myspecies,]
# Sp.PSC.df<-aggregate(data=spPSC,PSCNQ_ESTIMATE ~ YEAR + SPECIES_GROUP_NAME,FUN=sum)
# names(Sp.PSC.df)[names(Sp.PSC.df) == 'PSCNQ_ESTIMATE'] <- 'Sp_PSC'
# things.df<-merge(All.PSC.df,Sp.PSC.df,all = T)
# things.df$PSC_Prop<-things.df$Sp_PSC/things.df$PSCNQ_ESTIMATE
# Flip.PSC.df<-reshape2::dcast(data = things.df,SPECIES_GROUP_NAME ~ YEAR,FUN = sum,var.value = PSC_Prop)
# write.csv(Flip.PSC.df,"report/PSCNQ_table.csv", row.names=FALSE)
# ## Special halibut table
# thePSC.Hal<-thePSC[thePSC$SPECIES_GROUP_NAME=="Halibut",]
# All.Hal.df<-aggregate(data=thePSC.Hal,HALIBUT_MORTALITY_TONS ~ YEAR + SPECIES_GROUP_NAME,FUN=sum)
# thePSC.Hal.Sp<-thePSC.Hal[thePSC.Hal$TRIP_TARGET_NAME==myspecies,]
# Sp.Hal.df<-aggregate(data=thePSC.Hal.Sp,HALIBUT_MORTALITY_TONS ~ YEAR +SPECIES_GROUP_NAME,FUN=sum)
# names(Sp.Hal.df)[names(Sp.Hal.df) == 'HALIBUT_MORTALITY_TONS'] <- 'Sp_Hal'
# Hal.df<-merge(All.Hal.df,Sp.Hal.df,all = T)
# Hal.df$Prop_Hal<-Hal.df$Sp_Hal/Hal.df$HALIBUT_MORTALITY_TONS
# Halibut.df<-data.frame(Year = Hal.df$YEAR,
#                        Species_Halibut_Mortality_Tons = Hal.df$Sp_Hal,
#                        Prop_Halibut_Mortality = Hal.df$Prop_Hal)
# write.csv(Halibut.df,"report/halibut_PSC_table.csv", row.names=FALSE)


# warning("The PSC queries need to be checked")

### Close connections to prevent a warning when sourced
# message("Closing ODBC connections..")
# odbcCloseAll()

# 2020 Notes from CCM ----
## To do: The processing of the fishery ages and lengths is still
## a mess and needs to be cleaned up. I still need to remove the
## dependency on the newsbss repo and make this self contained. I
## also would really like to understand better why so many
## queries are needed to get the survey ages/lengths. Also plots
## of differences between data input files for the different data
## types.


#### Data retrieval file

### Inputs: Raw data files pulled from either ODBC or Dropbox
### flatfiles.

### Outputs: Flatfiles representing the raw data file at time of
### running this script, saved into 'data' folder for
### transparency and reproducibility. No processing is done, see
### inputs.R.

### Started 2020 by Cole
