## old fishery lcomp routine
## combination of scripts in 2020 get_lcomps_fishery and process_lcomps_fishery
## presets from data.r
library(RODBC)
library(rstudioapi)
library(dplyr)
library(tidyr)
library(ggplot2)

## setup ----
## must be on VPN WEST
username_AFSC <- showPrompt(title="Username", message="Enter your AFSC username:", default="")
password_AFSC <- askForPassword(prompt="Enter your AFSC password:")
AFSC <- odbcConnect("AFSC",username_AFSC,password_AFSC,  believeNRows = FALSE)

username_AKFIN <- showPrompt(title="Username", message="Enter your AKFIN username:", default="")
password_AKFIN <- askForPassword(prompt="Enter your AKFIN password:")
AKFIN <- odbcConnect("AKFIN",username_AKFIN,password_AKFIN,  believeNRows = FALSE)


source("C:/USERS/MAIA.KAPUR/WORK/ASSESSMENTS/NEWSBSS/FUNCTIONS/BIN_LEN_DATA.R")
source("C:/USERS/MAIA.KAPUR/WORK/ASSESSMENTS/NEWSBSS/FUNCTIONS/BIN_AGE_DATA.R")
## Flathead sole only = 103, Rex = 105, Dover = 107, Bering flounder = 145, GT = 102, Deepsea sole = 110
## Setup options for flathead sole in BSAI
final_year <- 2024
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
gear <- "nonpelagic"
with_unsexed <- FALSE
## NMFS_AREA between 500 and 544 for BSAI, above 600 for GOA
species <- 103
low.nmfs.area <- "'500'" #"'600'" #500
hi.nmfs.area <- "'544'" #"'699'"  #544
SpeciesCode = "103"
## Potentially unbury the choices for FmpArea for max flexibility.
FmpArea <- "'BS'" #Options are 'AI' = 539-544 'GOA' = 600 to 699 'BS' = 500 to 539



## sql download ----
#* domestic hauls----
message("Querying haul info for domestic fishery...")
lcompq <- paste0(
  "SELECT OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN,\n ",
  "OBSINT.DEBRIEFED_LENGTH.SPECIES,\n ",
  "OBSINT.DEBRIEFED_LENGTH.SEX,\n ",
  "OBSINT.DEBRIEFED_LENGTH.LENGTH,\n ",
  "OBSINT.DEBRIEFED_LENGTH.FREQUENCY,\n ",
  "OBSINT.DEBRIEFED_LENGTH.YEAR,\n ",
  "OBSINT.DEBRIEFED_LENGTH.GEAR,\n ",
  "OBSINT.DEBRIEFED_LENGTH.NMFS_AREA,\n ",
  "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),9,19) AS LAST1,\n ",
  "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),1,8) AS FIRST1\n ",
  "FROM OBSINT.DEBRIEFED_LENGTH\n ",
  "INNER JOIN OBSINT.DEBRIEFED_HAUL\n ",
  "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN    = OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN\n ",
  "WHERE OBSINT.DEBRIEFED_LENGTH.SPECIES = ",species,"\n ",
  "AND OBSINT.DEBRIEFED_LENGTH.NMFS_AREA BETWEEN ",low.nmfs.area," AND ",hi.nmfs.area)
hauls_fishery_domestic <- sqlQuery(AFSC,lcompq)
if(!is.data.frame(hauls_fishery_domestic)) stop("Query failed")
hauls_fishery_domestic$ID <- paste0(hauls_fishery_domestic$FIRST1, hauls_fishery_domestic$LAST1)
hauls_fishery_domestic <- select(hauls_fishery_domestic, -FIRST1, LAST1)
write.csv(here::here(year,'data','raw','hauls_fishery_domestic.csv'), x=hauls_fishery_domestic, row.names=FALSE)
#* foreign hauls----
message("Querying haul info for foreign fishery...")
## NORPAQ number of hauls and individuals: Add NORPAQ query for
## foreign hauls (don't think you can split by
## nonpelagic/pelagic) here the FOREIGN_HAUL table needs to be
## joined to the FOREIGN_LENGTH table because foreign haul has
## info on the nmfs areas (called GENERIC_AREA) where fishing
## occurred and the length table is missing that.
lcompq <-paste0("SELECT NORPAC.FOREIGN_LENGTH.SPECIES,\n ",
                "NORPAC.FOREIGN_LENGTH.SEX,\n ",
                "NORPAC.FOREIGN_LENGTH.YEAR,\n ",
                "NORPAC.FOREIGN_LENGTH.FREQUENCY,\n ",
                "NORPAC.FOREIGN_HAUL.GENERIC_AREA,\n ",
                "NORPAC.FOREIGN_LENGTH.SIZE_GROUP,\n ",
                "NORPAC.FOREIGN_HAUL.HAUL_JOIN,\n ",
                "NORPAC.FOREIGN_HAUL.HOOKS_PER_SKATE,\n ",
                "NORPAC.FOREIGN_HAUL.NUMBER_OF_POTS\n ",
                "FROM NORPAC.FOREIGN_LENGTH\n ",
                "INNER JOIN NORPAC.FOREIGN_HAUL\n ",
                "ON NORPAC.FOREIGN_HAUL.HAUL_JOIN    = NORPAC.FOREIGN_LENGTH.HAUL_JOIN\n ",
                "WHERE NORPAC.FOREIGN_LENGTH.SPECIES = ",species,"\n ",
                "AND NORPAC.FOREIGN_HAUL.GENERIC_AREA BETWEEN ",low.nmfs.area," AND ",hi.nmfs.area)
hauls_fishery_foreign <- sqlQuery(AFSC, lcompq)
hauls_fishery_foreign <- hauls_fishery_foreign[hauls_fishery_foreign$GENERIC_AREA != 670,]
hauls_fishery_foreign$ID <-hauls_fishery_foreign$HAUL_JOIN
write.csv(here::here(year,'data','raw','hauls_fishery_foreign.csv'), x=hauls_fishery_foreign, row.names=FALSE)

#* domestic lengths----
## Go query domestic (DLCOMP) and foreign (FLCOMP) fishery length
## information from OBSINT and NORPAQ Using extrapolated number
## (and the equivalent from foreign data), calculate
## population-level length comps in terms of numbers return a
## dataframe with those comps (comps) as well as the total
## population numbers summed over males, females, and length bins
## for later use (totals)

message('Querying domestic fishery lengths..')
if(FmpArea=="'AI'") region<-"539 and 544"
if(FmpArea=="'GOA'") region<-"600 and 699"
if(FmpArea=="'BS'") region<-"500 and 539"
test <- paste0("SELECT OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN,\n ",
               "OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN AS HAUL_JOIN1,\n ",
               "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT,\n ",
               "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_NUMBER,\n ",
               "OBSINT.DEBRIEFED_SPCOMP.YEAR,\n ",
               "OBSINT.DEBRIEFED_SPCOMP.SPECIES,\n ",
               "OBSINT.DEBRIEFED_LENGTH.GEAR,\n ",
               "OBSINT.DEBRIEFED_LENGTH.NMFS_AREA,\n ",
               "OBSINT.DEBRIEFED_LENGTH.SEX AS SEX,\n ",
               "OBSINT.DEBRIEFED_LENGTH.LENGTH,\n ",
               "OBSINT.DEBRIEFED_LENGTH.FREQUENCY,\n ",
               "OBSINT.DEBRIEFED_LENGTH.VESSEL_TYPE,\n ",
               "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),9,19) AS LAST1,\n ",
               "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),1,8) AS FIRST1\n ",
               "FROM OBSINT.DEBRIEFED_SPCOMP\n ",
               "INNER JOIN OBSINT.DEBRIEFED_LENGTH\n ",
               "ON OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN    = OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN\n ",
               "WHERE OBSINT.DEBRIEFED_SPCOMP.SPECIES   = ", fsh_sp_str, "\n ",
               "AND OBSINT.DEBRIEFED_LENGTH.NMFS_AREA BETWEEN ",region,"\n ",
               "AND OBSINT.DEBRIEFED_LENGTH.SPECIES in (",fsh_sp_str,")\n ",
               "ORDER BY OBSINT.DEBRIEFED_LENGTH.YEAR")
lengths_dom <- sqlQuery(AFSC,test)
if(!is.data.frame(lengths_dom)) stop('Failed query')
## Note that in 2020 the ID had NAs in it.. does that break it?
lengths_dom$ID <- with(lengths_dom, paste0(FIRST1,LAST1))
saveRDS(lengths_dom, file=here::here(year,'data','raw','lengths_fishery_domestic.RDS'))
#* foreign lengths----
message('Querying foreign fishery lengths..')
test <- paste("SELECT NORPAC.FOREIGN_LENGTH.HAUL_JOIN,\n ",
              "NORPAC.FOREIGN_LENGTH.SPECIES,\n ",
              "NORPAC.FOREIGN_LENGTH.SEX,\n ",
              "NORPAC.FOREIGN_LENGTH.SIZE_GROUP,\n ",
              "NORPAC.FOREIGN_LENGTH.YEAR,\n ",
              "NORPAC.FOREIGN_LENGTH.FREQUENCY,\n ",
              "NORPAC.FOREIGN_SPCOMP.SPECIES_HAUL_NUMBER,\n ",
              "NORPAC.FOREIGN_SPCOMP.SPECIES_HAUL_WEIGHT,\n ",
              "NORPAC.FOREIGN_HAUL.GENERIC_AREA,\n ",
              "NORPAC.FOREIGN_HAUL.HOOKS_PER_SKATE,\n ",
              "NORPAC.FOREIGN_HAUL.NUMBER_OF_POTS\n ",
              "FROM NORPAC.FOREIGN_HAUL\n ",
              "INNER JOIN NORPAC.FOREIGN_LENGTH\n ",
              "ON NORPAC.FOREIGN_HAUL.HAUL_JOIN = NORPAC.FOREIGN_LENGTH.HAUL_JOIN\n ",
              "INNER JOIN NORPAC.FOREIGN_SPCOMP\n ",
              "ON NORPAC.FOREIGN_HAUL.HAUL_JOIN    = NORPAC.FOREIGN_SPCOMP.HAUL_JOIN\n ",
              "WHERE NORPAC.FOREIGN_LENGTH.SPECIES in (",fsh_sp_str,")\n ",
              "AND NORPAC.FOREIGN_HAUL.GENERIC_AREA between ",region,sep="")
lengths_fishfor <- sqlQuery(AFSC,test)
lengths_fishfor$GEAR <- 1
lengths_fishfor$GEAR[lengths_fishfor$HOOKS_PER_SKATE > 0] <- 8
lengths_fishfor$GEAR[lengths_fishfor$NUMBER_OF_POTS > 0] <- 6
lengths_fishfor <- select(lengths_fishfor, -HOOKS_PER_SKATE,
                          -NUMBER_OF_POTS)
saveRDS(lengths_fishfor, file=here::here(year,'data','raw','lengths_fishery_foreign.RDS'))

## process fishery lcomps ----

### Cole took file Get_Fishery_Length_HaulStats_With_NORPAC.R and
### modified for use here. 9/2020

if(gear!='nonpelagic') stop("not set up for a gear other than nonpelagic")
if(with_unsexed) stop("not set up to use unsexed fish")


#* process hauls ----

#**  Domestic fleet----
## This file produces the total number of hauls and number of
## individuals in the length data for the fishery.  Made by Carey
## in 2015 for the GOA species (and based on calcs that were done
## in 2014 for BSAI flathead) Can be run for non-pelagic gear
## only or pelagic gear only, or all gears (for the domestic
## number of haulss) There is no gear delineation in the foreign
## data query This should not matter for GOA Rex because nearly
## all the data are non-pelagic trawl NORPAQ gear type does not
## appear to be recorded in NORPAQ.FOREIGN_LENGTH or
## NORPAQ.FOREIGN_HAUL.  The number of hauls from this code may
## need to be combined with PortStats.R for number of tows that
## come from port data, if using port data.
haulsd <- read.csv(here::here(year,'data','raw','hauls_fishery_domestic.csv'))
## ID replaces HAUL_JOIN because HAUL_JOIN is too long to read
## into R correctly.
if(!with_unsexed) haulsd <- haulsd[haulsd$SEX!="U",]
if(gear=="nonpelagic") haulsd <- haulsd[haulsd$GEAR==1,]
## if(gear=="pelagic") haulsd <- haulsd[haulsd$GEAR==2,]
hauls.domestic.df <- rename(haulsd, year=YEAR) %>% group_by(year) %>%
  summarize(nhauls_tot=length(unique(ID)),
            ninds_tot=sum(FREQUENCY),
            nhauls_female=length(unique(ID[SEX=="F"])),
            ninds_female=sum(FREQUENCY[SEX=="F"]),
            nhauls_male=length(unique(ID[SEX=="M"])),
            ninds_male=sum(FREQUENCY[SEX=="M"]), .groups='drop')

#**  foreign fleet----
haulsf <- read.csv(here::here(year,'data','raw','hauls_fishery_foreign.csv'))
## sort out gears:
haulsf$GEAR <- 1
haulsf$GEAR[haulsf$HOOKS_PER_SKATE > 0] <- 8
haulsf$GEAR[haulsf$NUMBER_OF_POTS > 0] <- 6
if(!with_unsexed) haulsf <- haulsf[haulsf$SEX!="U",]
hauls.foreign.df <- rename(haulsf, year=YEAR) %>% group_by(year) %>%
  summarize(nhauls_tot=length(unique(ID)),
            ninds_tot=sum(FREQUENCY),
            nhauls_female=length(unique(ID[SEX=="F"])),
            ninds_female=sum(FREQUENCY[SEX=="F"]),
            nhauls_male=length(unique(ID[SEX=="M"])),
            ninds_male=sum(FREQUENCY[SEX=="M"]), .groups='drop')
## Final dataframe of # hauls for both domestic and foreign fleets
all_hauls <- rbind(hauls.foreign.df, hauls.domestic.df) %>%
  group_by(year) %>% summarize_all('sum') %>% arrange(year)


message("Writing fishery length comp haul table input to report folder...")
write.csv(all_hauls, file=here::here(year,'data','output','lcomp_fishery_hauls.csv'), row.names=FALSE)

#* process length comps ----
#** domestic lengths ----
lend <- readRDS(here::here(year,'data','raw','lengths_fishery_domestic.RDS')) %>%
  group_by(ID) %>% 
  mutate(total=sum(FREQUENCY))

## Calculate proportions
lend <- BIN_LEN_DATA(lend, len_bins=len_bins) %>%
  mutate(LENGTH=BIN, PROP=FREQUENCY/total,
         LNUMBERS=PROP*EXTRAPOLATED_NUMBER) %>%
  filter(!is.na(LNUMBERS)) %>%  select(-BIN, -total)
## Sum over duplicates
lend <- group_by(lend, YEAR, LENGTH, SEX, GEAR) %>%
  summarize(LNUMBERS=sum(LNUMBERS), .groups='drop') %>%
  arrange(GEAR, YEAR, SEX, LENGTH)
## Add missing combinations
grid <- expand.grid(LENGTH=len_bins,
                    YEAR=unique(lend$YEAR),
                    SEX=unique(lend$SEX),
                    GEAR=unique(lend$GEAR))
DLCOMP <- merge(lend, grid, all=TRUE)
DLCOMP$LNUMBERS[is.na(DLCOMP$LNUMBERS)] <- 0


#** foreign lengths ----

### This was copied from
### Get_Fishery_Lengths_With_Extrapolated_Number.R and
### modified. Calculate the actual length compositions from
### extrapolated numbers. Cole 9/2020
## Redone in tidyverse
lenf <- readRDS(here::here(year,'data','raw','lengths_fishery_foreign.RDS')) %>%
  rename(LENGTH=SIZE_GROUP,
         EXTRAPOLATED_NUMBER=SPECIES_HAUL_NUMBER,
         EXTRAPOLATED_WEIGHT=SPECIES_HAUL_WEIGHT) %>%
  group_by(HAUL_JOIN) %>% mutate(total=sum(FREQUENCY))

## Calculate proportions
lenf <- BIN_LEN_DATA(lenf, len_bins=len_bins) %>%
  mutate(LENGTH=BIN, PROP=FREQUENCY/total,
         LNUMBERS=PROP*EXTRAPOLATED_NUMBER) %>%
  filter(!is.na(LNUMBERS)) %>%  select(-BIN, -total)
## Sum over duplicates
lenf <- group_by(lenf, YEAR, LENGTH, SEX, GEAR) %>%
  summarize(LNUMBERS=sum(LNUMBERS), .groups='drop') %>%
  arrange(GEAR, YEAR, SEX, LENGTH)
## Add missing combinations
grid <- expand.grid(LENGTH=len_bins,
                    YEAR=unique(lenf$YEAR),
                    SEX=unique(lenf$SEX),
                    GEAR=unique(lenf$GEAR))
FLCOMP <- merge(lenf, grid, all=TRUE)
FLCOMP$LNUMBERS[is.na(FLCOMP$LNUMBERS)] <- 0

#** combine into ss3  comps ----

## Sum over duplicated combinations while pivoting SEX wider
lcomps <- rbind(FLCOMP, DLCOMP) %>%
  ## This replaces the old aggregate and cast at the same time
  pivot_wider(names_from=SEX, values_from=LNUMBERS, values_fn=sum) %>%
  ## Drop unsexed column. Code not setup to use it anymore
  select(-U)
## Calculate proportions
lcomps <- lcomps %>% group_by(YEAR, GEAR) %>%
  ## Divide by totals to get proportions for each year:gear combo
  mutate(total=sum(F+M), Fprop=F/total, Mprop=M/total) %>%
  as.data.frame() %>% arrange(YEAR,GEAR, LENGTH) %>%
  filter(total>0) # some totals=0 cause NaNs
## Create male and female wide data.frames
lcomps_f <- lcomps %>%
  pivot_wider(id_cols=c('YEAR', 'GEAR'), values_fn=sum,
              names_from=LENGTH, values_from='Fprop')
lcomps_m <- lcomps %>%
  pivot_wider(id_cols=c('YEAR', 'GEAR'), values_fn=sum,
              names_from=LENGTH, values_from='Mprop')
lcomps <- cbind(lcomps_f, lcomps_m[,-c(1,2)])

## Add Nsamp which comes from the hauls data created above
lcomps <- merge(y=lcomps, x=all_hauls[,c('year', 'nhauls_tot')],
                by.y='YEAR', by.x='year', all=TRUE)
## Checks
if(!all.equal(rowSums(lcomps[,-(1:3)]), rep(1, times=nrow(lcomps))))
  stop("Rows did not sum to zero in lcomps, something went wront")
if(!all(!is.na(lcomps$nhauls_tot)))
  stop("Something wrong with using hauls for Nsamp")

### Build SS structure
## Code only setup to use non-pelagic gear so filter
lcomps <- lcomps[lcomps$GEAR==1,]
SS_lcomps_fishery <-
  data.frame(year=lcomps$year, month=7, fleet=1,
             sex=3, part=0, Nsamp=lcomps$nhauls_tot,
             lcomps[,-(1:3)])

mod_2020_dat <- r4ss::SS_readdat(here::here(year, 'mgmt','18.2c_2020','2020_bsai_fhs.dat'))

names(SS_lcomps_fishery) <- names(mod_2020_dat$lencomp)

## turn off lengths in years with marginal ages
offyrs <- mod_2020_dat$lencomp$Yr[which(mod_2020_dat$lencomp$FltSvy==-1)]
SS_lcomps_fishery$FltSvy[SS_lcomps_fishery$Yr %in% offyrs] <- -1


write.csv(SS_lcomps_fishery, 
          file = here::here(year,'data','output',"fsh_len_ss3-oldmethod.csv"), row.names=FALSE)
          
## compare to previous data pull

comparedf <- SS_lcomps_fishery %>% mutate(src = 'newpull') %>%
  bind_rows(., mod_2020_dat$lencomp%>% mutate(src = 'oldpull')) %>%
  filter(FltSvy!=2)

## identical inputN and off years
ggplot(comparedf, aes(x =Yr, y = Nsamp, fill = src )) +
  geom_bar(stat = 'identity',position = 'dodge') +
  geom_bar(data = subset(comparedf, FltSvy == -1), fill = 'red',stat = 'identity',position = 'dodge') +
  scale_fill_manual(values = c('grey22','grey77')) +
  theme_minimal()

comparedf %>%
  select(-Seas,-FltSvy,-Gender,-Part,-Nsamp) %>%
  reshape2::melt(id = c('Yr','src')) %>%
  mutate(variable =as.character(variable), value = as.numeric(value)) %>%
  mutate(sex = substr(variable,1,1)) %>%
  mutate(across(c('variable'), substr, 2, nchar(variable))) %>%
  filter(Yr >= 2010) %>%
ggplot(., aes(x =variable, y = value, color = src, group = src )) +
  geom_line() +
  facet_grid(sex~Yr)+
  # geom_bar(stat = 'identity',position = 'dodge') +
  scale_color_manual(values = c('grey22','grey77')) +
  theme_void()


comparedf %>%
  select(-Seas,-FltSvy,-Gender,-Part,-Nsamp) %>%
  reshape2::melt(id = c('Yr','src')) %>%
  mutate(variable =as.character(variable), value = as.numeric(value)) %>%
  mutate(sex = substr(variable,1,1)) %>%
  mutate(across(c('variable'), substr, 2, nchar(variable))) %>%
  filter(Yr >= 2000 & Yr < 2010) %>%
  ggplot(., aes(x =variable, y = value, color = src, group = src )) +
  geom_line() +
  facet_grid(sex~Yr)+
  # geom_bar(stat = 'identity',position = 'dodge') +
  scale_color_manual(values = c('grey22','grey77')) +
  theme_void()

comparedf %>%
  select(-Seas,-FltSvy,-Gender,-Part,-Nsamp) %>%
  reshape2::melt(id = c('Yr','src')) %>%
  mutate(variable =as.character(variable), value = as.numeric(value)) %>%
  mutate(sex = substr(variable,1,1)) %>%
  mutate(across(c('variable'), substr, 2, nchar(variable))) %>%
  filter(Yr <= 2000) %>%
  ggplot(., aes(x =variable, y = value, color = src, group = src )) +
  geom_line() +
  facet_grid(sex~Yr)+
  # geom_bar(stat = 'identity',position = 'dodge') +
  scale_color_manual(values = c('grey22','grey77')) +
  theme_void()


          
          
          
          
          
          
          