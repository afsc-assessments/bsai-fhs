### Cole took file Get_Fishery_Length_HaulStats_With_NORPAC.R and
### modified for use here. 9/2020

if(gear!='nonpelagic') stop("not set up for a gear other than nonpelagic")
if(with_unsexed) stop("not set up to use unsexed fish")

### --------------------------------------------------
### Domestic fleet
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
haulsd <- read.csv('data/hauls_fishery_domestic.csv')
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
#### --- Foreign fleet
haulsf <- read.csv('data/hauls_fishery_foreign.csv')
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
write.csv(all_hauls, file='report/lcomp_fishery_hauls.csv', row.names=FALSE)
### This was copied from
### Get_Fishery_Lengths_With_Extrapolated_Number.R and
### modified. Calculate the actual length compositions from
### extrapolated numbers. Cole 9/2020
## Redone in tidyverse


lenf <- readRDS('data/lengths_fishery_foreign.RDS') %>%
  rename(LENGTH=SIZE_GROUP,
         EXTRAPOLATED_NUMBER=SPECIES_HAUL_NUMBER,
         EXTRAPOLATED_WEIGHT=SPECIES_HAUL_WEIGHT) %>%
  group_by(HAUL_JOIN) %>% mutate(total=sum(FREQUENCY))
## Calcualte proportinos
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

lend <- readRDS('data/lengths_fishery_domestic.RDS') %>%
  group_by(ID) %>% mutate(total=sum(FREQUENCY))
## Calcualte proportinos
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
names(SS_lcomps_fishery) <- names(SS_dat$lencomp)
write.csv(SS_lcomps_fishery, file = "inputs/SS_lcomp_fishery.csv", row.names=FALSE)





