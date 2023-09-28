## code to extrapolate catches for future years
## based on C. Monnohan's file from the 2020 update entitled inputs.R & reports.R

library(dplyr)
# library(tidyr)
library(ggplot2)
require(here)
require(lubridate)



### ----- Catch from AKFIN -----
message("Processing catch data...")
BSAIt <- read.csv(here('data',"2021-10-29-catch.csv")) 
## Type= (R)etained or (D)iscarded
g <- BSAIt %>% group_by(YEAR, ZONE, TYPE, GEAR) %>%
  summarize(TONS=sum(TONS), .groups='drop') %>%
  ggplot(aes(YEAR, TONS, color=GEAR)) +
  geom_line() + facet_grid(ZONE~TYPE, scales='free') + scale_y_log10()
ggsave(g, file = here('figs',paste0(Sys.Date(),'-catch_by_gear_zone.png')))


## Aggregate catch to just year
annual_catch <- BSAIt %>%
  group_by(YEAR) %>% 
  summarize(total=sum(TONS), .groups='drop') %>% 
  data.frame()
## Build SS structure
SS_catch <- data.frame(year=annual_catch$YEAR, seas=1, fleet=1,
                       catch=annual_catch$total, catch_se=.01)
write.csv(x=SS_catch, file=here('data',paste0(Sys.Date(),'-SS_catch.csv') ), row.names=FALSE)

## for table - catch by area in 2021
# tmp0 <- BSAIt %>% 
#   # filter(year==this_year) %>%
#   filter(YEAR > 2015) 
# 
# tmp0$ZONE <- ifelse(tmp0$ZONE =="EY"|tmp0$ZONE == "SE" |tmp0$ZONE == "WY",
#                     'EGOA',tmp0$ZONE)
# 
# tmp0 %>% group_by(YEAR,ZONE) %>%
#   summarise(sc=sum(TONS)) %>%
#   tidyr::pivot_wider(., id_cols = YEAR,
#                      names_from = ZONE, 
#                      values_from = sc) %>%
#   mutate(total = CG+EGOA+WG) %>%
#   select(YEAR,total, WG, CG, EGOA)



## Inputs for Projections/spp_catch.dat ----

#* catches 2015-2020 ----
round(tail(SS_catch)) 

#* catches 2021 ----
this_year= 2021
# files <- list.files(here('data','weekly_catches'), full.names=TRUE)
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
message("Precited ", this_year, " catch= ", round(catch_this_year + catch_to_add,0)) ##9272
## message('Predicted ',this_year, ' catch= ', round(catch_this_year/avg_proportion,0))
##

#* catches 2022/2023 ----
##  use last 5 years' real data average
SS_catch %>% 
  filter(year  < 2021& year  > 2015) %>% 
  summarise(mean(catch )) ## 11140

