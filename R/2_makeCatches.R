## code to extrapolate catches for future years
## based on C. Monnohan's file from the 2020 update entitled inputs.R & reports.R
library(dplyr)
# library(tidyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
theme_update(legend.text = element_text(size = 14), legend.position = 'bottom',
                     axis.text = element_text(size = 14),axis.title  = element_text(size = 14))
require(here)
require(lubridate)

this_year = lubridate::year(Sys.Date())
last_yr = this_year-1
date_use <- "2022-05-31" ## dwnld date
data_folder = here('data','/')
## Summarise and plot catches by discard/retention/geartype
gearpal = c('grey22',
            alpha('grey22',0.65), 
            'goldenrod',
            alpha('goldenrod',0.65),
            'seagreen4',
            alpha('seagreen4',0.65)) 



BSAIt <- read.csv(paste0(data_folder,date_use,"-catch.csv")) 
BSAIt %>% 
  group_by(YEAR, ZONE, TYPE, GEAR) %>%
  summarize(TONS=sum(TONS), .groups='drop') %>%
  mutate(lab = paste0(GEAR, ifelse(TYPE == 'D',' Discarded', ' Retained'))) %>%
  ggplot(aes(YEAR, TONS, color=lab)) +
  scale_color_manual(values = gearpal)+
  geom_line() + 
  labs(x = 'Year', y= 'Catch (tons)', col = '')+ 
  facet_wrap(GEAR~ZONE, scales = 'free', nrow = 3)  

ggsave(last_plot(), 
       file = here('figs',paste0(Sys.Date(),'-catch_by_gear_zone.png')))


## Aggregate catch to just year
annual_catch <- BSAIt %>%
  group_by(YEAR) %>% 
  summarize(total=sum(TONS), .groups='drop') %>% 
  data.frame()
## Build SS structure
SS_catch <- data.frame(year=annual_catch$YEAR, seas=1, fleet=1,
                       catch=annual_catch$total, catch_se=.01)
write.csv(x=SS_catch, file=here('data',paste0(Sys.Date(),'-SS_catch.csv') ), row.names=FALSE)

## for table - catch by area this year
tmp0 <- BSAIt %>%
  filter(YEAR > (this_year-6))
tmp0 %>% group_by(YEAR,ZONE) %>%
  summarise(sc=sum(TONS)) %>%
  tidyr::pivot_wider(., id_cols = YEAR,
                     names_from = ZONE,
                     values_from = sc) %>%
  mutate(total = AI+BS) %>%
  select(YEAR,total,AI, BS)  %>%
write.csv(., file=here('data',paste0(Sys.Date(),'-catch_by_zone') ), row.names=FALSE)

## Inputs for Projections/spp_catch.dat ----

#* catches 2015-2020 ----
round(tail(SS_catch)) 

#* catches this year ----

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
message("Predicted ", this_year, " catch= ", round(catch_this_year + catch_to_add,0)) ##9272

#* catches for projection years ----
##  use last 5 years' real data average
projc <- SS_catch %>% 
  filter(year  < this_year & year  > (this_year-6)) %>% 
  summarise(mean(catch )) %>% as.numeric()

catchvec = matrix(c((last_yr-1),
                  last_yr,
                  this_year:(this_year+2),
                  SS_catch$catch[SS_catch$year == last_yr-1],
                  SS_catch$catch[SS_catch$year == last_yr],
       round(catch_this_year + catch_to_add,0),
         projc,
         projc), ncol = 2)

save(catchvec,file = here('data', paste0(Sys.Date(),"-catches_for_proj.rdata")))
