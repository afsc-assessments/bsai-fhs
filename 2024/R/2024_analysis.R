# 2024 BSAI Flathead Sole Analysis
# maia.kapur@noaa.gov

# load ----
## do NOT update odbc or connect() won't work
## need odbc v 1.3.5 https://github.com/r-dbi/odbc/archive/refs/tags/v1.3.5.tar.gz
# install.packages("https://github.com/r-dbi/odbc/archive/refs/tags/v1.3.5.tar.gz", repos=NULL, type='source')
# devtools::install_github("afsc-assessments/afscdata", force = TRUE) 
# devtools::install_github("afsc-gap-products/gapindex")
# devtools::install_github("BenWilliams-NOAA/afscassess", force = TRUE)
# devtools::install_github('r4ss/r4ss')
# devtools::install_github("afsc-assessments/afscISS")

library(gapindex)
library(afscISS)
library(afscdata)
library(afscassess)
library(r4ss)
library(dplyr)
library(here)
library(purrr)
library(ggplot2)
library(afscOSA)
theme_set(afscassess::theme_report())

# globals ----
year <- this_year <-  2024
rec_age = 0 ## this is default for SS3
plus_age = 21
ages = rec_age:plus_age
lengths = c(seq(6,40,2),seq(43,58,3))
TAC = c(25000, 25000, 35500) # 2021, 2022, 2023
species = "FSOL"
curr_mdl_fldr = "18.2c_2024"
prev_mdl_fldr = "18.2c_2020"  
mod_path <- here::here(year,'mgmt',curr_mdl_fldr)
# query data ----
## you must be on the VPN (West) for this to work, and it takes about 5 minutes
## this automates the AI interpolation for the biomass survey and outputs in in SS format

## run this if you haven't yet
# afscdata::setup_folders()

afscdata::bsai_fhs(year)
source(here::here(year,'r','bsai_fhs_wrangle_data.R'))

#* future catches to the end of the current year for spm.dat and SS ----
## MAKE SURE TO UPDATE THE CURRENT YEAR CATCH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## You will need to manually obtain this data and put it the following folder 'current_year/data/raw/weekly_catches'
## You will have to manually create the weekly catches folder in raw if it is not there.
## You will have to manually download all the weekly catch files from the current year to the current year - 5, if they are not already in the weekly_catches file 
## the latest data are available at: https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#bsai-groundfish
## scroll to BSAI Groundfish > catches by week > click on 2024 
weekly_catch_files <- list.files(here::here(year,'data','raw','weekly_catches'), 
                                 full.names=TRUE)
test <- lapply(1:length(weekly_catch_files), function(i){
  skip <- grep('ACCOUNT.NAME', readLines(weekly_catch_files[i]))-1
  data.frame(read.table(weekly_catch_files[i], skip=skip, header=TRUE, sep=',',
                        stringsAsFactors=FALSE))
})
weekly_catches <- do.call(rbind, test)
names(weekly_catches) <- c('species', 'date', 'catch')
weekly_catches <- weekly_catches %>%
  ## No species for Bering flounder, probably in FHS already
  filter(grepl("Flathead", x=species)) %>%
  mutate(date=lubridate::mdy(date), 
         week=lubridate::week(date),  
         year=lubridate::year(date))
this_year <- year
## catch so far this year
catch_this_year <- weekly_catches %>% 
  filter(year==this_year) %>%
  pull(catch) %>% 
  sum
## average catch last five years
average_catch <- weekly_catches %>% 
  filter(year %in% (this_year-5):this_year) %>%
  summarise(total_catch = sum(catch), .by = year) %>%
  summarise(mean(total_catch)) %>%
  as.numeric()

## Get average catch between now and end of year for previous 5 years
catch_to_add <- weekly_catches %>% 
  filter(year>=this_year-5 & week > lubridate::week(lubridate::today())) %>%
  group_by(year) %>% 
  summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Predicted ", this_year, " catch= ", round(catch_this_year+ catch_to_add,0)) 
stop("paste the Predicted catch into ss .dat file, and ensure spm.dat uses the same value for 2024")


# run Stock Synthesis ----
## You will need to do this manually outside of R. Manually copy the pulled data into the SS files and Run SS.
## Make sure to copy the current year catch into SS!!!!!!!!!!!!!!! when you do the final run!!!!!!!!!!!!!!!!!!!!
## make sure to run "hess_step" to minimize the gradient. Might need a "-" before.
## ss
## ss -hess_step

 
# run retrospective ----
## NOT DOING THIS FOR AN UPDATE

# run profiles ----
## NOT DOING THIS FOR AN UPDATE

# run projections ----
## takes less than one minute; only run this if model and/or projected catches change 

# Read in the functions in year/R/proj_functions folder
# lapply(list.files(here::here(year,'r',"proj_functions/"), full.names = T, pattern = ".r$"),  source) 
foos <- list.files(here::here(year,'r',"proj_functions/"))[grep(".R",list.files(here::here(year,'r',"proj_functions/")))]
walk(foos, ~source(here::here(year,'r',"proj_functions/",.x)))

# Write proj files ----
#* projection_data.dat ----
mod18.2c_2024 <- SS_output(here::here(year,'mgmt',curr_mdl_fldr), verbose = F)

## passed to write_projection_data_spm function
NSEX=2						# number of sexes used in assessment model
Nfishery=1					# number of fisheries(fleets) #This was set equal to 2
fleets=1					# fleet index number (associated with commercial fishery)
rec_age=3					# assumed age at recruitment
max_age=21					# maximum age in model
NAGE=length(rec_age:max_age)			# number of ages
# FY=1964 					# first year used to subset SSB, per memo this is always 1977 but was 1964 in prev assessments
# rec_FY=1964					# first year used to subset recruitment, per memo this is always 1977 but was 1964 in prev assessments
FY=1964 					# first year used to subset SSB, per memo this is always 1977, 1964 for consistency
rec_FY=1977					# first year used to subset recruitment
rec_LY_decrement=0				# value subtracted from assessment final year to subset recruitment vector
spawn_month=1					# spawning month
Fratios=1            				# Proportion F per fishery
#passed to write_proj_spcat
ct_yrs=3			#Number of future catch years given to projection model
## passed to setup function
nsims=1000			# number of projection model simulations
nproj=14			# number of projection years ALSO USED BY get_proj_res
## passed to get_proj_res
spp="BSAI_flathead"

## this will give you the general values, but mightn't be perfectly formatted
## YOU WILL NEED TO paste the right values into projection_data.dat in the "year/Model_runs/03b_projection" folder
## if you get an indexing error there is probably a duplication of the recruitment info
## towards the bottom; just delete it
## and ensure the catches are specified in spm.dat before re-running the projections
## Lee: this function is located in the "R/proj_functions". You might need to source this 
write_projection_data_spm(dir=here::here(year,'model_runs','03b_projection'),
           # sdir =x,
           data_file=paste0(Sys.Date(),"-projection_data.dat"),
           data= mod18.2c_2024,
           NSEX=NSEX, NAGE=NAGE, Nfishery=Nfishery,
           fleets=fleets, rec_age=rec_age, max_age=max_age, FY=FY,
           rec_FY=rec_FY, rec_LY_decrement=rec_LY_decrement,
           spawn_month=spawn_month, Fratios=Fratios)

## what to paste into bottom of spm.dat 
catch_proj <- cbind(year = c(this_year+(0:2)),
                    catch = c(round(catch_this_year+catch_to_add),
                              round(average_catch),
                              round(average_catch)))


setwd(here::here(year,'model_runs','03b_projection'))
shell('spm') ## so long as you see "finished simulations..." you can ignore error

rec_table1 <-
  read.table(here::here(year,'model_runs','03b_projection','percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% c(2025,2026) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  tidyr::pivot_wider(names_from=year, values_from=value)
rec_table1[3:6,3:4] <- rec_table1[3:6,3:4]

rec_table2 <-
  read.table(here::here(year,'model_runs','03b_projection','alt_proj.out'), header=TRUE) %>%
  filter(Year %in% c(2025,2026) & Alt==1) %>%
  tidyr::pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  tidyr::pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table2[,2:3] <- rec_table2[,2:3]
rec_table <- bind_rows(rec_table1, rec_table2)

## There's an error in spm.tpl where the sex ratio calcs didn't happen for the 
## biomass reference points. Here I'm manually dividing them by two.
rec_table[3:5,2:3]<-rec_table[3:5,2:3]/2


## change order to match SAFE format & magnitudes
rec_table <-rec_table[c(12,6,3,4,5,2,1,1,10,9,9),] 

# rec_table[c(1:5,9:11),2:3] <-formatC(rec_table[c(1:5,9:11),2:3] , format="d", big.mark=",") 
write.csv(rec_table, 
          file = here::here(year,'model_runs','03b_projection',paste0(Sys.Date(),'-exec_summ.csv')), row.names=FALSE)

file.copy(from = here::here(year,'model_runs','03b_projection',paste0(Sys.Date(),'-exec_summ.csv')),
          to = here::here('docs',year,paste0(Sys.Date(),'-exec_summ.csv')))
 
# render proj tables ----
## I'm still not sure whether these three tables ( 13-year projected: catches, spawning biomass, and fishing mortality rates corresponding to the alternative harvest scenarios
## ) will be required, but generating them here in case they are.

proj_scenario0 <- read.table(here::here(year,'model_runs','03b_projection','percentdb.out')) %>%
  filter(V4 %in% c("SSBMean","F_Mean","CMean")) %>%
  mutate(V5 = ifelse(V4 != 'F_Mean',round(V5,0),round(V5,3))) %>% ## convert to tons
  tidyr::pivot_wider(names_from =c(V2), 
                     names_prefix = 'Scenario ',
                     values_from = V5, id_cols = c(V3,V4))

proj_scenario0 %>%
  filter(V4 == 'CMean') %>% select(Year = V3,  everything(),-V4) %>%
  write.csv(.,here::here(year,'model_runs','03b_projection','proj_CMean.csv'),row.names = FALSE)
proj_scenario0 %>%
  filter(V4 == 'SSBMean') %>% select(Year = V3,  everything(),-V4)%>%
  write.csv(.,here::here(year,'model_runs','03b_projection','proj_SSBMean.csv'),row.names = FALSE)
proj_scenario0 %>%
  filter(V4 == 'F_Mean') %>% select(Year = V3,  everything(),-V4)%>%
  write.csv(.,here::here(year,'model_runs','03b_projection','proj_F_Mean.csv'),row.names = FALSE)  


# process results ----

mod18.2c_2024 <- r4ss::SS_output(mod_path, verbose = FALSE)

## get average recruitment after 1976; this is in a table caption 
## (open safe/11-tables-update.Rmd and look for label timeseriest)
meanrec_77 <- with(subset(mod18.2c_2024$timeseries, Yr > 1976 & Yr < 2025), mean(Recruit_0)) 
meanrec_all <- with(mod18.2c_2024$timeseries, mean(Recruit_0)) 

abs(meanrec_77-meanrec_all)/meanrec_all

#* save NAA as electronic file ----
write.csv(mod18.2c_2024$natage, file = here::here(mod_path,'natage.csv'), row.names = FALSE)

#* re-create SS-plots ----
##  ensure this folder gets named "plots/" or lookup won't work

SS_plots(mod18.2c_2024)

#* re-create comparison plots ----
## will save these in the plots/ folder made above
mod18.2c_2020 <- r4ss::SS_output(here::here(year,'mgmt','18.2c_2020'), verbose = FALSE)
SSplotComparisons(SSsummarize(biglist = list(mod18.2c_2020,mod18.2c_2024)),
                  legendlabels = c('18.2c (2020)', '18.2c (2024)'),
                  col = c('grey40','blue'),
                  png = TRUE,
                  plotdir = here::here(mod_path,'plots','compare'))

## need to manually make total biomass comparison plot
mod18.2c_2020$timeseries %>%
  select(Yr, Bio_smry) %>%
  mutate(mod = '18.2c (2020)') %>%
  rbind(., mod18.2c_2024$timeseries %>%
          select(Yr, Bio_smry) %>%
          mutate(mod = '18.2c (2024)')) %>%
  ggplot(., aes(x = Yr, y = Bio_smry/1000, color = mod)) +
  geom_line(lwd = 0.75) +
  theme(legend.position = c(0.85,0.9),
        legend.text  = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))+
  scale_y_continuous(limits = c(0,900), breaks = seq(0,800,100))+
  scale_x_continuous(limits = c(1960,2025), breaks = c(seq(1970,2020,10),2025))+
  scale_color_manual(values = c('grey22','blue')) +
  labs(x = 'Year', y = 'Total 3+ Biomass (x1000 t)', color = '')

ggsave(last_plot(), file = here::here(mod_path,'plots','compare',
                                      'compare18_totalbiomass.png'),
       width = 4, height = 3, dpi = 520, units = 'in')

ggsave(last_plot(), file = here::here('docs',year,'model_plots', 
                                      'compare18_totalbiomass.png'),
       width = 4, height = 3, dpi = 520, units = 'in')
# misc figures ----
#* growth, selectivity and maturity composite ----
 
##make the custom selex plots if needed
png(here::here(mod_path,"plots","growth_selex_maturity.png"), 
    width = 6, height =8, unit = 'in', res = 420) 
layout.matrix <- matrix(c(1,1,2,3,4,5), nrow = 3, ncol = 2, byrow = TRUE)

layout(mat = layout.matrix)
SSplotBiology(mod18.2c_2024, subplots = 1)
par(mar = c(0,0,1,1))
SSplotSelex(mod18.2c_2024, fleets  = 1,
            subplots = 3) 
 
SSplotSelex(mod18.2c_2024, fleets  = 2,
            subplots =2, agefactors = 'Asel' ) 

SSplotBiology(mod18.2c_2024, subplots = 6)
graphics.off()
 
#* timeseries_compare.png ----
## Time series with associated uncertainty confidence bounds of: 
## total biomass,  SB/ spawning output, stock depletion, and fishing mortality; 
## show previously accepted model
## list all comparison plots made above
filepaths <- list.files(here::here(mod_path, "plots","compare"), 
                        pattern = 'compare', full.names = TRUE)

## wrangle the three into a new image
png(here::here(mod_path,"plots","compare","timeseries_compare.png"), width = 12, height =8, unit = 'in', res = 420) 
rl = lapply(filepaths[c(12,13,15,19)], png::readPNG)
gl = lapply(rl, grid::rasterGrob) 
gridExtra::grid.arrange(grobs=gl,ncol = 2) 
dev.off()

#* survey map ---- 
# library(akgfmaps)
# SEBS <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")

#* catch vs abc ----
## (uses table made in section below)
catchabc <- read.csv(here::here(year,'safe','static_tables',
                                'catch_abc_tac_ofl_mgmt.csv')) %>%
  select(-Management.Measures) %>%
  reshape2::melt(id = 'Year') %>%
  mutate(value = as.numeric(gsub(',','',value)))

catchabc$variable <- factor(catchabc$variable, 
                            levels = c('Total','OFL','ABC','TAC'))

ggplot(catchabc, aes(x= Year, color = variable, fill = variable,y = value/1000)) +
  geom_bar(data=subset(catchabc, variable =='Total'),stat = 'identity',
           position = 'stack') +
  geom_line(data=subset(catchabc, variable !='Total')) +
  scale_color_manual(values = c('white','black','grey50','seagreen4'))+
  scale_fill_manual(values = rep('blue',4))+
  scale_x_continuous(limits = c(1995,year))+
  labs(x = 'Year', y = 'Value (1000 t)', color = '',fill = '') +
  theme(legend.position = 'top')

ggsave(last_plot(),
       file = here::here('docs',year,'model_plots','catch_abc_tac_ofl_mgmt.png'),
       width = 6, height =4 ,unit = 'in', dpi = 520)

#* copy into docs/ for presentation ----

file.copy(from = list.files(here::here(mod_path,'plots'), full.names = TRUE),
          to = here::here('docs','2024','model_plots'),
          overwrite = TRUE)

file.copy(from = list.files(here::here(mod_path,'plots','compare'), full.names = TRUE),
          to = here::here('docs','2024','model_plots'),
          overwrite = TRUE)

#* phase-plane plot ----
## take all reference values from Proj, noting that refs to "ofl" correspond to "b35" therein
 
## use proj values as denominators
rec_table1 <-
  read.table(here::here(year,'model_runs','03b_projection','percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), 
            year=as.numeric(V3), 
            metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year:(this_year+2)) & scenario==1 
         &  metric %in% c('SSBMean','SSBFofl', 'SSBFabc',
                          'SSBF100', 'Fofl', 'Fabc', 'F_Mean')
  ) %>%
  arrange(year, metric) %>%
  tidyr::pivot_wider(names_from=year, values_from=value)

b35 <- as.numeric(subset(rec_table1, metric == 'SSBFofl')[,'2025']) ## in mt
f35 <- as.numeric(subset(rec_table1, metric == 'Fofl')[,'2025'] )
b100 <- as.numeric(subset(rec_table1, metric == 'SSBF100')[,'2025']) ## in mt

pp_dat0 <- mod18.2c_2024$timeseries %>% 
  dplyr::select(Yr, SpawnBio, Fy = `F:_1`, catch = `obs_cat:_1`, totbio = Bio_smry) %>%
  filter(Yr > 1977  ) %>%
  mutate(SB_B35 = SpawnBio/(b35), F_F35 = Fy/f35, type = 'aa') %>%
  arrange(., Yr)

pp_dat <- pp_dat0 %>% select(-catch,-totbio)
## fill in yr, spawnbio, fy, b/b35, f/f35, type for proj years
## note that SS3 auto-populates yr 2025 but we want to overwrite it with proj outputs.

pp_dat[pp_dat$Yr == 2025,] <- c(2025, 
                              as.numeric(subset(rec_table1, metric == 'SSBMean')[,'2025']),
                              as.numeric(subset(rec_table1, metric == 'F_Mean')[,'2025']),
                              as.numeric(subset(rec_table1, metric == 'SSBMean')[,'2025']/b35),
                              as.numeric(subset(rec_table1, metric == 'F_Mean')[,'2025']/f35),
                              'aa') 

pp_dat <- bind_rows(pp_dat, c('Yr' = 2026, 
                            "SpawnBio" = as.numeric( subset(rec_table1, metric == 'SSBMean')[,'2026']),
                            'Fy' = as.numeric(subset(rec_table1, metric == 'F_Mean')[,'2026']),
                            "SB_B35"= as.numeric(subset(rec_table1, metric == 'SSBMean')[,'2026']/b35),
                            "F_F35"= as.numeric(subset(rec_table1, metric == 'F_Mean')[,'2026']/f35),
                            'type'= 'aa')) 



## make phase plane plot
ggplot(data = pp_dat, aes(x = as.numeric(SB_B35), 
                          y = as.numeric(F_F35),color = Yr)) +
  geom_hline(yintercept = 1, col = 'grey88') +  geom_vline(xintercept = 1, col = 'grey88') +
  geom_segment(data = NULL, aes(x =0.4/0.35,  y = 1,xend =1.5, yend = 1), color = 'red') + ## OFL plateau
  geom_segment(data = NULL, aes(x =0.05,  y = 0,xend =0.4/0.35, yend = 1), color = 'red') + ## OFL ramp
  geom_segment(data = NULL, aes(x = 0.4/0.35,  y = 0.8,xend =1.5, yend = 0.8), color = 'red', linetype = 'dotted') + ## ABC  plateau
  geom_segment(data = NULL, aes(x = 0.05,  y = 0,xend =0.4/0.35, yend = 0.8), color = 'red', linetype = 'dotted') + ## ABC  ramp
  geom_path(color = 'grey80', lwd = 0.75, aes(group = type)) +
  geom_point(data = subset(pp_dat, Yr %in% 1979:2024),  shape=16) +
  
  geom_point(data = subset(pp_dat, Yr == 1978), color = '#b5e48c', shape=5) +
  geom_point(data = subset(pp_dat, Yr == 2024), color = 'blue', shape=16) +
  geom_point(data = subset(pp_dat, Yr > 2024), color = '#ffc300') +
  
  ## year labels for high F years
  geom_text(data = subset(pp_dat, Yr %in% c(1978,1990,2008, 2024:2026)),
            vjust = c(-1,-1,-1,2,2,2),
            size = 2,
            aes(label = Yr), 
            color = "#184e77") +
  scale_color_manual(values = colorRampPalette(c("#b5e48c","#184e77"))(49))+
  scale_x_continuous(limits = c(0,1.5)) +
  scale_y_continuous(limits = c(0,1.5)) +
  theme(legend.position = 'none')+
  labs(x = expression('Spawning Biomass/B'[35]*"%"),y = expression('F/F'[35]*"%"))

ggsave(last_plot(),
       file =here::here(year,'mgmt', model, "plots", 'phase_plane.png'),
       width = 5, height = 5, dpi = 400)



#* catch/sb plot ----
ggplot(data = pp_dat0, aes(x = as.numeric(SpawnBio), 
                           y = as.numeric(catch),color = Yr)) +
  geom_path( lwd = 0.75, aes(group = type)) +
  geom_point(data = subset(pp_dat0, Yr %in% 1979:2024),  shape=16) +
  geom_point(data = subset(pp_dat0, Yr == 1978), color = '#b5e48c', shape=5) +
  geom_point(data = subset(pp_dat0, Yr == 2024), color = 'blue', shape=16) + 
  ## year labels for high F years
  geom_text(data = subset(pp_dat0, Yr %in% c(1978,1990,2008, 2024)),
            vjust = c(-1,-1,-1,2),
            size = 2,
            aes(label = Yr),
            color = "#184e77") +
  scale_colour_gradientn(colours = colorRampPalette(c("#b5e48c","#184e77"))(49))+ 
  theme(legend.position = 'none')+
  labs(x = 'Spawning Biomass (t)', y = 'Observed Catch (t)')

ggsave(last_plot(),
       file =here::here(year,"figs", 'catch_vs_spawnbio.png'),
       width = 5, height = 5, dpi = 400)
ggsave(last_plot(),
       file =here::here(mod_path, "plots", 'catch_vs_spawnbio.png'),
       width = 5, height = 5, dpi = 400)

#*  catch/totbio timeseries plot ----
ggplot(data = pp_dat0, aes(x = Yr, 
                           y = catch/totbio)) +
  geom_line( lwd = 0.75, aes(group = type), color = "#184e77") +  
  theme(legend.position = 'none')+
  labs(x = 'Year', y = 'Observed Catch (t)/Total Biomass (t)')

ggsave(last_plot(),
       file =here::here(year,"figs", 'catch_over_totbio_timeseries.png'),
       width = 5, height = 5, dpi = 400)
ggsave(last_plot(),
       file =here::here(mod_path, "plots", 'catch_over_totbio_timeseries.png'),
       width = 5, height = 5, dpi = 400)

#* OSA residuals ----
## need to create arrays from SS3 outputs


#** OSA for age comps (both fleets & sexes) ----
for(flt in 1:1){
  for(sx in 1:2){
    name_use <- paste0( c('fishery','survey')[flt],
                        '_age_',
                        c('females','males')[sx]
    )
    
    run_osa_temp <- mod18.2c_2024$agedbase %>%
      filter(Fleet == flt & Sex == sx) %>% 
      select(obs = Obs, exp = Exp,N =  effN,  index = Bin, years = Yr )  
    
    obs_use <- tidyr::pivot_wider(run_osa_temp %>% select(years, obs, index),
                                  names_from = index, values_from = obs,
                                  id_cols = years) %>%  select(-years)
    exp_use <- tidyr::pivot_wider(run_osa_temp %>% select(years, exp, index),
                                  names_from = index, values_from = exp,
                                  id_cols = years) %>% select(-years)
    
    N_use <- run_osa_temp %>% select(years, N) %>% distinct() %>% select(-years) %>% t()
    
    osa_res <- run_osa(obs_use, 
                       exp_use,
                       N_use,
                       fleet = c('fishery','survey')[flt], 
                       index = mod18.2c_2024$agebins, ## actual index not ages
                       years = unique(run_osa_temp$years), 
                       name_use)
    
    ## save them in two places
    plot_osa(list(osa_res),
             outpath = here::here(mod_path,"plots"))
    plot_osa(list(osa_res),
             outpath = here::here('docs',year,'model_plots'))
  }
}

#** OSA for length comps (both fleets & sexes) ----
for(flt in 1:2){
  for(sx in 1:2){
    name_use <- paste0( c('fishery','survey')[flt],
                        '_length_',
                        c('females','males')[sx]
                      )
    
    run_osa_temp <- mod18.2c_2024$lendbase %>%
      filter(Fleet == flt & Sex == sx) %>% 
      select(obs = Obs, exp = Exp,N =  effN,  index = Bin, years = Yr )  
    
    obs_use <- tidyr::pivot_wider(run_osa_temp %>% select(years, obs, index),
                                  names_from = index, values_from = obs,
                                  id_cols = years) %>%  select(-years)
    exp_use <- tidyr::pivot_wider(run_osa_temp %>% select(years, exp, index),
                                  names_from = index, values_from = exp,
                                  id_cols = years) %>% select(-years)
    
    N_use <- run_osa_temp %>% select(years, N) %>% distinct() %>% select(-years) %>% t()
    
    osa_res <- run_osa(obs_use, 
                       exp_use,
                       N_use,
                       fleet = c('fishery','survey')[flt], 
                       index =  mod18.2c_2024$lbins, ## actual index not ages
                       # index = 1:length(mod18.2c_2024$lbins), ## actual index not ages
                       years = unique(run_osa_temp$years), 
                       name_use)
    
    ## save them in two places
    plot_osa(list(osa_res),
             outpath = here::here(mod_path,"plots"))
    plot_osa(list(osa_res),
             outpath = here::here('docs',year,'model_plots'))
  }
}

## projection stuff 


# create tables ----
#* catch/abc/tac----

## this option only gives values 2007+
## dwnld values from AKFIN, bind to total catches and management measures (static table)
# akfin <- afscdata::connect(db = 'akfin')
# abc0 <- afscdata::sql_run(database = akfin, 
#                           query = "SELECT * FROM AKR.V_CAS_TAC") %>%
#   filter(FMP_AREA_CODE == 'BSAI' & 
#            grepl('Flathead|Flounder',SPECIES_GROUP_LABEL)) %>%
#   select(YEAR,ACCEPTABLE_BIOLOGICAL_CATCH, FINAL_TAC, OVERFISHING_LEVEL) %>%
#   arrange(YEAR)

## read in file from M Callahan
abc0 <- t(read.csv(here::here(year, 'data', 'raw','bsai_fhs_harvest_specs_1986.csv'))) %>%
  data.frame()
names(abc0)[1] <- 'value'
abc0$YEAR <- rep(2024:1986, each = 5)
abc0$variable <- stringr::str_extract(rownames(abc0), "^[^.]*")
abc0 <- abc0 %>% 
  filter(!(variable %in% c('CDQ','iTAC'))) %>%
  tidyr::pivot_wider(id_cols = 'YEAR', names_from = 'variable',
                      values_from = 'value')

## merge with observed catches & management measures
abc1 <- merge(abc0, mod18.2c_2024$catch[,c('Yr','Obs')], 
              by.x = 'YEAR', by.y = 'Yr') %>%
  merge(.,read.csv(here::here(year,'safe',
                              'static_tables','mgmt_measures.csv'))[,c('Year','Management.Measures')],
        by.x = 'YEAR', by.y = 'Year') %>%
  select(Year = YEAR, Total = Obs, ABC,
         TAC , OFL, 'Management Measures'= Management.Measures) %>%
  arrange(Year)

## overwrite final year of catches with value from PROJ
pcatch <- readLines(here::here(year,'model_runs','03b_projection','spm.dat'))[54:56] %>%
  strsplit(.,'\t') %>% ## split at tabs
  unlist() %>%
  strsplit(.,' +') %>% ## split at spaces 
  unlist()
pcatch<- round(as.numeric(pcatch[c(2,4,6)]))
abc1$Total[abc1$Year==year] <- pcatch[2]

write.csv(abc1,here::here(year,'safe','static_tables','catch_abc_tac_ofl_mgmt.csv'),
          row.names = FALSE)


## Figures for Director's Briefing Nov 2024 ----

### survey data

sdat <- mod18.2c_2024$cpue %>%
  mutate(lci = Obs-Obs*SE, uci = Obs + Obs*SE) %>%
  mutate(new = Yr >= 2020)

ggplot(data = sdat, aes(x = Yr, color = new)) +
  geom_point(  aes(y = Obs)) +
  geom_errorbar(aes(ymin = lci, ymax = uci)) +
  scale_color_manual(values = c('grey50','dodgerblue2'))+
  # theme_void()+
  labs(x = 'Year', y = 'Survey Biomass (t)', color = '') +
  theme(legend.position = 'none')


ggsave(last_plot(),
       height = 4, width = 6, unit = 'in',
       file = here::here(mod_path,'plots','directorsbriefing_survyobs.png'))


## SSB timeseries

sdat <- mod18.2c_2024$derived_quants %>%
  filter(grepl('SSB_',Label)) %>%
  mutate(Yr = as.numeric(substr(Label, 5,8)),
    lci = Value -1.96*StdDev, uci = Value + 1.96*StdDev) %>%
  filter(!is.na(Yr) & Yr < 2025) %>%
  mutate(new = Yr >= 2020)

ggplot(data = sdat, aes(x = Yr, color = new, fill = new)) +
  geom_ribbon(aes(ymin = lci, ymax = uci),alpha = 0.2, color = NA) +
  geom_point(  aes(y = Value)) +
  scale_fill_manual(values = c('grey50','dodgerblue2'))+
  scale_color_manual(values = c('grey50','dodgerblue2'))+
  # theme_void()+
  labs(x = 'Year', y = 'SSB (t)', color = '') +
  theme(legend.position = 'none')
ggsave(last_plot(),
       height = 4, width = 6, unit = 'in',
       file = here::here(mod_path,'plots','directorsbriefing_SSB.png'))
