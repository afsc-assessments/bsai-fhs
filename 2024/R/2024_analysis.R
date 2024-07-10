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

library(gapindex)
library(afscdata)
library(afscassess)
library(r4ss)
library(dplyr)
library(here)
library(purrr)
library(ggplot2)
theme_set(afscassess::theme_report())

## This worked for MK in VSCode:
# options(buildtools.check = function(action) TRUE )
# devtools::install_github("afsc-assessments/rema") ## did not update other pckgs
# pak::pkg_install("afsc-assessments/rema")
library(rema)


# globals ----
year <- this_year <-  2024
rec_age = 0 ## this is default for SS3
plus_age = 21
ages = rec_age:plus_age
lengths = c(seq(6,40,2),seq(43,58,3))
TAC = c(25000, 25000, 35500) # 2021, 2022, 2023
species = "FSOL"
curr_mdl_fldr = "18.2c_2024-lcf2"
prev_mdl_fldr = "18.2c_2020" 

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
message("Predicted ", this_year, " catch= ", round(catch_this_year+ catch_to_add,0)) ##9272
stop("update the ss .dat file with the newly pulled data and past the Predicted catch into ss .dat file as well.")


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
model <- '18.2c_2024'
mod_path <- here::here(year,'mgmt',model)
mod18.2c_2024 <- r4ss::SS_output(mod_path, verbose = FALSE)
#* save NAA as electronic file ----
write.csv(mod18.2c_2024$natage, file = here::here(mod_path,'natage.csv'), row.names = FALSE)
#* re-create SS-plots ----
##  ensure this folder gets named "plots/" or lookup won't work

SS_plots(mod18.2c_2024)

#* re-create comparison plots ----
## will save these in the plots/ folder made above
mod18.2c_2020 <- r4ss::SS_output(here::here(year,'mgmt','18.2c_2020'), verbose = FALSE)
SSplotComparisons(SSsummarize(biglist = list(mod18.2c_2020,mod18.2c_2024)),
                  legendlabels = c('2020 Model', '2024 Model'),
                  col = c('grey22','blue'),
                  png = TRUE,
                  plotdir = here::here(mod_path,'plots','compare'))
## need to manually make total biomass comparison plot
mod18.2c_2020$timeseries %>%
  select(Yr, Bio_smry) %>%
  mutate(mod = '2020 Model') %>%
  rbind(., mod18.2c_2024$timeseries %>%
          select(Yr, Bio_smry) %>%
          mutate(mod = '2024 Model')) %>%
  ggplot(., aes(x = Yr, y = Bio_smry/1000, color = mod)) +
  geom_line(lwd = 0.75) +
  theme(legend.position = c(0.85,0.9))+
  scale_y_continuous(limits = c(0,800))+
  scale_color_manual(values = c('grey22','blue')) +
  labs(x = 'year', y = 'Total 3+ Biomass', color = '')

ggsave(last_plot(), file = here::here(mod_path,'plots','compare',
                                      'compare18_totalbiomass.png'),
       width = 5, height = 4, dpi = 520, units = 'in')
# misc figures ----
#* growth, selectivity and maturity composite ----
 
##make the custom selex plots if needed
png(here::here(mod_path,"plots","growth_selex_maturity.png"), 
    8, height =5, unit = 'in', res = 420) 
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

pp_dat <- mod18.2c_2024$timeseries %>% 
  dplyr::select(Yr, SpawnBio, Fy = F1) %>%
  filter(Yr > 1977  ) %>%
  mutate(SB_B35 = SpawnBio/(b35), F_F35 = Fy/f35, type = 'aa') %>%
  arrange(., Yr)

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
            size = 2, aes(label = substr(Yr,3,4)), color = "#184e77") +
  scale_color_manual(values = colorRampPalette(c("#b5e48c","#184e77"))(49))+
  scale_x_continuous(limits = c(0,1.5)) +
  scale_y_continuous(limits = c(0,1.5)) +
  theme(legend.position = 'none')+
  labs(x = expression('Spawning Biomass/B'[35]*"%"),y = expression('F/F'[35]*"%"))

ggsave(last_plot(),
       file =here::here(year,'mgmt', model, "plots", 'phase_plane.png'),
       width = 5, height = 5, dpi = 400)

AAA

## projection stuff 

pdt <- data.frame(read.table(here::here(year,'mgmt',curr_mdl_fldr,'proj',"goa_pop_out","bigfile.out"), header=TRUE))
pdt.long <- tidyr::pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  dplyr::summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
g <- ggplot(pdt.long, aes(Yr,  med, ymin=lwr, ymax=upr, fill=Alternative, color=Alternative)) +
  facet_wrap('metric', scales='free_y') + ylim(0,NA) +
  geom_ribbon(alpha=.4) + theme_bw() +
  labs(x='Year', y='Estimated 80% CI')

## SB vs Year custom plot for ppt
png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                              "proj_sb.png"), 
    width = 6, height = 4, units = 'in', type ="cairo", res = 200)
pdt.long %>%
  filter(metric == 'SSB' & Alternative %in% c(1,4)) %>%
  ggplot(., aes(x = Yr, y = med, color = Alternative)) +
  theme(legend.position = 'none') +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Alternative), color =NA, alpha = 0.2) +
  scale_y_continuous(limits = c(100,260)) +
   scale_x_continuous(limits = c(2023,2035), labels = seq(2023,2035,2),
                     breaks =  seq(2023,2035,2)) +
  scale_color_manual(values = c('dodgerblue','grey44')) +
  scale_fill_manual(values = c('dodgerblue','grey44')) +
  geom_hline(yintercept = 137.447, linetype = 'dotted') + ## b40
  geom_hline(yintercept = 120.266) + ## b35
  geom_text(x = 2030, y = 200, label = 'Alt. 4 (avg F)', 
  color = 'grey44', size = 2) + 
  geom_text(x = 2030, y = 160, label = 'Alt. 1 (maxABC)', 
  color = 'dodgerblue', size = 2) + 
  geom_text(x = 2024, y = 140, label = 'SB40', size = 2) + 
  geom_text(x = 2024, y = 115, label = 'SB35', size = 2) + 
  labs(y = 'SSB (1000 t)', x = 'Projection Year')

dev.off()


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

## NAA as a separate file
mrep <- readLines(here::here(year, 'mgmt',model,paste0(modname,'.rep')))
naa0 <- mrep[grep('Numbers',mrep):(grep('Numbers',mrep)+length(1961:2023))] 
df1 <- strsplit(naa0[-1], " ")
df1 <- do.call(rbind, df1)
df1 <- df1[,-2]
df1 <- as.data.frame(df1)
names(df1) <- c('Year',paste0('age_',2:29))
write.csv(df1,file =  here::here(year, 'mgmt',model,'processed','naa.csv'),row.names = FALSE)



x_full = data.frame()
for (i in 2:length(naa0) ) {
  x<-data.frame(naa0[[i]])
  writeLines(x[[i]],"test.csv")
  data<-read.csv("test.csv", header=F, sep=" ")
  df<-data[,colSums(is.na(data)) == 0]
  print(df)
}

## parameter summaries
allpars <- read.csv(here::here(year, 'mgmt',model,'processed','mcmc.csv')) %>%
  reshape2::melt() %>%
  dplyr::group_by(variable) %>%
  summarise_at(vars(value),
               list(
                 Q1=~quantile(., probs = 0.25),
                 median=median, 
                 Q3=~quantile(., probs = 0.75))) 

other_pars <- filter(allpars, !grepl('dev',variable)) %>%
  mutate(Parameter = c('Avg. log Annual Recruitment',
                       'Age at 50% Selectivity, Timeblock 2',
                       'Delta Selectivity, Timeblock 2',
                       'Age at 50% Selectivity, Timeblock 3',
                       'Delta Selectivity, Timeblock 3',
                       'Age at 50% Selectivity, Timeblock 4',
                       'Delta Selectivity, Timeblock 4',
                       'Age at 50% Selectivity, Survey',
                       'Delta Selectivity, Survey',
                       'Avg. log fishing mortality',
                       'Age at 50% maturity',
                       'Delta Maturity',
                       'log catchability (survey)',
                       'log natural mortality',
                       'F50%',
                       'F40%',
                       'F35%'
                       )) %>%
  select(Parameter, everything())
f_rec_devs <- filter(allpars, grepl('dev',variable)) %>% 
  mutate(year = c(1961:2023,1935:2023)) %>%
  select(variable, year, Q1,median,Q3)

write.csv(f_rec_devs,here::here(year, 'mgmt', curr_mdl_fldr,'processed','parameter_summary_devs.csv'),row.names = FALSE)
write.csv(other_pars,here::here(year, 'mgmt', curr_mdl_fldr,'processed','parameter_summary.csv'),row.names = FALSE)


## Figures for Director's Briefing Nov 2024 ----

### survey data
# ggplot(data= NULL, aes(x = year)) +
# geom_point(data=subset(dat, src == '2023 Assessment' &
#  name == 'Observed' & year < 2023), aes(y = value), color = 'grey88') +
#  geom_errorbar(data=subset(dat, src == '2023 Assessment' & year < 2023), width = 0, 
# aes( ymin = lci, ymax = uci), color ='grey88') +
# geom_point(data=subset(dat, src == '2023 Assessment' &
#  name == 'Observed' & year == 2023), aes(y = value), size = 3, color = 'dodgerblue2') +
# geom_errorbar(data=subset(dat, src == '2023 Assessment' & year == 2023), width = 0, 
# aes( ymin = lci, ymax = uci), color ='dodgerblue2') +
# 
# labs(x = 'Year', y = 'Survey Biomass (t)', color = '') +
# theme(legend.position = c(0.2,0.8)) +
# theme_void()
# 
# ggsave(last_plot(),
# height = 4, width = 6, unit = 'in',
# file = here(here(year,'mgmt',curr_mdl_fldr,'figs','directorsbriefing_survyobs.png')))


