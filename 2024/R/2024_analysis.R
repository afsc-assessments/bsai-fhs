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
#testy tests

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
## -hess_step

 
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
                  png = TRUE, plotdir = here::here(mod_path,'plots','compare'))

# misc figures ----
#* selectivity and maturity composite ----
 
## first make the custom selex plots if needed
png(here::here(mod_path,"plots","selex_maturity.png"), 
    8, height =5, unit = 'in', res = 420) 

par(mfrow=c(2,2), mar = c(0,0,1,1))
SSplotSelex(mod18.2c_2024, fleets  = 1,
            subplots = 3) 
 
SSplotSelex(mod18.2c_2024, fleets  = 2,
            subplots =2, agefactors = 'Asel' ) 

SSplotBiology(mod18.2c_2024, subplots = 6)
graphics.off()
 
#* biofreccompare.png ----
## comparison of spbio, recruitment, f traj 
## list all comparison plots made above
filepaths <- list.files(here::here(mod_path, "plots"), 
                        pattern = 'compare', full.names = TRUE)

## wrangle the three into a new image
png(here::here(mod_path,"plots","bio_f_rec.png"), width = 12, height =8, unit = 'in', res = 420) 
rl = lapply(filepaths[c(12,18,2)], png::readPNG)
gl = lapply(rl, grid::rasterGrob) 
gridExtra::grid.arrange(grobs=gl,ncol = 1) 
dev.off()
#* phase-plane plot ----
## take all reference values from Proj, noting that refs to "ofl" correspond to "b35" therein
 
## use proj values as denominators
rec_table1 <-
  read.table(here::here(year,'model_runs','03b_projection','percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year:(this_year+2)) & scenario==1 
         &  metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc', 'F_Mean')
  ) %>%
  arrange(year, metric) %>%
  tidyr::pivot_wider(names_from=year, values_from=value)

b35 <- as.numeric(subset(rec_table1, metric == 'SSBFofl')[,'2025']) ## in mt
f35 <- as.numeric(subset(rec_table1, metric == 'Fofl')[,'2025'] )
b100 <- as.numeric(subset(rec_table1, metric == 'SSBF100')[,'2025']) ## in mt

pp_dat <- mod18.2c_2024$timeseries %>% 
  dplyr::select(Yr, SpawnBio, Fy = `F:_1`) %>%
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

ggplot(data = pp_dat, aes(x = as.numeric(SB_B35), y = as.numeric(F_F35))) +
  geom_hline(yintercept = 1, col = 'grey88') +  geom_vline(xintercept = 1, col = 'grey88') +
  geom_segment(data = NULL, aes(x =0.4/0.35,  y = 1,xend =3.5, yend = 1), color = 'red') + ## OFL plateau
  geom_segment(data = NULL, aes(x =0.05,  y = 0,xend =0.4/0.35, yend = 1), color = 'red') + ## OFL ramp
  geom_segment(data = NULL, aes(x = 0.4/0.35,  y = 0.8,xend =3.5, yend = 0.8), color = 'red', linetype = 'dotted') + ## ABC  plateau
  geom_segment(data = NULL, aes(x = 0.05,  y = 0,xend =0.4/0.35, yend = 0.8), color = 'red', linetype = 'dotted') + ## ABC  ramp
  geom_path(color = 'grey44', lwd = 0.75, aes(group = type)) +
  geom_point(data = subset(pp_dat, Yr == 1978), color = 'black', shape=5) +
  geom_point(data = subset(pp_dat, Yr == 2024), color = 'black', shape=16) +
  geom_point(data = subset(pp_dat, Yr > 2024), color = 'seagreen4') +
  
  ## year labels for high F years
  geom_text(data = subset(pp_dat, Yr %in% c(1978,1990,2008, 2024:2026)),
            vjust = c(-1,-1,-1,2,2,2),
            size = 2, aes(label = substr(Yr,3,4))) +
  scale_x_continuous(limits = c(0,3.5)) +
  scale_y_continuous(limits = c(0,1.5)) +
  labs(x = expression('Spawning Biomass/B'[35]*"%"),y = expression('F/F'[35]*"%"))

ggsave(last_plot(),
       file =here::here(year,'mgmt', model, "plots", 'phase_plane.png'),
       width = 5, height = 5, dpi = 400)

AAA
## catch with inset
catch <- read.csv(here(year,'data','output','fsh_catch.csv')) %>% 
mutate(catch = catch/1000)
cplot1<-ggplot(catch, aes(x = year, y =catch)) +
  geom_line() + 
  labs(x = 'Year', y = 'Catch (t)')
cplot2<-ggplot(subset(catch, year > 1994), aes(x = year, y =catch)) +
  geom_line() + 
  scale_y_continuous(limits = c(0,35))+
  labs(x = 'Year', y = 'Catch (t)')


vp <- grid::viewport(width = 0.5, height = 0.5, x = 0.70, y = 0.65)
png(here::here(year,'mgmt',curr_mdl_fldr,'figs', 'catch_timeseries.png'),
    width = 6, height = 4, unit = 'in', res = 500)
print(cplot1)
print(cplot2, vp = vp)
dev.off()
 

 ## catch fits
png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                              "catch_fits.png"), 
    width = 6, height = 4, units = 'in', type ="cairo", res = 200)
catch_obspred <- read.csv(here::here(year,'mgmt',curr_mdl_fldr,'processed','catch.csv')) %>% 
 reshape2::melt(id = 'year')
 ggplot(data = NULL, aes(x = year, y =value/1000, color = variable)) +
  geom_line(data=subset(catch_obspred, variable == 'pred')) + 
  geom_point(data=subset(catch_obspred, variable != 'pred')) + 
  scale_color_manual(values = c('grey22','blue'), labels = c('Observed','Predicted'))+
  theme(legend.position = 'top') +
  labs(x = 'Year', y = 'Catch (t)', color = '')
dev.off()

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


## retro recruitment
retro_mc <- read.csv(here::here(year,'mgmt',curr_mdl_fldr,'processed',"retro_mcmc.csv"))
yrs <- 1961:2023
max_year = 2023
peels = 10
q_name <- purrr::map_chr(c(.025,.975), ~ paste0("q", .x*100))
q_fun <- purrr::map(c(.025,.975), ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
    purrr::set_names(nm = q_name)

  retro_mc %>%
    dplyr::select(paste0("log_rec_dev_", yrs), retro_year) %>%
    tidyr::pivot_longer(c(-retro_year), values_to = "biomass") %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(name, "[[:digit:]]+")),
                  biomass = biomass / 1000) %>%
    dplyr::group_by(year, retro_year) %>%
    dplyr::summarise_at(dplyr::vars(biomass), tibble::lst(!!!q_fun, median)) %>%
    dplyr::mutate(Retro = factor(retro_year)) %>%
    dplyr::ungroup() -> dat


  dat %>%
    dplyr::select(year, retro_year, median) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(pdiff = (median - median[retro_year==max_year]) /
                    median[retro_year==max_year]) %>%
    tidyr::drop_na() %>%
    dplyr::filter(year %in% (max_year-peels):max_year) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == retro_year, year !=max_year) %>%
    dplyr::summarise(rho = mean(pdiff)) %>%
    dplyr::pull(rho) -> ssb_rho

      png(filename=here::here(year, 'mgmt',curr_mdl_fldr, "figs", "retro_rec.png"), width = 6.5, height = 6.5,
      units = "in", type ="cairo", res = 200)

    dat %>%
    # filter(retro_year==2022) %>%
    ggplot2::ggplot(ggplot2::aes(year, median, color = Retro, group = Retro)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q2.5, ymax = q97.5, fill = Retro),
                         alpha = .05, color = NA) +
    ggplot2::ylab("Log Rec-Dev \n") +
    ggplot2::xlab("\nYear") +
    ggplot2::expand_limits(y = 0) +
    scico::scale_fill_scico_d(palette = "roma") +
    scico::scale_color_scico_d(palette = "roma") +
    funcr::theme_report() +
    ggplot2::scale_x_continuous(breaks = afscassess::tickr(dat, year, 10, start = 1960)$breaks,
                                labels = afscassess::tickr(dat, year, 10, start = 1960)$labels) +
    ggplot2::annotate(geom = "text", x=1963, y=Inf, hjust = -0.05, vjust = 2,
                      label = paste0("Mohn's rho = ", round(ssb_rho, 3)),
                      family = "Times") +
    ggplot2::theme(legend.position = "none") 

dev.off()

## comp data fits ----
## to use these functions it will want to look into processed/ for the fac
#afscassess::correct_comps(year, model_dir = curr_mdl_fldr, modname = mdl_name,
#dat_name = dat_name,  rec_age = 2, plus_age = 25, len_bins = lengths)
afscassess::plot_comps(year, folder = paste0('mgmt/',curr_mdl_fldr),save = TRUE)
# afscassess::plot_catch(year, folder = paste0('mgmt/',curr_mdl_fldr),save = TRUE)
## params 
afscassess::plot_params(year, 
folder = paste0('mgmt/',curr_mdl_fldr),
model_name = mdl_name, save = TRUE)


afscassess::plot_retro(year, folder = paste0('mgmt/',curr_mdl_fldr), n_retro = 10 )
afscassess::plot_selex(year, folder = paste0('mgmt/',curr_mdl_fldr))
afscassess::plot_survey(year, folder = paste0('mgmt/',curr_mdl_fldr))
afscassess::plot_phase(year, folder = paste0('mgmt/',curr_mdl_fldr), model_name = mdl_name)
afscassess::plot_rec_ssb(year, folder = paste0('mgmt/',curr_mdl_fldr), rec_age=rec_age)

afscassess::base_plots(year, 
folder = paste0('mgmt/',curr_mdl_fldr),
model_name = mdl_name, rec_age = rec_age)


afscassess::plot_compare_biomass(year,
models = c('2020.1-2021','2020.1-2023'))

## comparison of survey fits
vroom::vroom(here::here(year, paste0("mgmt/",curr_mdl_fldr), "processed", "survey.csv")) %>%
    tidytable::rename_with.(tolower) %>%
    tidytable::select.(year = starts_with("y"),
                       Observed = starts_with("bio"),
                       Predicted = pred,
                       se, lci, uci) %>%
    tidytable::pivot_longer.(-c(year, se, uci, lci)) %>%
    tidytable::mutate.(value = value / 1000,
                       uci = uci / 1000,
                       lci = lci / 1000,
                       src = '2023 Assessment') %>%
    bind_rows(vroom::vroom(here::here(year, paste0("mgmt/",prev_mdl_fldr), "processed", "survey.csv")) %>%
    tidytable::rename_with.(tolower) %>%
    tidytable::select.(year = starts_with("y"),
                       Observed = starts_with("bio"),
                       Predicted = pred,
                       se, lci, uci) %>%
    tidytable::pivot_longer.(-c(year, se, uci, lci)) %>%
    tidytable::mutate.(value = value / 1000,
                       uci = uci / 1000,
                       lci = lci / 1000,
                       src = '2021 Assessment'))    -> dat

ggplot(data= NULL, aes(x = year, color = src)) +
geom_point(data=subset(dat, src == '2023 Assessment' &
 name == 'Observed'), aes(y = value), color = 'grey44') +
geom_errorbar(data=subset(dat, src == '2023 Assessment'), width = 0, 
aes( ymin = lci, ymax = uci), color ='grey44') +
geom_line(data=subset(dat, src == '2023 Assessment' & 
name == 'Predicted'),aes(y = value),lwd = 0.75)+
geom_line(data=subset(dat, src == '2021 Assessment' & 
name == 'Predicted'),aes(y = value),lwd = 0.75) +
scale_color_manual(values = c('grey22','blue')) +
labs(x = 'Year', y = 'Survey Biomass (t)', color = '') +
theme(legend.position = c(0.2,0.8))

ggsave(last_plot(),
height = 4, width = 6, unit = 'in',
file = here(here(year,'mgmt',curr_mdl_fldr,'figs','survey_fit_compare.png')))



### comparison of VAST and DB estimator
biomass_dat <- read.csv(here(year,'data','raw','goa_total_bts_biomass_data.csv')) %>% 
mutate(sd = sqrt(biomass_var) ) %>%
select(year, 
biomass = total_biomass,sd) %>% mutate(src = 'Design-based')
vast <- read.csv(here(year,'dev','mb_vs_db','vast_2023.csv')) %>% 
select(year = Time, biomass = Estimate, sd = Std..Error.for.Estimate) %>%
mutate(biomass = biomass/1000, sd = sd/1000) %>%
mutate(src = 'VAST (model-based)') %>%
filter(biomass >0)

png(filename=here(here(year,'dev','mb_vs_db','mb_db_comparison.png')), 
    width = 6, height = 4, units = 'in', type ="cairo", res = 200)
rbind(biomass_dat,vast) %>%
mutate(lci = biomass-1.96*sd, uci = biomass+1.96*sd) %>%
ggplot(.,
 aes(x = year, y = biomass, fill = src, color = src)) +
geom_point(size = 3) +
#geom_ribbon(aes(ymin =lci,ymax = uci), alpha = 0.2) +
geom_errorbar(aes(ymin =lci,ymax = uci), width = 0) +
theme(legend.position = c(0.15,0.8)) +
scale_color_manual(values = c('blue','grey45')) +
scale_fill_manual(values = c('blue','grey45')) +
#scale_y_continuous(limits = c(0,2500), expand = c(0,0)) +
labs(x = 'Year', y = 'Biomass (1000 t)', fill = '', color = '')
dev.off()

png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", "selex_mat.png"), 
    width = 6, height = 6, units = 'in', type ="cairo", res = 200)
read.csv(here(year,'mgmt',curr_mdl_fldr,'processed','selex.csv')) %>%
  reshape2::melt(., id = 'age') %>%
  bind_rows(.,read.csv(here(year,'mgmt',curr_mdl_fldr,'processed','waa_mat.csv')) %>%
              dplyr::rename(., "waa"="srv1") %>%
  reshape2::melt(., id = 'age')) %>%
  filter(variable != 'waa') %>%
  ggplot(., aes(x = age, y = value, color = variable)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0,30,5))+
  geom_line(lwd = 1.1) +
  # theme(text = element_text(family = "Times New Roman"))+
  theme(legend.position = 'top') +
  scale_color_manual(values = c("#023047","#126782","#219ebc","#8ecae6","#fb8500","#ffb703"),
                     labels = c(paste0('Fishery ',c("1967-1976","1977-1995","1996-2006","2007-2023")),
                                'Survey','Maturity'))+
  labs(x = 'Age', y = 'Proportion', color = '')
dev.off()

## key derived quantities with uncertainty and comparison
biolabs <- as_labeller(c(
  'tot_biom'="Total Biomass (kt, ages 2+)",
  'spawn_biom'="Spawning Biomass (kt)",
  'Frate'="Fishing Mortality",
  'age2_recruits'="Age-2 Recruits (thousands)")
)

mcmc_summary_raw <- read.csv(here::here(year,'mgmt',curr_mdl_fldr,'processed','mceval.csv')) 
## manually rename the last chunk because the processing function didn't expect rec or F
names(mcmc_summary_raw)[282:(281+length(1961:2023))] <- paste0('age2_recruits_',1961:2023)
names(mcmc_summary_raw)[345:407] <- paste0('Frate_',1961:2023)



mcmc_summary_raw %>%
reshape2::melt()%>%
filter(grepl("biom|rec|Frate",variable)) %>%
mutate(year = as.numeric(stringr::str_sub(variable,-4,-1)),
variable = stringr::str_sub(variable, 1, -6)) %>%
mutate(src = '2023 Model') %>%
bind_rows(., read.csv(here::here(year,'mgmt',"2020.1-2021",'processed','bio_rec_f.csv')) %>%
            select(year,Frate = 'F',  age2_recruits=recruits,tot_biom,spawn_biom = sp_biom) %>%
            reshape2::melt(id = 'year') %>% 
            mutate(src = '2021 Model') %>%
            select(variable, value, year, src)) %>%
            
  
bind_rows(., read.csv(here::here(year,'mgmt',"2020.1-2021",'processed','mceval.csv')) %>%
            reshape2::melt() %>% 
filter(grepl("biom|rec|Frate",variable)) %>%
mutate(year = as.numeric(stringr::str_sub(variable,-4,-1)),
variable = stringr::str_sub(variable, 1, -6)) %>%
mutate(src = '2021 Model')) %>%
mutate(value = ifelse(value >1000,value/1000,value)) %>%
group_by(variable, year, src) %>%
summarise(median = median(value),
          lower = quantile(value, probs = 0.025), 
          upper = quantile(value, probs = 0.975)) ->
mcmc_summary

write.csv(subset(mcmc_summary, src == '2023 Model'), 
file = here(year,'mgmt',curr_mdl_fldr,'processed','mceval_summary.csv'), row.names = FALSE) 



summarize(median = median(value),
              lower = median(value) - qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n()),
              upper = median(value) + qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n())) 
mcmc_summary %>% filter(year == 2020 & variable == 'spawn_biom')

png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                              "bio_f_rec_compare.png"), 
    width = 6, height = 6, units = 'in', type ="cairo", res = 200)
mcmc_summary %>%
  filter(variable %in% c('spawn_biom', 'tot_biom', 'age2_recruits','Frate')) %>%
  ggplot(., aes(x = year,  color = src, fill = src)) +
  geom_line(aes(y=median))+
  geom_ribbon(aes(ymin =lower, ymax = upper),alpha = 0.2, color = NA)+
  theme(legend.position = 'top') +
  scale_fill_manual(values = c('grey44','blue'))+
  scale_color_manual(values = c('grey44','blue'))+
  facet_wrap(~variable,scales = 'free_y',labeller = biolabs) +
  labs(x = 'Year', y = '',color = '', fill = '') 
dev.off()

## update histogram plot

parlabs <- as_labeller(c(
  'natmort'="Natural Mortality (M)",
  'ABC'="ABC (kt)",
  'tot_biom_2023'="Current Total Biomass (kt)",
  'spawn_biom_2023'="Current Spawning Biomass (kt)",
  "q_srv1" = 'Trawl Survey Catchability q',
  'F40' = 'F40'))

mcmc_key_pars <- mcmc_summary_raw %>%
  dplyr::select(natmort, ABC, tot_biom_2023, F40, spawn_biom_2023, q_srv1) %>%
  reshape2::melt() %>%
  dplyr::mutate(value = ifelse(value > 1000,value/1000,value))

medians <- mcmc_key_pars %>%
  dplyr::group_by(variable) %>%
  summarize(median = median(value),
            lower = median(value) - qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n()),
            upper = median(value) + qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n()))

png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                        "hists_redux.png"), 
    width = 6, height = 4, units = 'in', type ="cairo", res = 200)  
ggplot(mcmc_key_pars, aes(x=value))+ 
  geom_histogram(fill = alpha('dodgerblue',0.85), color = 'dodgerblue') + 
  geom_vline(data=medians, aes(xintercept = median), linetype = 'dashed', color = 'black') +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  facet_wrap(~variable, scales = 'free_x', labeller = parlabs, ncol = 2)

dev.off()

## recdevs plot (requires parameter_summary csv made below)
png(filename=here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                        "recdevs.png"), 
    width = 6, height = 4, units = 'in', type ="cairo", res = 200)  
bind_rows(read.csv(here::here(year,'mgmt',curr_mdl_fldr,'processed','parameter_summary_devs.csv')) %>%
  filter(grepl('rec',variable)) %>%
  mutate(src = '2023 Model'),
read.csv(here::here(year,'mgmt',prev_mdl_fldr,'processed','parameter_summary_devs.csv')) %>%
  filter(grepl('rec',variable)) %>%
    mutate(src = '2021 Model')) %>%
  ggplot2::ggplot(., aes(x = year, color = src)) +
  geom_hline(yintercept =0, linetype = 'dashed', color = 'grey88') +
  geom_point(aes(y= median)) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0, alpha =0.5) +
  scale_color_manual(values = c(alpha('grey44',0.5),'blue')) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(1975,2025), 
                     breaks = c(seq(1975,2020,5),2023), 
                     labels = c(seq(1975,2020,5),2023))+
  labs(x = 'Year', y = 'log Recruitment Deviation', color = '') +
  theme(legend.position = c(0.1,0.8))

dev.off()

## prettier REMA plot (with theme)

load(here::here(year, 'mgmt',model,'apport','2023-09-16-rema_output.rdata'))  ## apport_out
apport_out$biomass_by_strata$strata = factor(apport_out$biomass_by_strata$strata,
                                             levels=c('WESTERN GOA','CENTRAL GOA','EASTERN GOA'))
ggplot(data = apport_out$biomass_by_strata, aes(x = year)) +
  geom_line(aes(y = pred), color = 'goldenrod') +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci), alpha = 0.2, fill = 'goldenrod') +
  geom_point(aes(y = obs), color = 'grey22') +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0, color = 'grey22') +
  facet_wrap(~strata) +
  labs(x = 'Year', y = 'Biomass (t)')

ggsave(last_plot(),
       file = here::here(year,'mgmt',model,'apport','rema_outs_redux.png'),
       width = 6, height =4 , unit = 'in',dpi =520)

### survey CPUE
library(akgfmaps)
## devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = FALSE)

SEBS <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")

## format this based on  https://github.com/afsc-gap-products/akgfmaps/blob/master/R/make_idw_map.R
## from 2021 production modsquad google drive folder
raw_surv <- readRDS(here(year,'data', 'raw','Data_Geostat_Sebastes_alutus.rds')) 

s2021 <- raw_surv %>% 
  mutate( COMMON_NAME = 'Pacific Ocean Perch') %>%
  dplyr::select(Year,COMMON_NAME, CPUE_KGHA  = Catch_KG, LATITUDE = Lat, LONGITUDE = Lon) %>%
  dplyr::filter(Year == 2021) %>%
  make_idw_map(region = "goa",
               # set.breaks = "jenks", ## auto
               set.breaks = c(0,1000, 2000, 3000, 4000,7500), ## standardized breaks
               in.crs = "+proj=longlat",
              
               out.crs = "EPSG:3338", # Set output coordinate reference system
               use.survey.bathymetry = FALSE, ## for GOA
               grid.cell = c(20000, 20000)) %>% # 20x20km grid
  add_map_labels() %>% 
  change_fill_color(new.scheme = "green2", show.plot = TRUE) #%>%
  # create_map_file(file.prefix = NA, 
  #                           file.path = NA, 
  #                           try.change_text_size = TRUE, # 12x9 is a pre-defined size
  #                           width = 12, 
  #                           height = 9, 
  #                           units = "in", 
  #                           res = 300, 
  #                           bg = "transparent")

s2023 <- raw_surv%>% 
  mutate( COMMON_NAME = 'Pacific Ocean Perch') %>%
  dplyr::select(Year,COMMON_NAME, CPUE_KGHA  = Catch_KG, LATITUDE = Lat, LONGITUDE = Lon) %>%
  dplyr::filter(Year == 2023) %>%
  make_idw_map(region = "goa",
               #set.breaks = "jenks", ## auto
                  set.breaks = c(0,1000, 2000, 3000, 4000,7500), ## standardized breaks
               in.crs = "+proj=longlat",
               out.crs = "EPSG:3338", # Set output coordinate reference system
               use.survey.bathymetry = FALSE, ## for GOA
               grid.cell = c(20000, 20000)) %>% # 20x20km grid
  add_map_labels() %>% 
  change_fill_color(new.scheme = "green2", show.plot = TRUE) #%>%
  # create_map_file(file.prefix = NA,
  #                           file.path = NA,
  #                           try.change_text_size = TRUE, # 12x9 is a pre-defined size
  #                           width = 12,
  #                           height = 9,
  #                           units = "in",
  #                           res = 300,
  #                           bg = "transparent")


p1 <- s2021$plot+theme(legend.position = 'right') 
p2 <- s2023$plot+theme(legend.position = 'right') 

png(file = here::here(year,'mgmt',model,'figs','cpue_maps.png'),
width = 8, height = 10, unit = 'in', res = 520)
Rmisc::multiplot(plotlist = list(p1,p2), cols = 1)
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


