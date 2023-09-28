### Updated 2020 by Cole based on a copy from Meaghan.

## this is not meant to be sourced, so go through it manually and
## carefully... especially the catch part which gets calculated
## and printed in the report.R script

## I'm basically using her code just to pull in results from the
## SS output files. I use the previous 2018 assessment files for
## the other inputs. I update the catches manually, then copy the
## files and run them.

## Next time I want to automate this a bit more but good enoguh
## for 2020 -CCM

library(r4ss)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
require(grid)
require(gridExtra)
require(lattice)
stop("don't source this file")
############2. DIRECTORIES #################################################################
# Directory with full assessment model results
master <- 'C:/Users/cole.monnahan/Work/assessments/BSAI_flathead/2020_BSAI_Flathead/projection'
setwd(master)

############3. SOURCE R SCRIPTS ##############################################################################
source("write_proj.r")         		 #Writes data files needed for proj_mod. Modified version of code from Steve Barbeaux
source("write_proj_spcat.r")
source("setup.r")
source("get_proj_res.r")	 		#Get projection results from proj_mod

############4.VARIABLES #######################################################################################
## passed to write_proj function
NSEX=2						# number of sexes used in assessment model
Nfishery=1					# number of fisheries(fleets) #This was set equal to 2
fleets=1					# fleet index number (associated with commercial fishery)
rec_age=3					# assumed age at recruitment
max_age=21					# maximum age in model
NAGE=length(rec_age:max_age)			# number of ages
FY=1964 					# first year used to subset SSB
rec_FY=1964					# first year used to subset recruitment
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

#5.2. Write projection input files
replist <- SS_output('../model_runs/Run06_francis_tuning/')
## Write proj.dat
write_proj(dir=getwd(), sdir=getwd(),
           data_file="Model_Proj.dat", data=replist,
           NSEX=NSEX, NAGE=NAGE, Nfishery=Nfishery,
           fleets=fleets, rec_age=rec_age, max_age=max_age, FY=FY,
           rec_FY=rec_FY, rec_LY_decrement=rec_LY_decrement,
           spawn_month=spawn_month, Fratios=Fratios)
#### Cole didn't use these, instead copied Carey's previous files
#### and updated catch manually
## ##Write proj_spcat.dat
## write_proj_spcat(dir=file.path(main_dir),sdir=proj_data_dir,
##                  data_file="Model_Proj_spcat.dat",
##                  data=Models[[1]],ct_yrs=ct_yrs)
## ## Write setup.dat
## setup(dir=main_dir, sdir=proj_data_dir, data_file="setup.dat",
##       data=Models[[1]], nsims=nsims, nproj=nproj)

#5.3. Run projection model
## copying files from master dir's parent directory to retro subdirs to run projection model
## run.bat, main.exe, and tacpar.dat need to be in parent directory
## main_dir <- c('projection'
## file.copy("run.bat", file.path(main_dir),overwrite=TRUE);
## file.copy("main.exe", file.path(main_dir),overwrite=TRUE);
## file.copy("tacpar.dat",file.path(main_dir),overwrite=TRUE);
## file.copy("setup.dat",file.path(main_dir),overwrite=TRUE);
## file.copy("Model_Proj.dat",file.path(main_dir),overwrite=TRUE);
## file.copy("Model_Proj_spcat.dat", 'projection/spp_catch.dat',overwrite=TRUE);
## setwd('projection')
## system("main")
## setwd('..')

##5.4. Get projection output
write("#Projection results",paste0(main_dir,"proj_res_summ.out"))
##The next 2 lines were missing
assess_LY=replist$endyr	#last assessment year
proj_out = get_proj_res(dir1=getwd(), proj_out='Projections',
                        data_file="proj_res_summ.out",
                        FY=(assess_LY+1), k=i, nproj=nproj, species=spp)
print(proj_out)


this_year <- 2020
rec_table1 <-
  read.table('Projections/percentdb.out') %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year+1:2) & scenario==1 &
         metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table2 <-
  read.table('Projections/alt2_proj.out', header=TRUE) %>%
  filter(Year %in% (this_year+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table <- bind_rows(rec_table1, rec_table2)
write.csv(rec_table, '../report/rec_table.csv', row.names=FALSE)


