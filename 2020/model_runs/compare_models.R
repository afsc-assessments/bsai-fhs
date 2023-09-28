### Run and compare models

library(tidyverse)
library(r4ss)
packageVersion('r4ss') #  '1.40.0'

m01 <- SS_output('Run01_2018_final', verbose=FALSE)
m02 <- SS_output('Run02_2018_update_SS', verbose=FALSE)
SS_plots(m01)
SS_plots(m02)


setwd('Run03_2018_new_data')
system('ss -nox -iprint 500')
setwd('..')
m03 <- SS_output('Run03_2018_new_data', verbose=FALSE)
SS_plots(m03)


### Take rec dev adjust output from here
setwd('Run04_2020_new_data')
system('ss -nox -iprint 500')
setwd('..')
m04 <- SS_output('Run04_2020_new_data', verbose=FALSE)
SS_plots(m04)

## Update it here
setwd('Run05_update_biasadjust')
system('ss -nox -iprint 500')
setwd('..')
m05 <- SS_output('Run05_update_biasadjust/')
SS_plots(m05)
## Francis data weighting. Seemed to converge quickly so just did
## one iteration for m06
tune1 <- SS_tune_comps(m05, option = "Francis")


## Take suggested weightings and update them here
setwd('Run06_francis_tuning')
system('ss -nox -iprint 500')
setwd('..')
m06 <- SS_output('Run06_francis_tuning')
SS_plots(m06)

tune2 <- SS_tune_comps(m06, option='Francis')
## Super close so no need to do another iteration
cbind(tune1$New_Francis, tune2$New_Francis)

## Calculate ADSB
yy <- 18
sbold <- filter(m01$timeseries, Yr >= 1997 & Yr <=2000+yy) %>% pull(SpawnBio)
sbnew <- filter(m06$timeseries, Yr >= 1997 & Yr <=2000+yy) %>% pull(SpawnBio)
sqrt(sum((sbnew/sbold-1)^2/(yy+24)))

### Comparisons among models.
## Verify the upgrade to SS version doesn't change it
x <- SSsummarize(biglist=list(m1=m01,m2=m02))
SSplotComparisons(x, png=TRUE,
                  plotdir='model_comparisons/m02_m01/',
                  legendlabels=c('m02', 'm01'))

x <- SSsummarize(biglist=list(m2=m02,m3=m03))
SSplotComparisons(x, png=TRUE,
                  plotdir='model_comparisons/m02_m03/',
                  legendlabels=c('m02', 'm01'))

## Did tuning have an effect?
x <- SSsummarize(biglist=list(m04,m06), verbose=FALSE)
SSplotComparisons(x, png=TRUE,
                  plotdir='model_comparisons/m04_m06/',
                  legendlabels=c('m04', 'm06'))

## 2018 vs 2020
x <- SSsummarize(biglist=list(m01, m06), verbose=FALSE)
SSplotComparisons(x, png=TRUE,
                  plotdir='model_comparisons/m04_m06/',
                  legendlabels=c('m01', 'm06'))


## test
mypar <- list(mar=c(3,3,1,1), mgp=c(1,.5,0), tck=-.01,
              col.axis='red',
              col.lab='blue', cex.lab=2)
mypar <- NULL
mypar <- list(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01,
              col.axis=gray(.4))
x <- SSsummarize(biglist=list(m02, m06), verbose=FALSE)
devtools::load_all('C:/Users/cole.monnahan/r4ss')
SSplotComparisons(x, png=TRUE, par=mypar, new=FALSE,
                  plotdir='model_comparisons/test',
                  legendlabels=c('m01', 'm06'))


###--------------------------------------------------
### Retrospective analysis
Npeels <- 10
SS_doRetro(getwd(), oldsubdir='Run06_francis_tuning', years=0:-Npeels)
retroModels <- SSgetoutput(dirvec=file.path("retrospectives",paste("retro",0:-Npeels,sep="")))
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + 0:-Npeels
SSplotComparisons(retroSummary, endyrvec=endyrvec,
                  legendloc='topleft',
                  plotdir='retrospectives/topleft', png=TRUE,
                  legendlabels=paste("Data",0:-Npeels,"years"))
SSplotComparisons(retroSummary, endyrvec=endyrvec,
                  legendloc='bottomleft',
                  plotdir='retrospectives/bottomleft', png=TRUE,
                  legendlabels=paste("Data",0:-Npeels,"years"))
## Calculate Mohn's rho
rho <- SSmohnsrho(retroSummary, endyrvec=endyrvec)
saveRDS(rho, file='retrospectives/FHS_retro_rhos.RDS')
rho$AFSC_Hurtado_SSB %>% round(3)
rho$AFSC_Hurtado_Rec %>% round(3)
rho$AFSC_Hurtado_F %>% round(3)

## ### --------------------------------------------------
## ### Likelihood profiles
## ### Not used in 2020, butr to explore at future date
## ##
## ## Over natural mortality
## dir.create('profiles/profile_M', FALSE)
## file.copy(list.files('Run06_francis_tuning', full.names=TRUE),
##           to='profiles/profile_M',
##           overwrite=TRUE, recursive=TRUE)
## mydir <- file.path('profiles', 'profile_M')
## starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
## starter$ctlfile <- "control_modified.ss"
## starter$prior_like <- 1
## SS_writestarter(starter, dir=mydir, overwrite=TRUE)
## m.vec <- seq(0.1,0.3,len=15) %>% round(3)
## Nprofile <- length(m.vec)
## profile <- SS_profile(dir=mydir, # directory
##                       model="ss",
##                       masterctlfile="control.ss_new",
##                       newctlfile="control_modified.ss",
##                       string="NatM",
##                       profilevec=m.vec)
## profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Nprofile)
## profilesummary <- SSsummarize(profilemodels)
## profilemodels$MLE <- m06
## profilesummary <- SSsummarize(profilemodels)
## png('profiles/profile_M.png', width=7, height=5, units='in', res=500)
## SSplotProfile(profilesummary, # summary object
##               profile.string = "NatM_p_1_Fem", # substring of profile parameter
##               profile.label="Natural Mortality (M)") # axis label
## dev.off()
## ## make timeseries plots comparing models in profile
## SSplotComparisons(profilesummary,legendlabels=paste("M =",c(m.vec,.2)),
##                   plotdir='profiles/profile_M', png=TRUE)
## ## ## Try to get it to work with fleets
## ## str(profilesummary$likelihoods)
## ## str(profilesummary$likelihoods_by_fleet)
## ##
## ### Over catchability dir.create('profiles/profile_Q', FALSE)
## ##
## file.copy(list.files('Run06_francis_tuning', full.names=TRUE),
##           to='profiles/profile_Q',
##           overwrite=TRUE, recursive=TRUE)
## mydir <- file.path('profiles', 'profile_Q')
## starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
## starter$ctlfile <- "control_modified.ss"
## starter$prior_like <- 1
## SS_writestarter(starter, dir=mydir, overwrite=TRUE)
## q.vec <- seq(-.75,0.75,len=10) %>% round(2)
## Nprofile <- length(q.vec)
## # run SS_profile command
## profile <- SS_profile(dir=mydir, # directory
##                       model="ss",
##                       masterctlfile="control.ss_new",
##                       newctlfile="control_modified.ss",
##                       string="LnQ",
##                       profilevec=q.vec)
## profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Nprofile)
## profilesummary <- SSsummarize(profilemodels)
## ## OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
## MLEmodel <- SS_output("Run06_francis_tuning")
## profilemodels$MLE <- MLEmodel
## profilesummary <- SSsummarize(profilemodels)
## ## END OPTIONAL COMMANDS
## ## plot profile using summary created above
## png('profiles/profile_Q.png', width=7, height=5, units='in', res=500)
## SSplotProfile(profilesummary, # summary object
##               profile.string = "LnQ", # substring of profile parameter
##               profile.label="log-catchability (survey) (lnQ)") # axis label
## dev.off()
## ## make timeseries plots comparing models in profile
## SSplotComparisons(profilesummary,legendlabels=paste("M =",c(q.vec,.2)),
##                   plotdir='profiles/profile_Q', png=TRUE)
