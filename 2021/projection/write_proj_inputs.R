## Cole's initial attempt to read results from SS for proj. THis
## is old and wrong. Got a better version from Meaghan
stop("Don't use this")

library(r4ss)
library(tidyverse)

dat <- SS_output('model_runs/Run04_2020_new_data', verbose=FALSE)

names(dat)
str(dat$timeseries)
str(dat$biology)


nages <- dat$nagebins
## Nat mortality at age
Mf <- dat$Natural_Mortality_Bmark[1,-(1:4)] %>% as.numeric()
Mm <- dat$Natural_Mortality_Bmark[2,-(1:4)] %>% as.numeric()
## Maturity at age
Matf <- filter(dat$endgrowth, Sex==1)$Age_Mat
Matm <- filter(dat$endgrowth, Sex==2)$Age_Mat
## weight at age for fishery
Wf <- filter(dat$wtatage, Yr==2020 & Sex==1 & Fleet==1)[,-(1:6)] %>% as.numeric()
Wm <- filter(dat$wtatage, Yr==2020 & Sex==2 & Fleet==1)[,-(1:6)] %>% as.numeric()
## selex at age for fishery
Sf <- filter(dat$ageselex, Yr==2020 & Sex==1 & Fleet==1 & Factor=='Asel2')[,-(1:7)] %>% as.numeric()
Sm <- filter(dat$ageselex, Yr==2020 & Sex==2 & Fleet==1 & Factor=='Asel2')[,-(1:7)] %>% as.numeric()
## numbers at age in 2020. this assumes beginning of year not mid year
Nf <- filter(dat$natage, Yr==2000 & Sex==1)[1,-(1:12)] %>% as.numeric()
Nm <- filter(dat$natage, Yr==2000 & Sex==2)[1,-(1:12)] %>% as.numeric()
## is this right?
recruits <- dat$timeseries$Recruit_0
ssb <- dat$timeseries$SpawnBio
Nrecruits <- length(recruits)

options(digits=7)
## Write to file
mywrite <- function(x, ...) write(x, file=out, append=TRUE, ...)
 fname <- 'test.dat'
out <- file.path('projection', fname)
write(paste("## Proj input file for BSAI flathead written by R script on", lubridate::today()), out, ncolumns =  1 )
mywrite("FLATHEAD")
mywrite("0 ?")
mywrite("0 # Dorn buffer")
mywrite("1 # num fisheries")
mywrite("2 # num sexes")
mywrite(".01 # average 5 year F") # right?
mywrite("0.4 # SPR ABC")
mywrite("0.35 # SPR MSY")
mywrite("1 # spawning month")
mywrite(paste(nages, " # number of ages"))
mywrite("1 # Fratio")
mywrite("# natural mortality, females then males")
mywrite(Mf, ncolumns=nages+1)
mywrite(Mm, ncolumns=nages+1)
mywrite("# maturity, females then males")
mywrite(Matf, ncolumns=nages+1)
mywrite(Matm, ncolumns=nages+1)
mywrite("# wt spawning females")
mywrite(Wf, ncolumns=nages+1)
mywrite("# Weight at age, females then males")
mywrite(Wf, ncolumns=nages+1)
mywrite(Wm, ncolumns=nages+1)
mywrite("# Selex at age, females then males")
mywrite(Sf, ncolumns=nages+1)
mywrite(Sm, ncolumns=nages+1)
mywrite("# Numbers at age in 2020, females then males")
mywrite(Nf, ncolumns=nages+1)
mywrite(Nm, ncolumns=nages+1)
mywrite("# Num of recruits")
mywrite(length(recruits))
mywrite("# recruits")
mywrite(recruits, ncolumns=length(recruits))
mywrite("# SSB")
mywrite(ssb, ncolumns=length(recruits))

