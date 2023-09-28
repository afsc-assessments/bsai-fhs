### File  copied from
### Get_Fishery_Ages_From_Raw_Length_StratifyOptions_wPortSamples2.R
### and modifed starting 9/2020 by Cole.

## Get Fishery Ages

## Carey McGilliard
## 8/9/2017
## This program pulls age and length data from the AFSC database
## NOTE: fishery length comps required;

## right now, just AKFIN ANSWERS Norpaq Observer Length Data
## saved as a .csv file works for the stratified method the
## following does not work with this version, but should be
## updated to do so (both updates for stratification options and
## with port data are necessary): first
## run"Pieces_Fishery_Lengths.R and save length comps to DataDir
## directory" organizes data into age-length key with optional
## stratifications by season, nmfs area (or code in a combo of
## nmfs areas), and gear reads in fishery length comps considers
## both port data and haul data, but port data is sometimes
## missing number of hauls in a port_join and these are filled in
## with mean number of tows for a port_join calculates proportion
## at age within each length bin and multiplies by length comps
## sums over the length bins to obtain age comps puts age comps
## into proportions organizes data to read into SS more easily

## Questions to ask to make decisions about stratification: Does
## length-at-age vary by stratification grouping for this stock?
## If so, use that stratification grouping. If not,
## stratification may not be necessary.  Where does the catch for
## the stock come from relative to the length and age samples?
## Is there about the same proportion of age or length samples in
## the data as for the catch by stratification groupings?  If not
## (for lengths), can you take care of this in standardizing the
## length comp data before running this program?  Options for
## taking care of this would be: used extrapolated number to
## scale up lengths if extrapolated number by stratification
## grouping is in the same proportions as the catch use catch
## proportions instead of extrapolated number to standardize the
## length data If the sampling ## s from ages specifically are
## different from catch proportions, then consider adding a
## standardization to this program that takes age comps (in
## numbers) by stratification grouping and standardizes by where
## the catch happens. The program doesnt' do this right now.

## could go back and write out Nhaul_Strat.df and
## NumPortHauls_Strat.df for haul info by stratification
## groupings program doesn't currently write this out, but
## calculates it.
## ----------------------------------------------------------------
## options(scipen = 999) #Must have for reading in hauljoin
## library(RODBC)
## library(mgcv)
## library(r4ss)
## library(reshape2)
## library(tidyverse)
## library(ggjoy)

## after these packages have been installed (what are you doing
## with these packages?)
## library(FSA)
## library(FSAdata)
## library(NCStats)
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#Parameter inputs:
#----------------------------------------------------------------------------
## MyDir <- "C:\\GitProjects\\newsbss\\"
## DirFUNCTIONS<-paste0(MyDir,"FUNCTIONS\\")
## DataDir <- "\\\\AFSC-S79\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2017\\Rex\\Data\\"
## DataDir <- "\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2018\\BSAI_Flathead\\Data\\"

## MyDir <- paste0(getwd(),"\\")
DirFUNCTIONS <- paste0("c:\\Users\\cole.monnahan\\Work\\assessments\\newsbss\\FUNCTIONS\\")
## DataDir <- paste0(getwd(), '/data/')

SpeciesCode = "103" #105 is rex sole
FmpArea = "500 and 544"  ##Typical options are AI = 539-544, GOA = 600 to 699 (600-650 incl. all the management areas and 690 is outside the EEZ), BS = 500 to 539
ByGear = 1               #1 = yes, by gear and produces age data in proportions for separate gears: gear types: 1 = nonpelagic trawl, 2 = pelagic trawl, 8 = longline
BySeason = 0             #1 = yes, sort by season
ByFmp.Subarea = 1        #1 = yes, sort by CG, WG, WY, SE, BS, AI      (probably don't want By Fmp.Subarea =1 AND ByNMFS.Area = 1)
ByNMFS.Area = 0          #1 = yes, sort by area 610, 620, etc. (probably don't want By Fmp.Subarea =1 AND ByNMFS.Area = 1)
#ByDepth = 0             An idea for later on (lots of depth info missing for rex ages) #1 = yes, sort by depth bin, 0 = don't
ByPortOrHaul = 1         #1 = sort by port or haul; 0 = lump port and haul together, but still add in the number of hauls that comprise the port data.
                         # not sorting by port or haul is not recommended if using # of hauls or a similar metric for data weighting within fishery ages is being used.
                         # only lump port and haul together if setting sample size = for all years of the data in the assessment.

UseExtrapNum = 1       #1= Scale the lengths by extrapolated number of that species in each haul (this is done for BSAI flathead); 0 = don't use extrapolated number (extrap num is not used for GOA rex sole)
BuckOrSS = "Buck"      #"Buck" = Run the program using Buck's length binning scheme, "SS" = use SS's length binning meanings.
SepGear = 1            #Separate results by gear so that can model gears separately in the assessment (also must have ByGear = 1 for this option to work)
SepPortOrHaul = 1       #Separate results by Port sample or haul sample so that can use one of the two in the assessment (also must have ByPortOrHaul = 1 for this option to work)
SepFmp.Subarea = 1     #Separate results by FMP Subarea
LengthBins = c(seq(6,40,2),seq(43,58,3)) #length bins for BSAI flathead sole assessment
AgeBins = seq(1,21,1)    #age bins for BSAI flathead sole assessment

## Final output file used in 2018

## "Observer_Age_Comps_SS_ByGear1_SepGear1_BySeason0_ByFmpSubarea1_SepFmpSubarea1_ByNMFSArea0_ExNum1"
## paste0("Observer_Age_Comps_SS_ByGear",ByGear,"_SepGear",SepGear,"_BySeason",BySeason,"_ByFmpSubarea",ByFmp.Subarea,"_SepFmpSubarea",SepFmp.Subarea,"_ByNMFSArea",ByNMFS.Area,"_ByPortOrHaul",ByPortOrHaul,"_SepPortOrHaul",SepPortOrHaul,"_ExNum",UseExtrapNum)

## -------------------------------------------------------------------

#---------------------
#source code:
#---------------------
source(paste0(DirFUNCTIONS,"BIN_LEN_DATA.R"))
source(paste0(DirFUNCTIONS,"BIN_AGE_DATA.R"))
source(paste0(DirFUNCTIONS,"BIN_LEN_DATA_Bucks_Models.R"))
#---------------------

## ## Read in raw length data downloaded from AKFIN answers observer
## ## length table need to manually change the Haul.Offload.Date
## ## column to a mm/dd/yyyy format before reading in
## ## test <- readRDS('data/lengths_fishery_domestic.RDS')
## AKL.df <- read.csv(file = paste0(DataDir,"norpac_length_report.csv"),skip = 7)
## #quarters(as.Date(Haul_Agg.df$HAUL_DATE))
## AKL.df$TheDate <- as.Date(AKL.df$Haul.Offload.Date,format = "%m/%d/%Y")
## AKL.df$Season <- quarters(AKL.df$TheDate)
## AKL.df$Haul.Join[AKL.df$Haul.Join == ""]<-NA
## AKL.df$Port.Join[AKL.df$Port.Join == ""]<-NA
## if (ByPortOrHaul == 1) {
##   AKL.df$PortOrHaul<-"none"
##   AKL.df$PortOrHaul[complete.cases(AKL.df$Haul.Join)]<-"haul"
##   AKL.df$PortOrHaul[complete.cases(AKL.df$Port.Join)]<-"port"
##   AKL.df$PortOrHaul[complete.cases(AKL.df$Haul.Join) & complete.cases(AKL.df$Port.Join)]<-"both"
## } else {
##   AKL.df$PortOrHaul = "lumped"
## }

#----------------------------------------------------------------------
if (UseExtrapNum==1) {
  #This only works for hauls, of course.
  #***************************************************
  #This commented out code works too, if you don't want to pull from SQL

  # SpComp.df<-read.csv(file = paste0(DataDir,"Catch\\norpac_catch_report.csv"),,skip = 6)
  # #merge AKL.df with SpComp.df
  # SpComp.df<-subset(SpComp.df,select = c("Haul.Join","Year","Gear","NMFS.Area","Extrapolated.Number"))
  # ExLengths.df<-merge(AKL.df,SpComp.df,by = c("Haul.Join","Year","Gear","NMFS.Area"))
  #****************************************************
  ## Dcombo <- readRDS("data/lengths_fishery.RDS") #AgeLength.df is df within this Rdata
  Dcombo <- readRDS('data/lengths_fishery_stratefied.RDS')
  ExLengths.df<-data.frame(Haul.Join = Dcombo$Haul.Join,
                           Extrapolated.Number = Dcombo$EXTRAPOLATED_NUMBER,
                           PortOrHaul = "haul",
                           Year = Dcombo$YEAR,
                           Season = Dcombo$Season,
                           FMP.Subarea = Dcombo$FMP_SUBAREA,
                           NMFS.Area = Dcombo$NMFS_AREA,
                           Gear = Dcombo$GEAR,
                           Sex = Dcombo$SEX1,
                           Length = Dcombo$LENGTH,
                           Frequency = Dcombo$FREQUENCY)
  ## calculate the proportion in each length bin within each
  ## haul, separating by all of the other relevant variables as
  ## well
  ExLengths.df <- ExLengths.df[ExLengths.df$Sex!="U",]
  RawLength.df <- aggregate(Frequency ~ Haul.Join +
                              Extrapolated.Number + PortOrHaul +
                              Season + NMFS.Area + FMP.Subarea +
                              Gear + Year + Sex + Length,
                            ExLengths.df, sum)
  ## total lengths by haul (not sex-specific b.c. Extrap.Num not sex-specific)
  RawNums.df <- aggregate(Frequency ~ Haul.Join +
                            Extrapolated.Number+PortOrHaul +
                            Season + NMFS.Area + FMP.Subarea+
                            Gear + Year,
                          ExLengths.df, sum)
  RawNums.df$Raw.Num <- RawNums.df$Frequency
  RawNums.df <- subset(RawNums.df,select = -c(Frequency))

  ## Proportion at length (and sex) within each haul
  ExProp.df <- merge(RawLength.df,RawNums.df,all = TRUE)
  ExProp.df$Prop <- ExProp.df$Frequency/ExProp.df$Raw.Num

  ## multiply by extrapolated number for that haul
  ExProp.df$LNUMBERS <- ExProp.df$Prop*ExProp.df$Extrapolated.Number
  ExFinal.df <- aggregate(LNUMBERS~PortOrHaul + Season +
                            NMFS.Area + FMP.Subarea + Gear + Year + Sex + Length,
                          ExProp.df, sum)
  LengthComps.df <- data.frame(PortOrHaul = ExFinal.df$PortOrHaul,
                           SEASON = ExFinal.df$Season,
                           NMFS_AREA = ExFinal.df$NMFS.Area,
                           FMP_SUBAREA = ExFinal.df$FMP.Subarea,
                           YEAR = ExFinal.df$Year,
                           SEX = ExFinal.df$Sex,
                           GEAR = ExFinal.df$Gear,
                           LENGTH = ExFinal.df$Length,
                           LNUMBERS = ExFinal.df$LNUMBERS)
} else if (UseExtrapNum!=1) {
  stop("not set up to work with this option")
  ## Put in format to match LengthComps
  RawLength.df <-
    aggregate(Frequency ~ PortOrHaul + Season +
                NMFS.Area + FMP.Subarea + Gear + Year + Sex + Length,
              AKL.df,sum)
  LengthComps.df <- data.frame(PortOrHaul = RawLength.df$PortOrHaul,
                             SEASON = RawLength.df$Season,
                             NMFS_AREA = RawLength.df$NMFS.Area,
                             FMP_SUBAREA = RawLength.df$FMP.Subarea,
                             YEAR = RawLength.df$Year,
                             SEX = RawLength.df$Sex,
                             GEAR = RawLength.df$Gear,
                             LENGTH = RawLength.df$Length,
                             LNUMBERS = RawLength.df$Frequency)
}
#----------------------------------------
## All data:
## Bin Lengths (binned lengths are not used when coming up with
## length comps, so this is just to create a check of length
## comps to compare to previous data):
if (BuckOrSS=="Buck") {
  LengthComps.df<-BIN_LEN_DATA_BUCK(LengthComps.df,LengthBins)
} else {
  LengthComps.df<-BIN_LEN_DATA(LengthComps.df,LengthBins)
}

## Get rid of unsexed lengths - they will not be used as part of the age comps
LengthComps.df <- LengthComps.df[LengthComps.df$SEX != "U",]

## ## this still contains info specific to NMFS area, etc.:
## write.csv(LengthComps.df,paste0(DataDir,"\\Fishery_Lengths\\Observer_LengthCompsSUCK.csv")

#-------------------------------------------------------------

##### Cole commented this out b/c it was not core to the
##### calculations. May need to come back and redo this.

## ##--------------------------------------------------------------
## ## Calculate frequency of samples by groupings to compare to
## ## catch groupings This will help determine whether incorporating
## ## extrapolated number into the length calculations is a
## ## necessary thing.  Compare to total catch groupings that are
## ## calculated using the code
## ## C:\\GitProject\\newsbss\\Assessment_Tables\\Catch_by_Grouping.R
## ## In 2017 these catch data were written out to:
## ## "\\\\AFSC-S79\\REFM_Users\\carey.mcgilliard\\My
## ## Documents\\FlatfishAssessments\\2017\\Rex\\Tables\\CatchByGrouping_1994_onwards\\"
## Tot.df<-aggregate(LNUMBERS ~ YEAR, LengthComps.df, sum)
## Tot.df$Tot <- Tot.df$LNUMBERS
## Tot.df<-subset(Tot.df,select = -c(LNUMBERS))
#---------------------------------------------------------
## ## Do some ggplots to figure out length distributions by groupings and by year.
## LC.df<-aggregate(LNUMBERS ~ YEAR + SEASON + FMP_SUBAREA + NMFS_AREA + GEAR + SEX + BIN,LengthComps.df,sum)
## LC.df<-merge(LC.df,Tot.df,all = TRUE)
## LC.df$Proportion<-LC.df$LNUMBERS/LC.df$Tot
## ##  ggplot(LC.df,aes(x=BIN ,y=YEAR,height = Proportion, fill=GEAR)) + geom_joy(stat = "identity", scale = 1.4,alpha=0.3) + theme_joy() + facet_wrap(~SEASON)
## p <- ggplot(LC.df,aes(BIN, fill = YEAR))
## p + geom_histogram(aes(Proportion))
#---------------------------------------------------------------
## Season.df<-aggregate(LNUMBERS ~ SEASON + YEAR,LengthComps.df,sum)
## Season.df<-merge(Season.df,Tot.df,all=TRUE)
## Season.df$Prop<-round(Season.df$LNUMBERS/Season.df$Tot,digits = 2)
## Flip.Season.df<-dcast(Season.df,YEAR ~ SEASON,sum,value.var = "LNUMBERS")
## Prop.Season.df<-dcast(Season.df,YEAR ~ SEASON,sum,value.var = "Prop")
## GEAR.df<-aggregate(LNUMBERS ~ GEAR + YEAR,LengthComps.df,sum)
## GEAR.df<-merge(GEAR.df,Tot.df,all=TRUE)
## GEAR.df$Prop<-round(GEAR.df$LNUMBERS/GEAR.df$Tot,digits = 2)
## Flip.GEAR.df<-dcast(GEAR.df,YEAR ~ GEAR,sum,value.var = "LNUMBERS")
## Prop.GEAR.df<-dcast(GEAR.df,YEAR ~ GEAR,sum,value.var = "Prop")
## NMFS_AREA.df<-aggregate(LNUMBERS ~ NMFS_AREA + YEAR,LengthComps.df,sum)
## NMFS_AREA.df<-merge(NMFS_AREA.df,Tot.df,all=TRUE)
## NMFS_AREA.df$Prop<-round(NMFS_AREA.df$LNUMBERS/NMFS_AREA.df$Tot,digits = 2)
## Flip.NMFS_AREA.df<-dcast(NMFS_AREA.df,YEAR ~ NMFS_AREA,sum,value.var = "LNUMBERS")
## Prop.NMFS_AREA.df<-dcast(NMFS_AREA.df,YEAR ~ NMFS_AREA,sum,value.var = "Prop")
## ## This is not a choice for processing the age data (FMP_SUBAREA
## ## variable is not carried forward from here, but NMFS areas can
## ## be approximately combined to get the FMP subareas.  This is so
## ## that the code applies better to the BSAI region where FMP
## ## subarea isn't really used for management
## FMP_SUBAREA.df<-aggregate(LNUMBERS ~ FMP_SUBAREA + YEAR,LengthComps.df,sum)
## FMP_SUBAREA.df<-merge(FMP_SUBAREA.df,Tot.df,all=TRUE)
## FMP_SUBAREA.df$Prop<-round(FMP_SUBAREA.df$LNUMBERS/FMP_SUBAREA.df$Tot,digits = 2)
## Flip.FMP_SUBAREA.df<-dcast(FMP_SUBAREA.df,YEAR ~ FMP_SUBAREA,sum,value.var = "LNUMBERS")
## Prop.FMP_SUBAREA.df<-dcast(FMP_SUBAREA.df,YEAR ~ FMP_SUBAREA,sum,value.var = "Prop")
## if (unique(AKL.df$FMP.Area) == "GOA") {
##   Flip.FMP_SUBAREA.df<-Flip.FMP_SUBAREA.df[c("YEAR","WG","CG","SE","WY")]
##   Prop.FMP_SUBAREA.df<-Prop.FMP_SUBAREA.df[c("YEAR","WG","CG","SE","WY")]
## }
## if (unique(AKL.df$FMP.Area) == "BSAI") {
##   Flip.FMP_SUBAREA.df<-Flip.FMP_SUBAREA.df[c("YEAR","BS","AI")]
##   Prop.FMP_SUBAREA.df<-Prop.FMP_SUBAREA.df[c("YEAR","BS","AI")]
## }
## write.csv(Flip.Season.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthCount.Season.ExNum",UseExtrapNum,".csv"))
## write.csv(Flip.GEAR.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthCount.GEAR.ExNum",UseExtrapNum,".csv"))
## write.csv(Flip.NMFS_AREA.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthCount.NMFS_AREA.ExNum",UseExtrapNum,".csv"))
## write.csv(Flip.FMP_SUBAREA.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthCount.FMP_SUBAREA.ExNum",UseExtrapNum,".csv"))
## write.csv(Prop.Season.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthProp.Season.ExNum",UseExtrapNum,".csv"))
## write.csv(Prop.GEAR.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthProp.GEAR.ExNum",UseExtrapNum,".csv"))
## write.csv(Prop.NMFS_AREA.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthProp.NMFS_AREA.ExNum",UseExtrapNum,".csv"))
## write.csv(Prop.FMP_SUBAREA.df,file = paste0(DataDir,"\\Fishery_Lengths\\LengthProp.FMP_SUBAREA.ExNum",UseExtrapNum,".csv"))
## ------------------------------------------------------------------------------------


## Erase any extraneous information: if not accounting for these
## factors, then set all their values to be the same (arbitrarily
## zero)
## Options given above:
## ByGear = 0               #1 = yes, by gear and produces age data in proportions for separate gears: gear types: 1 = nonpelagic trawl, 2 = pelagic trawl, 8 = longline
## BySeason = 0             #1 = yes, sort by season
## ByFmp.Subarea = 0        #1 = yes, sort by CG, WG, WY, SE      (probably don't want By Fmp.Subarea =1 AND ByNMFS.Area = 1)
## ByNMFS.Area = 0          #1 = yes, sort by area 610, 620, etc. (probably don't want By Fmp.Subarea =1 AND ByNMFS.Area = 1)

if (ByPortOrHaul == 0 & SepPortOrHaul == 0)  LengthComps.df$PortOrHaul = 0
## erase GEAR information if you want all gears together for this query; otherwise age data will be by gear
if (ByGear == 0 & SepGear == 0) LengthComps.df$GEAR = 0
if (BySeason == 0) LengthComps.df$SEASON = 0
if (ByNMFS.Area == 0)  LengthComps.df$NMFS_AREA = 0
if (ByFmp.Subarea == 0 & SepFmp.Subarea == 0) LengthComps.df$FMP_SUBAREA = 0
# if (ByFmp.Subarea == 1 & ByNMFS.Area == 1) {
#   LengthComps.df$NMFS_AREA= 0
#   print("not sorting by NMFS_AREA because ByFmp.Subarea = 1.")
# }

#Re-aggregate without extraneous variable values - those set to 0
#will not have any effect on the aggregation because every row =
#0 for that variable:
LengthComps1.df <- aggregate(LNUMBERS ~ PortOrHaul + SEASON +
                               FMP_SUBAREA + NMFS_AREA + GEAR +
                               YEAR + SEX + BIN,
                             LengthComps.df, sum)
LengthComps1.df <- LengthComps1.df[order(LengthComps1.df$PortOrHaul,LengthComps1.df$SEASON,LengthComps1.df$FMP_SUBAREA,LengthComps1.df$NMFS_AREA,LengthComps1.df$GEAR,LengthComps1.df$SEX,LengthComps1.df$YEAR,LengthComps1.df$BIN),]
rm(LengthComps.df)
LengthComps.df<-LengthComps1.df
rm(LengthComps1.df)


## ----------------------------------------------------------------------------------------------
## Get data from database and save .Rdata files; can just load
## the following Rdata files if already ran the up-to-date
## queries:

### Note: Do not use the .csv version. It fails b/c HAUL_JOIN is
## broken when reading in and that breaks the Nsamp calculations
## below. See data/get_agecomps_fishery.R script.
## AgeLength.df <- read.csv('data/Age_Length_Stratified.csv')
AgeLength.df <- readRDS('data/Age_Length_Stratified.RDS')
Ports.df <- read.csv('data/Port_Data.csv')

## Get total ages read by year for report before any filtering
total_Nages  <- AgeLength.df %>% filter(!is.na(AGE)) %>%
  group_by(YEAR) %>% summarize(Nages=length(AGE), .groups='drop')


#Make FMP Areas in case they are needed:
Ports.df$FMP_SUBAREA<-NA
Ports.df$FMP_SUBAREA[Ports.df$NMFS_AREA > 539 & Ports.df$NMFS_AREA <=544]<-"AI"
Ports.df$FMP_SUBAREA[Ports.df$NMFS_AREA >=500 & Ports.df$NMFS_AREA<=539]<-"BS"
Ports.df$FMP_SUBAREA[Ports.df$NMFS_AREA == 610]<-"WG"
Ports.df$FMP_SUBAREA[Ports.df$NMFS_AREA == 630 | Ports.df$NMFS_AREA == 620]<-"CG"
Ports.df$FMP_SUBAREA[Ports.df$NMFS_AREA == 640 | Ports.df$NMFS_AREA == 650]<-"EG"
AgeLength.df$FMP_SUBAREA<-NA
AgeLength.df$FMP_SUBAREA[AgeLength.df$NMFS_AREA > 539 & AgeLength.df$NMFS_AREA <=544]<-"AI"
AgeLength.df$FMP_SUBAREA[AgeLength.df$NMFS_AREA >=500 & AgeLength.df$NMFS_AREA<=539]<-"BS"
AgeLength.df$FMP_SUBAREA[AgeLength.df$NMFS_AREA == 610]<-"WG"
AgeLength.df$FMP_SUBAREA[AgeLength.df$NMFS_AREA == 630 | AgeLength.df$NMFS_AREA == 620]<-"CG"
AgeLength.df$FMP_SUBAREA[AgeLength.df$NMFS_AREA == 640 | AgeLength.df$NMFS_AREA == 650]<-"EG"
#------------------------------------------------------------------------------------------------


## If not sorting by gear type or other stratification types:
##  erase GEAR information if you want all gears together for this query; otherwise age data will be by gear
if (ByGear == 0 & SepGear == 0) AgeLength.df$GEAR = 0
if (BySeason == 0) AgeLength.df$SEASON = 0
if (ByNMFS.Area == 0) AgeLength.df$NMFS_AREA = 0
if (ByFmp.Subarea == 0)  AgeLength.df$FMP_SUBAREA = 0
## get rid of rows where NA in age column
AgeLength.df <- AgeLength.df[!is.na(AgeLength.df$AGE),]

## haul_join is needed, but there is more age data that is not assigned a haul_join.
## AgeLength.df<-AgeLength.df[complete.cases(AgeLength.df$HAUL_JOIN),]
if (ByPortOrHaul == 1 | SepPortOrHaul == 1) {
  AgeLength.df$PortOrHaul<-"none"
  AgeLength.df$PortOrHaul[!is.na(AgeLength.df$HAUL_JOIN)]<-"haul"
  AgeLength.df$PortOrHaul[!is.na(AgeLength.df$PORT_JOIN)]<-"port"
  AgeLength.df$PortOrHaul[!is.na(AgeLength.df$HAUL_JOIN) & !is.na(AgeLength.df$PORT_JOIN)]<-"both"
} else {
  #AgeLength.df$PortOrHaul = "lumped"
  AgeLength.df$PortOrHaul = 0
}

#Bin length data
AgeLength.df <- BIN_LEN_DATA(AgeLength.df,len_bins = LengthBins)



#order the data to see more clearly
#these are still individual fish:
AgeLength.df <- AgeLength.df %>%
  arrange(PortOrHaul, SEASON,FMP_SUBAREA, NMFS_AREA, GEAR, YEAR,
          SEX, BIN, AGE)
AgeLength.df$Freq <-1
#aggregate by: gear, sex, bin, age
FreqTable.df <- aggregate(Freq ~ PortOrHaul + SEASON +
                            FMP_SUBAREA + NMFS_AREA + GEAR + SEX
                          + YEAR + BIN + AGE,
                          AgeLength.df, sum)
FreqTable.df <- FreqTable.df %>%
  arrange(PortOrHaul, SEASON, FMP_SUBAREA, NMFS_AREA, GEAR,
    YEAR, SEX, BIN, AGE)

##-------------------------------------------------------------
## Get information on number of hauls from haul data and port
## data some number of tows per port join are missing; these are
## filled in with the mean number of tows per port_join
## --------------------------------------------------------------
Nhaul_Strat.df <- aggregate(HAUL_JOIN ~ YEAR + SEASON +
                              FMP_SUBAREA + NMFS_AREA + GEAR,
                            data = AgeLength.df,
                            function(x) length(unique(x)))
Nhaul_Strat.df <- Nhaul_Strat.df[order(Nhaul_Strat.df$SEASON,Nhaul_Strat.df$FMP_SUBAREA,Nhaul_Strat.df$NMFS_AREA,Nhaul_Strat.df$GEAR, Nhaul_Strat.df$YEAR),]
Nhaul_Strat.df$NumHauls <- Nhaul_Strat.df$HAUL_JOIN
Nhaul_Strat.df <- subset(Nhaul_Strat.df,select= -c(HAUL_JOIN))
Nhaul.df <- aggregate(HAUL_JOIN ~ YEAR + GEAR + FMP_SUBAREA,
                    data = AgeLength.df,
                    function(x) length(unique(x)))
Nhaul.df <- Nhaul.df[order(Nhaul.df$GEAR, Nhaul.df$YEAR),]
Nhaul.df$NumHauls <- Nhaul.df$HAUL_JOIN
Nhaul.df <- subset(Nhaul.df,select= -c(HAUL_JOIN))



### This was FALSE in 2020 so cleaned up a bit and commented
## out. Need to return and look at this more carefully. -Cole
## Get number of tows per port and number of tows per year
if (length(Ports.df$AGE[!is.na(Ports.df$AGE)])>0) {
  Ports.df<-Ports.df[!is.na(Ports.df$AGE),]
  JustPorts.df<-data.frame(PORT_JOIN = Ports.df$PORT_JOIN,
                         SEASON = Ports.df$SEASON,
                         FMP_SUBAREA = Ports.df$FMP_SUBAREA,
                         NMFS_AREA = Ports.df$NMFS_AREA,
                         GEAR = Ports.df$GEAR,
                         NO_OF_TOWS = Ports.df$NO_OF_TOWS,
                         YEAR = Ports.df$YEAR)
  JPorts.df<-unique(JustPorts.df) #gets rid of duplicates due to mutiple age-lengths sampled at a particular PORT_JOIN
  #what's happening here - are port_joins repeated over years? make a variable PORT_JOIN_YEAR.
  #need to do more exploration to determine how to handle NAs for port_join - take an average or median for # of tows per port?
  MeanTows<-mean(JPorts.df$NO_OF_TOWS[!is.na(JPorts.df$NO_OF_TOWS)])
  MedianTows<-median(JPorts.df$NO_OF_TOWS[!is.na(JPorts.df$NO_OF_TOWS)])
  #9.3 or 8.5, so looks like 9 tows per port_join would be reasonable
  JPorts.df$NO_OF_TOWS[is.na(JPorts.df$NO_OF_TOWS)] = round(MeanTows,0)
  #can add this into number of hauls
  if (ByGear == 0 & SepGear == 0) JPorts.df$GEAR = 0
  if (BySeason == 0) JPorts.df$SEASON = 0
  if (ByNMFS.Area == 0)JPorts.df$NMFS_AREA = 0
  if (ByFmp.Subarea == 0) JPorts.df$FMP_SUBAREA = 0
  #this is port_haul and number_of_tows through 2007 only.
  NumPortHaulsEarly.df<-aggregate(NO_OF_TOWS ~ YEAR + SEASON + FMP_SUBAREA + NMFS_AREA + GEAR,JPorts.df,sum)
  ## would need to subset to just port samples before merging and merging is only a check.
## pj.df<-merge(AgeLength.df,Ports.df,all = T)
## This makes 1 row for each port_join
## UPorts.df<-aggregate(Freq~YEAR + PORT_JOIN + GEAR + NO_OF_TOWS,Ports.df,sum)
## UPorts.df<-UPorts.df[order(UPorts.df$YEAR,UPorts.df$PORT_JOIN,UPorts.df$NO_OF_TOWS,UPorts.df$GEAR),]
##
## Nport_M_F.df<-aggregate(PORT_JOIN ~ YEAR + GEAR + SEX,data = AgeLength.df,function(x) length(unique(x)))
## Nport_M_F.df<-Nport_M_F.df[order(Nport_M_F.df$SEX, Nport_M_F.df$GEAR, Nport_M_F.df$YEAR),]
## Nport_M_F.df$NumPorts<-Nport_M_F.df$PORT_JOIN
## Nport_M_F.df<-subset(Nport_M_F.df,select = -c(PORT_JOIN))
  Nport.df<-aggregate(PORT_JOIN ~ YEAR + SEASON + FMP_SUBAREA + NMFS_AREA + GEAR,data = AgeLength.df,function(x) length(unique(x)))
  Nport.df<-Nport.df[order(Nport.df$SEASON,Nport.df$FMP_SUBAREA,Nport.df$NMFS_AREA,Nport.df$GEAR, Nport.df$YEAR),]
  Nport.df$Numports<-Nport.df$PORT_JOIN
  Nport.df<-subset(Nport.df,select= -c(PORT_JOIN)) #gets you number of ports after 2007
  LatePorts.df<-Nport.df[Nport.df$YEAR >2007,]
  LatePorts.df$NO_OF_TOWS<-MeanTows*LatePorts.df$Numports
  LatePorts.df<-subset(LatePorts.df,select = -c(Numports))
  #LatePorts.df$PortOrHaul<-"port"
  NumPortHauls_Strat.df<-rbind(NumPortHaulsEarly.df,LatePorts.df)
  NumPortHauls.df<-aggregate(NO_OF_TOWS ~ GEAR + FMP_SUBAREA + YEAR,NumPortHauls_Strat.df,sum)
  #-------------------------------------------------------
} #end if there is age data from ports
if (exists('JPorts.df')) {
  ##  if (nrow(JPorts.df)>0) { # check to see if using exists works if there IS port data
  CompleteHauls.df<-merge(Nhaul.df,NumPortHauls.df, all = T)
} else {
  CompleteHauls.df<-Nhaul.df
}
## Turn NAs to 0s
CompleteHauls.df$NumHauls[is.na(CompleteHauls.df$NumHauls)]<-0

if (exists('CompleteHauls.df$NO_OF_TOWS')) {
  CompleteHauls.df$NO_OF_TOWS[is.na(CompleteHauls.df$NO_OF_TOWS)]<-0
  CompleteHauls.df$TotHauls<-CompleteHauls.df$NumHauls + CompleteHauls.df$NO_OF_TOWS
  NumPortHauls.df$Hauls<-NumPortHauls.df$NO_OF_TOWS
  NumPortHauls.df$PortOrHaul<-"port"
  NumPortHauls.df<-subset(NumPortHauls.df,select = -c(NO_OF_TOWS))
  #NumPortHauls.df<-NumPortHauls.df[order(NumPortHauls.df$PortOrHaul,NumPortHauls.df$GEAR,NumPortHauls.df$YEAR),]
} else {
  CompleteHauls.df$TotHauls <- CompleteHauls.df$NumHauls
  Nhaul.df$Hauls <- Nhaul.df$NumHauls
  Nhaul.df$PortOrHaul <-"haul"
  Nhaul.df <- subset(Nhaul.df,select = -c(NumHauls))
  #Nhaul.df<-Nhaul.df[order(Nhaul.df$PortOrHaul,Nhaul.df$GEAR,Nhaul.df$YEAR),]
}
#Actually, you want this organized in long form to merge at the bottom:

Lumped.df <- data.frame(YEAR=CompleteHauls.df$YEAR,
                        GEAR=CompleteHauls.df$GEAR,
                        FMP_SUBAREA=CompleteHauls.df$FMP_SUBAREA,
                        Hauls=CompleteHauls.df$TotHauls)
Lumped.df$PortOrHaul<-"lumped"

if (exists('NumPortHauls.df')) {
  AllHaul.df<-rbind(Nhaul.df,NumPortHauls.df,Lumped.df)
} else {
  AllHaul.df<-rbind(Nhaul.df,Lumped.df)
}
AllHaul.df <- AllHaul.df %>% arrange(PortOrHaul, GEAR, FMP_SUBAREA, YEAR)
#--------------------------------------------------------------------------------------


## Figure out the proportion at each age within a length bin
## Need to sum over ages within length bin
SumFreq.df <- aggregate(Freq ~ PortOrHaul + YEAR + GEAR +
                        FMP_SUBAREA + NMFS_AREA + SEASON + SEX + BIN,
                      FreqTable.df, sum)
SumFreq.df <- SumFreq.df[order(SumFreq.df$PortOrHaul,SumFreq.df$YEAR,SumFreq.df$GEAR,SumFreq.df$FMP_SUBAREA,SumFreq.df$NMFS_AREA,SumFreq.df$SEASON,SumFreq.df$SEX,SumFreq.df$BIN),]
SumFreq.df$SumFreq <- SumFreq.df$Freq
SumFreq.df <- subset(SumFreq.df,select = -c(Freq))
## merge sumfreq on to calculate proportions
Prop.df <- merge(FreqTable.df,SumFreq.df,all = T)
Prop.df$Proportion <- Prop.df$Freq/Prop.df$SumFreq
Prop.df <- subset(Prop.df,select = -c(Freq,SumFreq))

## Merge with length comps
m.df <- merge(Prop.df, LengthComps.df, all = TRUE)
m.df <- m.df[!is.na(m.df$AGE),]
#Change NA's to 0's:
#m.df[is.na(m.df)] <- 0

## Multiply proportions by length comp in each bin (do you have
## this by gear? it would be good)
m.df$LengthAgeComp <- m.df$Proportion*m.df$LNUMBERS
## still stratified here and therefore multiplied by proportions
## within each local stratum by length comps within that stratum


## Carey is here in implementing PortOrHaul as a stratification
## option; see what's happening for gear as well.
if (SepPortOrHaul == 0 & ByPortOrHaul == 1) m.df$PortOrHaul = "StandardizedThenLumped"
if (SepPortOrHaul == 0 & ByPortOrHaul == 0) m.df$PortOrHaul = "NotStandardized"
if (SepGear == 0 & ByGear == 1) m.df$GEAR = "StandardizedThenLumped"
if (SepGear == 0 & ByGear == 0) m.df$GEAR = "NotStandardized"
if (SepFmp.Subarea == 0 & ByFmp.Subarea==1) m.df$FMP_SUBAREA = "StandardizedThenLumped"
if (SepFmp.Subarea == 0 & ByFmp.Subarea==0) m.df$FMP_SUBAREA = "NotStandardized"

## Drop the stratification here for total age comp:
AgeComp.df<-aggregate(LengthAgeComp ~ PortOrHaul + YEAR +
                        FMP_SUBAREA + GEAR + SEX + AGE,
                      m.df,sum)
AgeComp.df<-AgeComp.df[order(AgeComp.df$PortOrHaul,AgeComp.df$YEAR,AgeComp.df$FMP_SUBAREA,AgeComp.df$GEAR,AgeComp.df$SEX,AgeComp.df$AGE),]
BuckComp.df<-BIN_AGE_DATA(AgeComp.df,age_bins=AgeBins)
#aggregate and write out numbers for Buck's assessment:
## write.csv(BuckComp.df,file = paste0(DataDir,"Observer_AgeComps_LongBuck_ByGear",ByGear,"_SepGear",SepGear,"_BySeason",BySeason,"_ByFmpSubarea",ByFmp.Subarea,"_ByNMFSArea",ByNMFS.Area,"_ByPortOrHaul",ByPortOrHaul,"_SepPortOrHaul",SepPortOrHaul,"_ExNum",UseExtrapNum,".csv"))

## SS wants comps such that males + females sum to 1 in
## proportions the way Carey does assessments:
SumAges.df <- aggregate(LengthAgeComp ~ PortOrHaul + YEAR + FMP_SUBAREA + GEAR,AgeComp.df,sum)
SumAges.df$SumOverAges <- SumAges.df$LengthAgeComp
SumAges.df <- subset(SumAges.df, select = -c(LengthAgeComp))
AgeComp.df <- merge(AgeComp.df, SumAges.df, all = TRUE)
AgeComp.df$Proportion <- AgeComp.df$LengthAgeComp/AgeComp.df$SumOverAges
AgeComp.df <- subset(AgeComp.df,select = -c(LengthAgeComp,SumOverAges))
AgeComp.df <- BIN_AGE_DATA(AgeComp.df,age_bins = AgeBins)
AgeComp.df <- subset(AgeComp.df,select = c(PortOrHaul,YEAR,FMP_SUBAREA,GEAR,SEX,aBIN,Proportion))
AgeComp.df <- AgeComp.df[order(AgeComp.df$PortOrHaul,AgeComp.df$YEAR,AgeComp.df$FMP_SUBAREA,AgeComp.df$GEAR,AgeComp.df$SEX,AgeComp.df$aBIN,AgeComp.df$aBIN),]
#Write out the long form version of fishery age comps
## write.csv(AgeComp.df,file = paste0(DataDir,"Observer_AgeComps_LongForm_ByGear",ByGear,"_SepGear",SepGear,"_BySeason",BySeason,"_ByFmpSubarea",ByFmp.Subarea,"_ByNMFSArea",ByNMFS.Area,"_ByPortOrHaul",ByPortOrHaul,"_SepPortOrHaul",SepPortOrHaul,"_ExNum",UseExtrapNum,".csv"))

#-----------------------------------------------
#potential improvement
#Expand.grid or something to get all ages, no categories missing (?)
#or could do this manually...it's easy enough manually.
#---------------------------------------------------
#
#Flip aBIN up to be a column with females then males across columns
#make sex_by_age a column
## AgeComp.df$SexNum<-as.numeric(AgeComp.df$SEX=="M")
## AgeComp.df$SexAge<-as.numeric(paste0(AgeComp.df$SexNum,0,AgeComp.df$aBIN))
## AgeCompNew.df<-subset(AgeComp.df,select = -c(aBIN,SEX))


#---------------------------------------------------
#
#Flip aBIN up to be a column with females then males across columns
#make sex_by_age a column
AgeComp.df$SexNum <- as.numeric(AgeComp.df$SEX=="M")
AgeComp.df$SexAge <- as.numeric(paste0(AgeComp.df$SexNum,0,AgeComp.df$aBIN))
AgeCompNew.df <- subset(AgeComp.df,select = -c(aBIN,SEX))

## Pivoting to wide format. Old code used reshape2::dcast but
## that is pretty outdated so switched to pivot_wider
FlipComp1.df <- reshape2::dcast(AgeCompNew.df,PortOrHaul + YEAR +
                                    FMP_SUBAREA + GEAR  ~
                                      SexAge,sum,value.var =
                                                   "Proportion")
## test <- AgeCompNew.df %>% select(-SexNum) %>% #arrange(SexAge) %>%
##   pivot_wider(names_from=SexAge, values_from=Proportion,
##               values_fn=sum, values_fill=0) %>%
##   arrange(YEAR, FMP_SUBAREA,  GEAR) %>% as.data.frame()
## all.equal(test, FlipComp1.df)
FlipHaul.df <- reshape2::dcast(AllHaul.df,YEAR + FMP_SUBAREA + GEAR  ~ PortOrHaul,value.var = "Hauls")
FlipComp.df <- merge(FlipHaul.df,FlipComp1.df,all.y = TRUE)
FlipComp.df <- FlipComp.df[order(FlipComp.df$PortOrHaul,FlipComp.df$FMP_SUBAREA,FlipComp.df$GEAR,FlipComp.df$YEAR),]

### Cole modified this
## Subset down to what is used for FHS
AllHaul.df <- AllHaul.df %>% filter(FMP_SUBAREA=='BS' & PortOrHaul=='haul' & GEAR==1)
AgeComp.df <- AgeComp.df %>%
  filter(FMP_SUBAREA=='BS' & PortOrHaul=='haul' & GEAR==1) %>%
  ## Because of the plus group I need to sum across the last age bin
  group_by(FMP_SUBAREA, YEAR, SEX, aBIN, PortOrHaul) %>%
  summarize(Proportion=sum(Proportion), .groups='drop')

## Merge in all combinations to fill in missing year/bin combos
temp <- with(droplevels(AgeComp.df),
             expand.grid(PortOrHaul=unique(PortOrHaul),
                         FMP_SUBAREA=unique(FMP_SUBAREA),
                         YEAR=unique(YEAR), SEX=unique(SEX),
                         aBIN=AgeBins))
## Merge in and replace with zeroes
AgeCompNew.df <- merge(temp, AgeComp.df, all=TRUE)
AgeCompNew.df$Proportion[is.na(AgeCompNew.df$Proportion)] <- 0

### Merge in the number of hauls which is Nsamp in SS
AgeCompNew.df <- merge(AgeCompNew.df, AllHaul.df, all.x=TRUE, all.y=TRUE)
## Simplify down to what we want for FHS
Ages <- AgeCompNew.df %>% select(-PortOrHaul, -GEAR, -FMP_SUBAREA) %>% droplevels() %>%
  arrange(YEAR, SEX, aBIN)
## ## Check
## Ages %>% group_by(YEAR) %>% summarize(total=sum(Proportion), Nsamp=Hauls[1])

## Now massage the bins for SS.
Ages$aBIN <- paste0(as.character(Ages$SEX),Ages$aBIN)
Mages <- filter(Ages, SEX=='M') %>%
  pivot_wider(names_from=aBIN, values_from=Proportion)
Fages <- filter(Ages, SEX=='F') %>%
  pivot_wider(names_from=aBIN, values_from=Proportion)

SS_agecomps_fishery <-
  data.frame(year=Mages$YEAR, month=7, fleet=1, sex=3,
             part=0, ageerr=1, Lbin_lo=-1, Lbin_hi=-1,
             Nsamp=Mages$Hauls, Fages[,-(1:3)], Mages[,-(1:3)])
stopifnot(all(0==round(abs(1-rowSums(SS_agecomps_fishery[,-(1:9)])),10)))
## Carey excluded years <2000 b/c of small samples sizes. Set
## fleet to -1 so it's a ghost
SS_agecomps_fishery$fleet[SS_agecomps_fishery$year<2000] <- -1
names(SS_agecomps_fishery) <- names(SS_dat$agecomp)
## options(digits=7)


write.csv(SS_agecomps_fishery, file='inputs/SS_agecomps_fishery.csv', row.names=FALSE)

message("Calculating total ages read by year and saving to report folder...")
stopifnot(all(Mages$YEAR == total_Nages$YEAR))
write.csv( data.frame(total_Nages, hauls=Mages$Hauls), 'report/agecomp_fishery_Nages.csv', row.names=FALSE)

