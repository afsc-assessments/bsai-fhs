## Modified by Cole starting 7/2020 to work with 2020_BSAI_flathead

## CRM 10/6/17 (and used in 2015 as well before newsbss); Steve
## Barbeaux and Teresa A'mar wrote original code and CRM modified
## it to be a stand-alone piece.  Formats racebase specimen data
## for entry to SS 3.24 for conditional age-at-length input
## Get_Survey_Length_Age_and_Plot.R pulls the SpecimenFile from
## the database and saves it Get_Survey_Length_Age_and_Plot.R is
## also used to plot length at age information using ggplot as
## background info for the assessment The AL.df file uses
## ABUNDANCE_HAUL = 'Y', which excludes non-standardized data
## (i.e. 1970's length and age data)


## For EBS: from query written by Dan Nichol (listed in
## Get_EBS_Survey_Length_Age_and_Plot.R and in your email):

message("Processing CAAL data for EBS flathead")
## Read in data (SQL query follows), and if have already read in
## data, can use the following: also, if already ran this code
## there is a file further down that you can open rather than
## this one.

## Both species BF and FHS are included:
if (WhichSpecies=="both") {
  AL.df <- AL.df[AL.df$SPECIES_CODE==species1 | AL.df$SPECIES_CODE==species2,]
  sp_label = "both"
}
if (WhichSpecies==species1) {
	AL.df<-AL.df[AL.df$SPECIES_CODE==species1,]
	sp_label = species1
}
if (WhichSpecies == species2) {
	AL.df<-AL.df[AL.df$SPECIES_CODE==species2,]
	sp_label = species2
}
## get tskypehe subareas that you want:
if (ebsstandard=="standard")
  AL.df <- AL.df[AL.df$SUBAREA <=6,]
if (ebsstandard == "plusNW")
  AL.df <- AL.df[AL.df$SUBAREA <=9,]

## Bin data into length and age bins. There is an age of 0 that
## results in NA bin so drop it, also drop unsexed fish
AL.df <- filter(AL.df, !is.na(AGE) & AGE>0 & !is.na(LENGTH) & SEX!=3)
AL.df <- AL.df[AL.df$AGE>0 & !is.na(AL.df$AGE) & !is.na(AL.df$LENGTH),]
AL.df <- BIN_AGE_DATA(AL.df,age_bins)
AL.df$LENGTH <- round(AL.df$LENGTH/10,0) # mm to cm
## Note that teh caal length bins are not the same as those used
## in the length compositions
AL.df <- BIN_LEN_DATA(AL.df,len_bins=caal_len_bins)
AL.df$age_bin <- AL.df$aBIN

## ---------------------------------------------------------------
## Calculate conditional age-at-length separately by sex
caal <- AL.df %>%
  group_by(YEAR, SEX, aBIN, BIN) %>%
  summarize(Num_Fish=length(SPECIMENID), .groups='drop') %>%
  ## arange by AGE so bins are in right order below
  rename(AGE=aBIN) %>% arrange(AGE) %>%
  group_by(SEX, YEAR, BIN) %>%
  mutate(Nsamp=sum(Num_Fish)) %>%
  pivot_wider(names_from=AGE, values_from=Num_Fish,
              names_prefix='a', values_fill=0)
## Just being careful the columns are in right order
stopifnot(all(names(caal)[-(1:4)] == paste0('a', age_bins)))

## Build SS structures
SS_caal_survey <-
  data.frame(year=caal$YEAR, Month=7, Fleet=2,
             sex=ifelse(caal$SEX==1,2,1), ## SS sex is reversed
             Part=0, Ageerr=1,
             Lbin_lo=caal$BIN, Lbin_hi=caal$BIN,
             Nsamp=caal$Nsamp,
             ## Double up b/c SS needs dummy columns
             caal[,-(1:4)], caal[,-(1:4)]) %>%
    arrange(sex, year, Lbin_lo)
names(SS_caal_survey) <- names(SS_dat$agecomp)

stopifnot(ncol(SS_caal_survey)==9+length(age_bins)*2)
## --------------------------------------------------------------------

### A bunch of old plots I will clean up later
## ### Massage data a bit. Some of these are just used in plots below
## AL.df$START_TIME <- as.Date(AL.df$START_TIME,format = "%m/%d/%Y")
## AL.df$Quarters<-lubridate::quarter(AL.df$START_TIME)
## AL.df$Months<-lubridate::month(AL.df$START_TIME)
## stopifnot(all(AL.df$YEAR == lubridate::year(AL.df$START_TIME)))
## AL.df$Cohort<-AL.df$YEAR - AL.df$AGE
## ##--------------------------------------------------------------------
## ##Length-At_Age Plots
## ##--------------------------------------------------------------------
## ## Jitter ages:
## AL.df$Age <- AL.df$aBIN
## AL.df$Length <- AL.df$LENGTH/10
## AL.df$Age <- AL.df$Age + runif(n =nrow(AL.df),min = 0, max = 0.25)
## AL.df$Length <- AL.df$Length + runif(n =nrow(AL.df),min = 0, max = 0.25)
## AL.df <- AL.df[AL.df$SEX!=3,]
## AL.df$Sex[AL.df$SEX==1] <- "Male"
## AL.df$Sex[AL.df$SEX==2] <- "Female"
## AL.df$depth <- cut(AL.df$BOTTOM_DEPTH, breaks=seq(0,250, by=20))
## p <- ggplot(AL.df,aes(x = Age, y = Length)) +
##   geom_point(alpha=.5, aes(colour=factor(Cohort)),
##              stat="identity") + theme_bw()

## ## Growth by year
## p + facet_wrap(~ YEAR)
## ## p + facet_grid(~Sex)
## #p + facet_grid(Sex~ GrowthMorph)
## #dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Sex_GrowthMorph.png"))
## #dev.off()
## ## p + facet_grid(SUBAREA ~ Sex)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Sex_Subarea_sp_",sp_label,".png"))
## ## dev.off()
## #Growth by depth
## ## p + facet_wrap(~ depth) #could use some binning
## #Growth by INPFC Area
## ## p + facet_grid(NMFS_AREA~Sex)
## ## # #p + facet_wrap(~REGULATORY_AREA_NAME)
## ## # p + facet_grid(INPFC_AREA ~ MAX_DEPTH)
## ## # #p + facet_grid(REGULATORY_AREA_NAME ~ MAX_DEPTH)
## ## # p + facet_grid(YEAR~ MAX_DEPTH) + theme(panel.grid.major = element_line())
## ## # p + facet_grid(YEAR ~ INPFC_AREA)
## ## # p + facet_grid(YEAR ~ REGULATORY_AREA_NAME)
## ## # p + facet_grid(YEAR ~ Sex)
## ## #by sex (both species)
## ## AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) +
## ##   geom_point(data = NULL, aes(colour = factor(Cohort)),stat =
## ##                                                          "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~YEAR)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
## ## dev.off()
## ## AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~YEAR)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
## ## dev.off()
## ## #by NMFS-Area
## ## AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~NMFS_AREA)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_NMFS_Area_sp_",sp_label,".png"))
## ## dev.off()
## ## AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~NMFS_AREA)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_NMFS_Area_sp_",sp_label,".png"))
## ## dev.off()
## ## #by survey subarea
## ## AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~SUBAREA)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Subarea_sp_",sp_label,".png"))
## ## dev.off()
## ## AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~SUBAREA)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Subarea_sp_",sp_label,".png"))
## ## dev.off()
## ## #by Month
## ## AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~Months)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Month_sp_",sp_label,".png"))
## ## dev.off()
## ## AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## ## facet_wrap(~Months)
## ## dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Month_sp_",sp_label,".png"))
## ## dev.off()
## #--------------------------------------------------------------------
## #Weight-At_Age Plots
## #--------------------------------------------------------------------
## # w<-ggplot(AL.df,aes(x = AGE, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))
## # w + facet_wrap(~ YEAR)
## #AL.df<-AL.df[complete.cases(AL.df$YEAR)==T,]
## #by year and sex
## ALweight.df <- AL.df[complete.cases(AL.df$WEIGHT),]
## ALweight.df %>% filter(SEX == 2) %>%
##   ggplot(aes(x = AGE, y = WEIGHT)) +
##   geom_point(aes(colour = factor(Cohort)))+
##   theme(panel.grid.major = element_line(colour = "grey")) +
##   facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
## dev.off()
## ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
## dev.off()
## #by NMFS Area and sex
## ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~NMFS_AREA)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_NMFS_Area_sp_",sp_label,".png"))
## dev.off()
## ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~NMFS_AREA)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_NMFS_Area_sp_",sp_label,".png"))
## dev.off()
## #by survey subarea
## ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~SUBAREA)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Subarea_sp_",sp_label,".png"))
## dev.off()
## ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~SUBAREA)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Subarea_sp_",sp_label,".png"))
## dev.off()
## #by month
## ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~Months)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Month_sp_",sp_label,".png"))
## dev.off()
## ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~Months)
## dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Month_sp_",sp_label,".png"))
## dev.off()
## # w + facet_wrap(~REGULATORY_AREA_NAME)
## # w + facet_wrap(~INPFC_AREA)
## # w + facet_grid(REGULATORY_AREA_NAME~YEAR)
## # w + facet_grid(MAX_DEPTH ~ YEAR)
## #--------------------------------------------------------------------
## #Weight-Length relationships
## #--------------------------------------------------------------------
## wl<-ggplot(ALweight.df,aes(x = LENGTH, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))
## wl + facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_WtLength_by_Cohort_Year_sp_",sp_label,".png"))
## dev.off()
## # wl + facet_wrap(~REGULATORY_AREA_NAME)
## # wl + facet_wrap(~MAX_DEPTH)
## #--------------------------------------------------------------------
## #Mean weight-at-age by year
## #--------------------------------------------------------------------
## MeanWA.df<-aggregate(WEIGHT ~ YEAR + SEX + AGE +Cohort,ALweight.df,mean)
## MeanWA.df$MeanWeight<-MeanWA.df$WEIGHT/1000
## MeanWA.df<-subset(MeanWA.df,select = -c(WEIGHT))
## MeanWA.df<-MeanWA.df[order(MeanWA.df$YEAR,MeanWA.df$AGE),]
## # wa<-ggplot(MeanWA.df,aes(x = AGE, y = MeanWeight))
## # wa + geom_line(data = NULL,aes(colour = YEAR))
## wa<-ggplot(MeanWA.df,aes(x=AGE,y=MeanWeight)) + geom_line(size=1) + ylab("Mean Weight (kg)")
## wa + facet_grid(SEX~YEAR)
## #By sex and year
## MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Females_Year_sp_",sp_label,".png"))
## dev.off()
## MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Males_Year_sp_",sp_label,".png"))
## dev.off()
## #didn't disaggregate by subarea - would need to do another aggregate function to get this.
## # #By survey subarea
## # MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## # facet_wrap(~SUBAREA)
## # dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Females_Subarea_sp_",sp_label,".png"))
## # dev.off()
## # MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## # facet_wrap(~SUBAREA)
## # dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Males_Subarea_sp_",sp_label,".png"))
## # dev.off()
## #By Cohort, sex, and year
## MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
## dev.off()
## MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
## facet_wrap(~YEAR)
## dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
## dev.off()
## ## AL.df %>% group_by(YEAR, SEX) %>% filter(YEAR>2014) %>%
## ##   mutate(mean.age=mean(AGE), mean.length=mean(LENGTH)) %>%
## ## ggplot(aes(LONGITUDE, LATITUDE, size=AGE)) + geom_point() +
## ##   facet_wrap(YEAR~SEX)
## ##----------------------------------------------------------------------------
