## This is a function I got from Steve in 2020 to extract SARA
## info from an SS run. -Cole


GET_SARA<-function(dir="K:\\teresa\\Pacific cod\\2015 SAFE\\models\\Nov models\\Model 2\\run B1",Enter_Data=TRUE,STOCK="Pacific cod",REGION="GOA",TIER="3a",T2="none",UPDATE="benchmark",LH=2,AF=3,AL=4,CD=5,AD=2,MFR=1000,NOTES="Age-0 recruits estimated through 2009 only.",stryr=1977)
{
  require(r4ss)
  require(stringr)
  setwd(dir)
  data<-SS_output(dir=dir,forecast=F,covar=T)
  input<-SS_readdat("data.ss_new",version="3.30")
  starter=SS_readstarter("starter.ss_new")
  names(data$timeseries)<-str_replace_all(names(data$timeseries),":",".")
  names(data$endgrowth)<-str_replace_all(names(data$endgrowth),":",".")
  names(data$sprseries)<-str_replace_all(names(data$sprseries),"=",".")
  names(data$sprseries)<-str_replace_all(names(data$sprseries),"-",".")

  fish_name<-as.character(data$definitions[,9])[1:(data$nfishfleets)]
  survey_name<-as.character(data$definitions[2,])[(data$nfishfleets+2):ncol(data$definitions)]
  Nage_LY<-subset(data$natage,data$natage[,11]=="B"&data$natage$Yr==data$endyr)

  if(Enter_Data){

    STOCK  <-readline("Enter Stock Name: " )
    REGION <- readline("Enter Region (AI AK BOG BSAI EBS GOA SEO WCWYK): ")
    TIER <- readline("Enter Tier TIER (1a 1b 2a 2b 3a 3b 4 5 6): " )
    T2<- readline("Enter if mixed tiers (none 1a 1b 2a 2b 3a 3b 4 5 6): ")
    NOTES<-readline("Enter any notes on the assessment: ")
    MFR<- readline("Enter multiplier for recruitment data, N at age, and survey number (1,1000,1000000): ")
}

  prolog=paste(
    STOCK,"     # stock \n",
    REGION,"       # region     (AI AK BOG BSAI EBS GOA SEO WCWYK)\n",
    data$endyr, "       # ASSESS_YEAR - year assessment is presented to the SSC \n",
    TIER, "         # TIER  (1a 1b 2a 2b 3a 3b 4 5 6) \n",
    T2, "       # TIER2  if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6) \n",
    sep="")

  write(noquote(prolog),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=45, append=F)

  data1<-paste(
  round(data$derived_quants$Value[data$derived_quants$Label==paste("SSB_",data$endyr,sep="")]-(1.96*data$derived_quants$StdDev[data$derived_quants$Label==paste("SSB_",data$endyr,sep="")])),  "      # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year \n",
  round(data$derived_quants$Value[data$derived_quants$Label==paste("SSB_",data$endyr,sep="")]+(1.96*data$derived_quants$StdDev[data$derived_quants$Label==paste("SSB_",data$endyr,sep="")])),  "      # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year \n",
  round(data$derived_quants$Value[data$derived_quants$Label=="SSB_MSY"]),  "      # BMSY  is equilibrium spawning biomass at MSY (Tiers 1-2) or 7/8 x B40% (Tier 3) \n",
  "SS         # MODEL - Required only if NMFS toolbox software used; optional otherwise \n",
  data$SS_versionshort, "   # VERSION - Required only if NMFS toolbox software used; optional otherwise \n",
  data$nsexes,"          # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE) \n",
  data$nfishfleets, "          # number of fisheries \n",
  MFR, "       # multiplier for recruitment, N at age, and survey number (1,1000,1000000)\n",
  "0          # recruitment age used by model \n",
  starter$min_age_summary_bio, "          # age+ used for biomass estimate \n",
  "#FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...) \n",
  paste(fish_name,collapse=" ")," \n",
  "#FISHERYYEAR -list years used in model \n",
  paste(data$recruit$Yr[data$recruit$era!="Forecast"&data$recruit$Yr>=stryr&data$recruit$Yr<=data$endyr],collapse=" ") ," \n",
  "#AGE -list ages used in model \n",
  paste(data$agebins,collapse=" ") ," \n",
  "#RECRUITMENT -Number of recruits by year (see multiplier above) \n",
  paste(data$recruit$pred_recr[data$recruit$era!="Forecast"&data$recruit$Yr>=stryr & data$recruit$Yr<=data$endyr],collapse=" ") ," \n",
  "#SPAWNBIOMASS -Spawning biomass by year in metric tons \n",
  paste(data$recruit$SpawnBio[data$recruit$era!="Forecast"&data$recruit$Yr>=stryr & data$recruit$Yr<=data$endyr]/2,collapse=" ") ," \n",
  "#TOTALBIOMASS -Total biomass by year in metric tons (see age+ above) \n",
  paste(data$timeseries$Bio_smry[data$timeseries$Era=="TIME"&data$timeseries$Yr>=stryr & data$timeseries$Yr<=data$endyr],collapse=" ") ," \n",
  "#TOTFSHRYMORT -Fishing mortality rate by year \n",
  paste(data$sprseries$F.Z.M[data$sprseries$Yr>=stryr & data$sprseries$Yr<=data$endyr],collapse=" ")," \n",
  "#TOTALCATCH -Total catch by year in metric tons \n",
  paste(data$sprseries$Dead_Catch_B,collapse=" ")," \n",
  "#FISHERYMORT -Fishing mortality rates by year (a line for each fishery) only if multiple fisheries",
  sep="")

  write(noquote(data1),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=100, append=T)


  for( j in 1: max(data$timeseries$Seas)){
    for ( i in 1:data$nfishfleets){
     x<-get(noquote(paste("F._",i,sep="")),pos=data$timeseries)[data$timeseries$Era=="TIME"&data$timeseries$Seas==j & data$timeseries$Yr>=stryr & data$timeseries$Yr<=data$endyr]
     write(paste(paste(x,collapse=" "),paste(" # ",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}

  write("#FISHERYCATCH -Catches by year (a line for each fishery) only if multiple fisheries",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)

  for( j in 1: max(data$timeseries$Seas)){
    for ( i in 1:data$nfishfleets){
      x<-get(noquote(paste("obs_cat._",i,sep="")),pos=data$timeseries)[data$timeseries$Era=="TIME"&data$timeseries$Seas==j & data$timeseries$Yr>=stryr & data$timeseries$Yr<=data$endyr]
      write(paste(paste(x,collapse=" "),paste(" # ",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}

  write("#MATURITY -Maturity ratio by age",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  for( i in 1:max(data$endgrowth$Seas)){
   x=data$endgrowth$Age_Mat[data$endgrowth$Sex==1&data$endgrowth$Real_Age>0 & data$endgrowth$Seas==i]*data$endgrowth$Len_Mat[data$endgrowth$Sex==1&data$endgrowth$Real_Age>0&data$endgrowth$Seas==i]
   write(paste(paste(x,collapse=" "),paste(" # Season ",i,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
  }

  write("#SPAWNWT -Average Spawning weight (in kg) by age",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  for( i in 1:max(data$endgrowth$Seas)){
    x=data$endgrowth$Wt_Mid[data$endgrowth$Sex==1&data$endgrowth$Real_Age>0&data$endgrowth$Seas==i]
    write(paste(paste(x,collapse=" "),paste(" # Season ",i,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }
  write("#NATMORT -Natural mortality rate by age (a line for each sex)",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  for(j in 1 : max(data$endgrowth$Seas)){
    for ( i in 1:data$nsexes){
      x=data$endgrowth$M[data$endgrowth$Sex==i&data$endgrowth$Real_Age>0&data$endgrowth$Seas==j]
       write(paste(paste(x,collapse=" "),paste(" # Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}


  write("#N_AT_AGE -N at age by age (see number multiplier above)(a line for each sex)",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  for( j in 1:max(Nage_LY$Seas)){
    for(i in 1:data$nsexes){
      Nage<-subset(Nage_LY,Nage_LY$Seas==j)
      x = as.numeric(Nage[i,13:ncol(Nage)])
      write(paste(paste(x,collapse=" "),paste(" # Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
  }}

  write("#FSHRY_WT_KG -Fishery weight at age (in kg) first FEMALES/ALL (a line for each fishery) then MALES (a line for each fishery)",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)

  if(data$nsexes>1){
    for(j in 1:max(data$ageselex$Seas)){
    for ( i in 1:data$nfishfleets){
      x<-get(noquote(paste("SelWt._",i,sep="")),pos=data$endgrowth)[data$endgrowth$Real_Age>0&data$endgrowth$Sex==1&data$endgrowth$Seas==j]
       write(paste(paste(x,collapse=" "),paste(" # Females",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }
    for ( i in 1:data$nfishfleets){
      x<-get(noquote(paste("SelWt._",i,sep="")),pos=data$endgrowth)[data$endgrowth$Real_Age>0&data$endgrowth$Sex==2&data$endgrowth$Seas==j]
       write(paste(paste(x,collapse=" "),paste(" # Males",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}}

  if(data$nsexes==1){
    for(j in 1:max(data$ageselex$Seas)){
    for ( i in 1:data$nfishfleets){
      x<-get(noquote(paste("SelWt._",i,sep="")),pos=data$endgrowth)[data$endgrowth$Real_Age>0&data$endgrowth$Seas==j]
      write(paste(paste(x,collapse=" "),paste(" # ",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}}


 write("#SELECTIVITY -Fishery selectivity first FEMALES/ALL (a line for each fishery) then MALES (a line for each fishery)",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
      if(data$nsexes>1){
        for(j in 1:max(data$ageselex$Seas)){
         for ( i in 1:data$nfishfleets){
            x<-subset(data$ageselex,data$ageselex$Yr==data$endyr&data$ageselex$Sex==1&data$ageselex$Fleet==i&data$ageselex$Factor=="Asel2"&data$ageselex$Seas==j)
            write(paste(paste(as.numeric(x[,9:ncol(data$ageselex)]),collapse=" "),paste(" # Females",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }
    for ( i in 1:data$nfishfleets){
      x<-subset(data$ageselex,data$ageselex$Yr==data$endyr&data$ageselex$Sex==2&data$ageselex$Fleet==i&data$ageselex$Factor=="Asel2"&data$ageselex$Seas==j)
       write(paste(paste(as.numeric(x[,9:ncol(data$ageselex)]),collapse=" "),paste(" # Males",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }}}

  if(data$nsexes==1){
  for(j in 1:max(data$ageselex$Seas)){
    for ( i in 1:data$nfishfleets){
      x<-subset(data$ageselex,data$ageselex$Yr==data$endyr&data$ageselex$Fleet==i&data$ageselex$Factor=="Asel2"&data$ageselex$Seas==j)
       write(paste(paste(as.numeric(x[,9:ncol(data$ageselex)]),collapse=" "),paste(" # ",fish_name[i]," - Season ",j,sep="")),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
    }} }

  write("# set of survey names - none EBS_trawl_biomass_mtons BS_slope_trawl_biomass_mtons AI_trawl_biomass_mtons GOA_trawl_biomass_mtons Acoustic_trawl_biomass_mtons AFSC_longline_relative_numbers Coop_longline_relative_numbers not_listed",paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500,append=T)
  write("#SURVEYDESC",paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500,append=T)



  write(noquote(paste(input$fleetnames[sort(unique(input$CPUE$index[input$CPUE$index>0]))])),paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  write("#SURVEYMULT",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  write(noquote( paste(paste(input$CPUEinfo$Units[sort(unique(input$CPUE$index[input$CPUE$index>0]))],collapse=" "),paste(" # survey units multipliers"))),paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=500)

   surveys<-sort(unique(input$CPUE$index[input$CPUE$index>0]))
   for( j in 1:max(input$CPUE$seas)){
    for (i in surveys){

       x<-input$CPUE$year[input$CPUE$index==i&input$CPUE$seas==j]
       y<-input$CPUE$obs[input$CPUE$index==i&input$CPUE$seas==j]
       if(length(x)>0){
       write(paste("#",input$fleetnames[i]," - Season ",j,sep=""),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       write(x,paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       write(y,paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       remove(x)
       remove(y)
       }}}
  write("#STOCKNOTES",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=50)
  write(paste("",NOTES,"",sep='"'),paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
  }

wd <- 'C:/Users/cole.monnahan/Work/assessments/BSAI_flathead/2020_BSAI_Flathead/model_runs/Run06_francis_tuning'
GET_SARA(dir=wd, Enter_Data=FALSE,
         STOCK="flathead sole",
         REGION="BSAI", TIER="3a", T2="none", UPDATE="benchmark",
         LH=2, AF=3, AL=4, CD=5, AD=2, MFR=1000,
         NOTES="Model 18.2c (2020) based no Model 18.2c from 2018 with new data.",
         stryr=1964)

## GET_SARA(dir=getwd(), Enter_Data=FALSE, STOCK="Pacific cod",
##          REGION="GOA", TIER="3b", T2="none", UPDATE="benchmark",
##          LH=2, AF=3, AL=4, CD=5, AD=2, MFR=1000,
##          NOTES="Model 19.1 for 2020 based no Model19.14.48c from 2019 with new data.",
##          stryr=1977)

## Still have to manually change some things. Also the B_MSY is
## not right you need to get that from the proj output
