#write updated catch estimates to species.dat file that is required to run projections
write_sppcatch <- function(dir="foo",
                           datfile = 'Model1_Proj.dat',
                           sdir="data/",
                           data_file="spp_catch.dat",
                           data=Models[[1]],
                           OYMIN = 116000,
                           OYMAX = 800000,
                           # ct_yrs=2,
                           catchvec =NULL){
  write_file=paste0(dir,"/",data_file)
  ct_yrs = nrow(catchvec)
  write(noquote("#_SETUP_FILE_FOR_PROJ_MODEL"),file=write_file)
  
  write(noquote("#_Number_of_years with specified	catch"),file=write_file,append=TRUE)										
  write(ct_yrs,file=write_file,append=TRUE)
  
  write(noquote("# Number of runs"),file=write_file,append=TRUE)
  write(1,file=write_file,append=TRUE)
  
  write(noquote("#	OY	Minimum"),file=write_file,append=TRUE)
  write(paste(OYMIN,	noquote("#	for	age-structured	species"),sep=" "),file=write_file,append=TRUE)

  write(noquote("#	OY	Maximum"),file=write_file,append=TRUE)
  write(paste(OYMAX,	noquote("#	for	age-structured	species"),sep=" "),file=write_file,append=TRUE)
  
  write(noquote("#	data	files	for	each	species"),file=write_file,append=TRUE)
  
  write(noquote("#	Pollock	PCOD SAB YFN	GTBT ATF RKS FHS AKP POP NRF ATM"),file=write_file,append=TRUE)												
  write(noquote("#	1	2	3	4	5	6	7	8	9	10 11	12"),file=write_file,append=TRUE)

  write(noquote("# data files for	each species"),file=write_file,append=TRUE)
  write(noquote(datfile),file=write_file,append=TRUE)																				
  
  write(noquote("# ABC	Multipliers	"),file=write_file,append=TRUE)
  write(1,file=write_file,append=TRUE)

  write(noquote("# Population	scalars"),file=write_file,append=TRUE)
  write(1000,file=write_file,append=TRUE)

  write(noquote("#	New	Alt	4	Fabc	SPRs (Rockfish	=	0.75,	other	0.6)"),file=
          write_file,append=TRUE)										
  write(0.75,file=write_file,append=TRUE)	
  
  write(noquote("#	Number	of	TAC	model	categories"),file=write_file,append=TRUE)
  write(1,file=write_file,append=TRUE)																															
  
  write(noquote("#	TAC	model	indices	(for	aggregating)"),file=write_file,append=TRUE)
  write(1,file=write_file,append=TRUE)																															
  
  write(noquote("#	Catch	in	each	future	year"),file=write_file,append=TRUE)
  LY=data$Retro_year
  
  #Set catch to last year's catch if this year's catch is 0
  # write(paste(LY, sum(data$catch$Obs[data$catch$Yr==(LY)])),file=write_file,append=TRUE)
  
  if(ct_yrs > 1){
    write.table(catchvec, file=write_file, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  file.copy(write_file, file.path(dir, sdir), overwrite=TRUE)
}
