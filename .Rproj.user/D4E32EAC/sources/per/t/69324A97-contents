#get projection results

get_proj_res<-function(dir1="foo",dir2="foo",proj_out="Model_Proj_out/",data_file="proj_res_summ.out",FY=2018, k=1,nproj=2,species="foo")
{
	
	#Proj output files
 	#alt2=read.table(file=file.path(dir1,proj_out,"alt2_proj.out"),header=TRUE)
	 alt2=read.table(file=file.path(dir1,"alt2_proj.out"),header=TRUE)
 	print("Read alt2_proj.out")
  	
  	#Check skip lines equation. Not sure if percentiles prints the same for everyone...it should though
  	ssb_ref=as.data.frame(matrix(scan(file=file.path(dir1,"percentiles.out"),
  	                                  skip=(23+(nproj*5)),nlines=nproj),nrow=nproj,
        	ncol=9,byrow=TRUE))
        print("Read ssb from percentiles .out")
  	
        names(ssb_ref)=c("Year", "SSB100", "SSBabc", "SSBofl", "LowCI_SSB", "Median_SSB", "Mean_SSB", "UpperCI_SSB", "Stdev_SSB")
  	print("attaching names to ssb_ref")
  	print("first row of ssb_ref")
  	print(ssb_ref[1,])
  	
  	#See above comment
  	#f_ref=as.data.frame(matrix(scan(file=file.path(dir1,proj_out,"percentiles.out"),skip=(26+(nproj*6)),nlines=nproj),nrow=nproj,ncol=9,byrow=TRUE))
  	  	f_ref=as.data.frame(matrix(scan(file=file.path(dir1,"percentiles.out"),skip=(26+(nproj*6)),nlines=nproj),nrow=nproj,ncol=9,byrow=TRUE))

	print("Read f from percentiles .out")
  	
  	names(f_ref)=c("Year", "F0", "Fabc", "Fofl", "LowCI_F", "Median_F", "Mean_F", "UpperCI_F", "Stdev_F")
  	print("attaching names to f_ref")
  	print("first row of f_ref")
  	print(f_ref[1,])

  	  	
 	res=data.frame(Spp=species, 
			 Year=round(alt2$Year[alt2$Year>=FY & alt2$Year<=(FY+15)],0),
		  	 TotBiom=round(alt2$TotBiom[alt2$Year>=FY & alt2$Year<=(FY+15)],0),
		  	 SSB=round(alt2$SSB[alt2$Year>=FY & alt2$Year<=(FY+15)],0),
		  	 SSB100=round(ssb_ref[ssb_ref$Year>=FY & ssb_ref$Year<=(FY+15),"SSB100"],0),
		  	 SSBabc=round(ssb_ref[ssb_ref$Year>=FY & ssb_ref$Year<=(FY+15),"SSBabc"],0),
		  	 SSBofl=round(ssb_ref[ssb_ref$Year>=FY & ssb_ref$Year<=(FY+15),"SSBofl"],0),
		  	 Fofl=round(f_ref$Fofl[f_ref$Year>=FY & f_ref$Year<=(FY+15)],2),
		  	 Fabc=round(f_ref$Fabc[f_ref$Year>=FY & f_ref$Year<=(FY+15)],2),
		  	 OFL=round(alt2$OFL[alt2$Year>=FY & alt2$Year<=(FY+15)],0),
 			 ABC=round(alt2$ABC[alt2$Year>=FY & alt2$Year<=(FY+15)],0))
 	names(res)=c("Spp","Year","TotBiom","SSB","SSB100","SSBabc","SSBofl","Fofl","Fabc","OFL","ABC")
 	res$SSB_SSBofl=round(alt2$SSB[alt2$Year>=FY & alt2$Year<=(FY+15)],0)/round(ssb_ref[ssb_ref$Year>=FY & ssb_ref$Year<=(FY+15),"SSBofl"],0)
 
  	print("Summary of results")
  	print(res)
  	
  	print("writing summary projection results")
	write.table(res,file.path(dir1,data_file), row.names=FALSE, col.names=FALSE)
	print("finished writing summary projection results")
	
	return(res)
}