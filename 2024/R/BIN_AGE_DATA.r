# adapted/generalized from Steve Barbeaux' files for
# generating SS files for EBS/AI Greenland Turbot
# ZTA, 2013-05-08, R version 2.15.1, 32-bit

BIN_AGE_DATA <- function(data,age_bins=seq(1,100,1))
{
    age<-data.frame(AGE=c(1:max(data$AGE)))
    age$aBIN<-max(age_bins)
    n<-length(age_bins)
    for(i in 2:n-1)
    {
       age$aBIN[age$AGE < age_bins[((n-i)+1)] ]<-age_bins[n-i]
    }
    data<-merge(data,age,all.x=T,all.y=F)
    data
}

