# adapted/generalized from Steve Barbeaux' files for
# generating SS files for EBS/AI Greenland Turbot
# ZTA, 2013-05-08, R version 2.15.1, 32-bit

BIN_LEN_DATA <- function(data,len_bins=seq(1,100,1))
{
    length<-data.frame(LENGTH=c(1:max(data$LENGTH)))
    length$BIN<-max(len_bins)
    n<-length(len_bins)
    for(i in 2:n-1)
    {
       length$BIN[length$LENGTH < len_bins[((n-i)+1)] ]<-len_bins[n-i]
    }
    data<-merge(data,length,all.x=T,all.y=F)
    data
}

