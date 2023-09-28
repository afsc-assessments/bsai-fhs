library(ggplot2)
library(dplyr)
library(tidyr)
theme_set(theme_bw())
library(cowplot)

ggwidth <- 7
ggheight <- 5
## Source this to make some of the figures for the SAFE

g <- read.csv('data/catch.csv') %>%
  group_by(YEAR) %>% summarize(catch=sum(TONS), .groups='drop') %>%
  ggplot(aes(YEAR, catch)) + geom_line()
g

## Survey by species and area
g1 <- read.csv('report/biomass_surveys_table.csv') %>%
  select(year, bio_flathead_sole, bio_Bering_flounder) %>%
  pivot_longer(-1, names_to='species', values_to='biomass') %>%
  mutate(species=gsub('bio_', '', species)) %>%
  ggplot(aes(year, biomass/1000, fill=species)) +
  geom_col(position='stack') +labs(x='Year', y='Biomass (1000 t)', fill='Species') +
  theme(legend.position="top")
g2 <- read.csv('report/biomass_surveys_table.csv') %>%
  select(year, bio_ebs_grouped, bio_ai) %>%
  pivot_longer(-1, names_to='region', values_to='biomass') %>%
  mutate(region=ifelse(region=='bio_ai', 'AI', 'EBS')) %>%
  filter(!is.na(biomass)) %>%
  ggplot(aes(year, biomass/1000, fill=region)) +
  geom_col(position='stack') +labs(x='Year', y='Biomass (1000 t)', fill='Region') +
  theme(legend.position="top")
g <- plot_grid(g1,g2, nrow=2)
ggsave('report/figures/Fig9.4.png', g, width=ggwidth, height=6)


## ## Used Excel instead
## temp <- read.csv('data/bttemp_ebs_standard_lt200.csv') %>%
##   select(Year=YEAR, temp=AVGBSBT)
## bio <- read.csv('report/biomass_surveys_table.csv') %>%
##   select(Year=year, biomass=bio_total)
## xx <- merge(temp, bio, by='Year', all=TRUE)
## stopifnot(!any(is.na(xx)))
## g <- ggplot(xx, aes(Year, temp)) + geom_line() +
##   labs(x='Year', y='Temperature (°C)')
## ggsave('report/figures/Fig9.5.png', g, width=ggwidth,
##        height=ggheight)


## Age-length plots
xx <- read.csv('inputs/SS_caal_survey.csv') %>%
  select(-FltSvy, -Seas, -Gender, -Part, -Ageerr, -Nsamp, -Lbin_hi) %>%
  rename(year=Yr, length=Lbin_lo) %>%
  pivot_longer(cols=-(1:2)) %>% filter(value>0) %>%
  mutate(sex=substr(name, 0,1), age=as.numeric(gsub("f|m",'', name))) %>%
  mutate(cohort=year-age)

g <- filter(xx, sex=='f') %>%
  ggplot(aes(age, length)) + facet_wrap('year') +
  geom_point(alpha=.5) + labs(x='Age', y='Length') +
  theme(panel.spacing = unit(0, "lines"))
ggsave('report/figures/Fig9.6.png', g, width=ggwidth*1.5,
       height=ggheight*1.5)

g <- filter(xx, sex=='m') %>%
  ggplot(aes(age, length)) + facet_wrap('year') +
  geom_point(alpha=.5) + labs(x='Age', y='Length') +
  theme(panel.spacing = unit(0, "lines"))
ggsave('report/figures/Fig9.7.png', g, width=ggwidth*1.5,
       height=ggheight*1.5)


library(r4ss)

m02 <- SS_output('model_runs/Run02_2018_update_SS')
m06 <- SS_output('model_runs/Run06_francis_tuning')
x <- SSsummarize(biglist=list(m02, m06), verbose=FALSE)


### note that there was a bug in r4ss that I had to correct to
### get the uncertainty intervals on the index plot here. Need to
## install development version once Ian merges in the pull request
## devtools::install('C:/Users/cole.monnahan/r4ss')
## SSplotComparisons(x, subplots=13, indexUncertainty=TRUE,
##                   indexPlotEach=FALSE)

## Make them twice with dif legend locations. Some
## inconsistencies in SSplotComparisons wrt par() so have to work
## around that a bit.
mypar <- list(mar = c(3.25, 3.25, 1, 1), mgp=c(1.75,.25,0), tck=-.01)
pwidth <- 5.5
pheight <- 4
SSplotComparisons(x, png=TRUE, par = mypar, subplots=c(2,10,14),
                  legendloc='topright',
                  indexUncertainty=TRUE,
                  plotdir='report/figures',
                  tickEndYr=FALSE,
                  legendlabels=c('18.2c','18.2c (2020)'),
                  pwidth=pwidth, pheight=pheight)
mypar <- list(mar = c(3.25, 5, 1, 1), mgp=c(1.75,.25,0), tck=-.01)
SSplotComparisons(x, png=TRUE, par = mypar, subplots=c(8,6),
                  legendloc='topright',
                  indexUncertainty=TRUE,
                  plotdir='report/figures',
                  tickEndYr=FALSE,
                  legendlabels=c('18.2c','18.2c (2020)'),
                  pwidth=pwidth, pheight=pheight)

### Code from Carey to make a phase plot. Adapted heavily here.
## MyOutput<-SS_output(dir = RunDir,covar = Uncertainty)
png('report/figures/phase_plot.png', width=5.5, height=4.5,
    units='in', res=500)
MyOutput <- m06
Mmale <- 0.2
Mfem <- 0.2
Mline = paste0(Mfem,"(f)",", ",Mmale,"(m)")
Tier = "3a"
species = "BSAI_Flathead"
LastYr <- 2020
scale <- 1000 #ss: scale is 1000
## Projected Total biomass:
ProjDir <- 'projection/Projections'
percentiles.file = file.path(ProjDir,"percentiles.out")
Alt1.sb.df<-read.table(file = percentiles.file,skip = 24,nrows = 14,header = T)
Frefs.df = read.table(file = percentiles.file,skip = 41,nrows = 1,header = T)
Brefs.df = read.table(percentiles.file,skip = 1,nrows = 1,header=T)
F35 = round(Frefs.df$Fofl,digits = 2)
Fabc = round(Frefs.df$Fabc,digits = 2)
F35precise = Frefs.df$Fofl
B35 <- B35precise  <-  scale*Brefs.df$SB35
F40precise = Frefs.df$Fabc
B40precise = scale*Brefs.df$SB40
means.file = file.path(ProjDir,"means.out")
means.out <-readLines(means.file)
FirstYr = MyOutput$startyr
LastYr = MyOutput$endyr
nyears = length(FirstYr:(LastYr))
## Cole: my proj input is split in two lines so combine them here
## SPB1 <- t(read.table(file=file.path(ProjDir,"Model_Proj.dat"), skip=36, fill=TRUE, header = FALSE))
## SPB2 <- t(read.table(file=file.path(ProjDir,"Model_Proj.dat"), skip = 37, header = FALSE))
if (MyOutput$ngpatterns == 1) {
  Fline = grep(paste0("F_",FirstYr),MyOutput$derived$Label) #be sure your F reporting
  Fseries = MyOutput$derived$Value[Fline:(Fline+nyears-1)]
} else {
  stop("broke Carey's code")
}
SPB <- readLines(file.path(ProjDir, "Model_Proj.dat"))[37:38] %>%
  strsplit(" ") %>% lapply(as.numeric) %>% unlist
ssb_yrplus1 = Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+1]*1000
ssb_yrplus2 = Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+2]*1000
ssb_w_future = c(SPB,ssb_yrplus1,ssb_yrplus2)
Alt1.F.df<-read.table(file = percentiles.file,skip = 41,nrows = 14,header = T)
F_yrplus1 = Alt1.F.df$Mean_F[Alt1.F.df$Year==LastYr+1]
F_yrplus2 = Alt1.F.df$Mean_F[Alt1.F.df$Year==LastYr+2]
F_w_future = c(Fseries,F_yrplus1,F_yrplus2)
RelF = F_w_future/F35precise
RelSSB = ssb_w_future/B35precise
RelB40 = B40precise/B35precise
RelF40 = F40precise/F35precise
## par(mai =c(1.02,1,0.82,0.42))
par(mar=c(4,4,1,1), mgp=c(2,.5,0), tck=-.02)
plot(RelSSB, RelF,type = "l", lwd = 2,
     xlim = c(0,(max(RelSSB)+1)), ylim = c(0,1.5),
     xlab ="(Spawning Biomass)/B35%" , ylab = "F/F35%",
     cex.axis = 1, cex.lab =1)
abline(h = 1,col = "grey")
abline(v = 1,col = "grey")
##Plot OFL control rule
Bofl0 = 0.05*B40precise/B35precise
Bflat = B40precise/B35precise
segments(x0 = RelB40,y0 = 1,x1=(max(RelSSB)+1),y1 = 1,col = "firebrick3",lwd = 2,lty = 3)
segments(x0 = Bofl0 ,y0 = 0,x1=RelB40,y1 = 1,col = "firebrick3",lwd = 2, lty = 3)
segments(x0 = 0,y0=0, x1 = 0.05/B35,y1= 0,col = "firebrick3",lwd = 2, lty = 3)
##Plot maxABC control rule
##----------------------------------------------------------------------------------
segments(x0 = RelB40,y0 = RelF40,x1=(max(RelSSB)+1),y1 = RelF40,col = "firebrick3",lwd = 2)
segments(x0 = Bofl0 ,y0 = 0,x1=RelB40,y1 = RelF40,col = "firebrick3",lwd = 2)
segments(x0 = 0,y0=0, x1 = 0.05/B35,y1= 0,col = "firebrick3",lwd = 2)
##----------------------------------------------------------
##mark the last year:
points(x = RelSSB[1],y = RelF[1],pch = 20,col = "grey",cex = 2)
dev.off()

