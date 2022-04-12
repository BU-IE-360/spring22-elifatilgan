library(xlsx)
library(lubridate)
library(ggplot2)
library(zoo)
require(GGally)
setwd("~/R")
InterestEVDS=read.xlsx("EVDS-Interest Rate.xlsx",sheetIndex=1, header=TRUE)
InterestEVDS = InterestEVDS[1:240,1:2]
InterestEVDS[,"Date"]=as.Date(as.yearmon(InterestEVDS[,"Date"],format="%Y-%m"))
colnames(InterestEVDS) <- c("Date","InterestRates")
InterestEVDS[,"InterestRates"]=as.numeric(InterestEVDS[,"InterestRates"])
head(InterestEVDS)


UnemploymentEVDS=read.xlsx("EVDS-Unemployment Rate.xlsx",sheetIndex=1, header=TRUE)
UnemploymentEVDS = UnemploymentEVDS[1:96,1:2]
UnemploymentEVDS[,"Date"]=as.Date(as.yearmon(UnemploymentEVDS[,"Date"],format="%Y-%m"))
colnames(UnemploymentEVDS) <- c("Date","UnemploymentRates")
UnemploymentEVDS[,"UnemploymentRates"]=as.numeric(UnemploymentEVDS[,"UnemploymentRates"])
head(UnemploymentEVDS)


COLEVDS=read.xlsx("EVDS-Cost of Living.xlsx",sheetIndex=1, header=TRUE)
COLEVDS = COLEVDS[1:240,1:2]
COLEVDS[,"Date"]=as.Date(as.yearmon(COLEVDS[,"Date"],format="%Y-%m"))
colnames(COLEVDS) <- c("Date","CostOfLiving")
COLEVDS[,"CostOfLiving"]=as.numeric(COLEVDS[,"CostOfLiving"])
head(COLEVDS)


trends=read.csv("multiTimeline.csv", header=TRUE)
trends[,"Ay"]=as.Date(as.yearmon(trends[,"Ay"],format="%Y-%m"))
colnames(trends) <- c("Date","iskur","zam")
trends[,"iskur"]=as.numeric(trends[,"iskur"])
trends[,"zam"]=as.numeric(trends[,"zam"])
head(trends)


EVDS1 <- merge(InterestEVDS,UnemploymentEVDS,by='Date')
EVDS <- merge(EVDS1,COLEVDS,by='Date')

ggplot(EVDS, aes(Date,InterestRates)) +
  geom_line(na.rm=TRUE,colour='red')+theme_classic()+scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month")
ggplot(EVDS, aes(Date,UnemploymentRates)) +
  geom_line(na.rm=TRUE,colour='blue')+theme_classic()+scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month")
ggplot(EVDS, aes(Date,CostOfLiving)) +
  geom_line(na.rm=TRUE,colour='green')+scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month")+theme_classic()

ggplot(EVDS,aes(x=factor(month(Date)),y=InterestRates))+geom_bar(stat="identity",aes(fill=month(Date)))+facet_wrap(~year(Date))+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x="Month",title="Histograms of Monthly Inflation Rates of Different Years")+scale_fill_distiller(palette="Pastel2")

ggplot(EVDS,aes(x=factor(month(Date)),y=UnemploymentRates))+geom_bar(stat="identity",aes(fill=month(Date)))+facet_wrap(~year(Date))+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
       axis.text.x = element_blank())+
  labs(x="Month",title="Histograms of Monthly Unemployment Rates of Different Years")+scale_fill_distiller(palette="Accent")
ggplot(EVDS,aes(x=factor(month(Date)),y=CostOfLiving))+geom_bar(stat="identity",aes(fill=month(Date)))+facet_wrap(~year(Date))+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x="Month",title="Histograms of Monthly Change in Cost of Living in Istanbul for Different Years")+scale_fill_distiller(palette="Set2")




ggplot(data=trends,aes(x=Date))+geom_line(aes(y=iskur))+geom_line(aes(y=zam),color="red")+theme_classic()
ccf(trends$zam,trends$iskur,lag.max=1000, plot=TRUE, main="Search for 'iskur' & 'zam'")
Find_Abs_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, lag.max = length(a)-5)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  absres = data.frame(abscor,lag)
  absres_max = res[which.max(absres$abscor),]
  return(absres_max)
}
Find_Abs_Max_CCF(trends$zam,trends$iskur)
ggplot(data=trends,aes(x=Date))+geom_line(aes(y=c(tail(iskur,-21),rep(NA,21))))+geom_line(aes(y=zam),color="red")

 
ggplot()+geom_line(data=trends[121:216,1:3],aes(x=Date,y=iskur), color="yellow")+geom_line(data=EVDS, aes(x=Date, y=UnemploymentRates),color="pink")+theme_dark()
cor(trends[121:216,1:3]$iskur,EVDS$UnemploymentRates)


ggpairs(EVDS)
ggpairs(trends)
