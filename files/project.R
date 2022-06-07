require(data.table)
require(lubridate)
require(zoo)
library(forecast)
library(tseries)
library(ggplot2)
require(urca)
library(dplyr)
data_path='C:/Users/DELL/Downloads/2022-06-02_production.csv'
production = fread(data_path)
production[,date:=as.Date(date)]

data_path2='C:/Users/DELL/Downloads/2022-06-02_weather.csv'
l_weather = fread(data_path2)
l_weather[,date:=as.Date(date)]
w_weather=dcast(l_weather,date+hour~variable+lat+lon,value.var="value")
w_weather$month=as.numeric(format(w_weather$date,format="%m"))

data=merge(production,w_weather,by=c("date","hour"))
data$month=as.numeric(format(data$date,format="%m"))

train_start=as.Date('2021-03-31')
test_start= as.Date('2022-03-01')
test_end = as.Date('2022-05-24')

ggplot(data[date>=train_start],aes(x=date,y=production))+geom_line()+facet_wrap(~hour)


#train startin nereden baslayacagini bu grafikten gördük
daily_series=data[,list(total=mean(production)),list(date)]
ggplot(daily_series,aes(x=date,y=total))+geom_line()

traindata = daily_series[date>=train_start &date <= test_start]
tsdata=ts(traindata$total,frequency = 7)
plot(tsdata, main = "Solar Power Production Time Series (Daily)", 
     col="darkgreen", lwd=1,ylab="Amount", xlab="Weeks") 
data_dec_multip<-decompose(tsdata,type="multiplicative")
deseasonalized=tsdata/(data_dec_multip$seasonal)
plot(deseasonalized,main="Time Series of deseasonalized Adjusted Production",col="orange")
detrend_seasonalized=deseasonalized/(data_dec_multip$trend)
data_random=detrend_seasonalized
ts.plot(data_random, main="Time Series of detrend & deseasonalized Adjusted Production",col="blue")
plot(acf(na.omit(data_random),lag.max=60,plot=FALSE), main = "Autocorrelation of detrend & deseasonalized Adjusted Production",col="black", lwd=1.5, xlab="Lag in Days")

plot(pacf(na.omit(data_random),lag.max=60,plot=FALSE), main = "Partial Autocorrelation of detrend & deseasonalized Adjusted Production",col="black", lwd=1.5, xlab="Lag in Days")
a<- ur.kpss(data_random)
summary(a)


avgdata=data.table(date=data$date,hour=data$hour,cloud=rowSums(data[,3:11])/9,dswrf=rowSums(data[,12:20])/9,relhumidity=rowSums(data[,21:29])/9,temp=rowSums(data[,30:38])/9,production=data$production,month=as.numeric(format(data$date,format="%m")))
avgweather=data.table(date=w_weather$date,hour=w_weather$hour,cloud=rowSums(data[,3:11])/9,dswrf=rowSums(data[,12:20])/9,relhumidity=rowSums(data[,21:29])/9,temp=rowSums(data[,30:38])/9)
avgdata_lagged <- data.table(avgdata,lag1=c(NA,avgdata$production[1:length(avgdata$production)-1]),lag24=c(rep(NA,24),avgdata$production[1:(length(avgdata$production)-24)]))

ForecastReportlm <- function(x_date) {
lm_vec <- rep(NA,12)
  dataf <- avgdata_lagged[date >= train_start & date < x_date & hour >=7 & hour <= 18]
  model1=lm(formula = production~.-date-hour-month+as.factor(month)+as.factor(hour)-production,dataf)
  test_data <- avgdata_lagged[date==as.Date(x_date)+1& hour >=7 & hour <= 18,c("date","hour","cloud",
                        "dswrf","relhumidity","month","temp","lag1","lag24")]
  lm_vec<- predict(model1,data.frame(test_data))
lm_vec <- c(rep(0,7),lm_vec,rep(0,5))
return(array(unlist(lm_vec)))
}
ForecastReportAutoAr <- function(x_date) {

arima_vec <- rep(NA,24)
for (i in 0:23){
  dataf <- data[date >= train_start & date < x_date & hour == i,c("date","production")]
  dataf <- ts(dataf)
  fitted=auto.arima(dataf[,'production'],seasonal = T,trace=F)
  forecasted1=forecast(fitted,h=3)
  arima_vec[i+1]<-forecasted1$mean[3]
}
return(arima_vec)
}
ForecastReportArimax <- function(x_date) {
arima_vec_reg <- rep(NA,24)
for(i in 0:23) {
  dataf <- data[date >= train_start & date < x_date & hour == i,c("date","production")]
  dataf <- ts(dataf)
  regressor <- data[date >= train_start & date < x_date & hour == i,c("CLOUD_LOW_LAYER_36.25_33")]
  fitted_reg <- auto.arima(dataf[,'production'],xreg=data.matrix(regressor))
  regforecast <- w_weather[(date==x_date & hour==i) |(date==as.Date(x_date)+1 & hour==i)|date==as.Date(x_date)+2 & hour==i,c("CLOUD_LOW_LAYER_36.25_33")]
  forecasted2=forecast(fitted_reg,xreg=data.matrix(regforecast),h=3)
  arima_vec_reg[i+1] <- forecasted2$mean[3]
}
return(arima_vec_reg)
}
ForecastReportArimax3<- function(x_date) {
arima_vec_reg2 <- rep(NA,24)
for(i in 0:23) {
  dataf <- avgdata[date >= train_start & date < x_date & hour == i,c("date","production")]
  dataf <- ts(dataf)
  regressor <- avgdata[date >= train_start & date < x_date & hour == i,c("cloud","temp","relhumidity")]
  fitted_reg <- auto.arima(dataf[,'production'],xreg=data.matrix(regressor))
  regforecast <- avgweather[(date==x_date & hour==i) |(date==as.Date(x_date)+1 & hour==i |date==as.Date(x_date)+2 & hour==i),c("cloud","temp","relhumidity")]
  forecasted2=forecast(fitted_reg,xreg=data.matrix(regforecast),h=3)
  arima_vec_reg2[i+1] <- forecasted2$mean[3]
}
return(arima_vec_reg2)
}

ForecastReportArima <- function(x_date,Order) {
  Forecasted <- c()
  for (i in 7:19){
    df1<-data[date<=x_date]
    #Hour filtering
    df1=df1[df1$hour==i,c("date","production")]
    
    
    
    combined_data_decomp<-decompose(ts(df1$production,freq=7),type="multiplicative")
    
    
    noise<-combined_data_decomp$random
    trend<-combined_data_decomp$trend
    seasonality<-combined_data_decomp$seasonal
    
    nahead=3 # 2 days later prediction
    
    fitted=arima(noise,order=Order)
    
    predicted_noise <- forecast(fitted,h=nahead)$mean[3]
    last_trend_value <-tail(combined_data_decomp$trend[!is.na(combined_data_decomp$trend)],1)
    seasonality<-tail(combined_data_decomp$seasonal[!is.na(combined_data_decomp$seasonal)],1)
    
    ProdForecast<-predicted_noise*last_trend_value*seasonality
    Forecasted<-c(Forecasted,ProdForecast)
    
  }
  return(Forecasted)
}

data2<- data[date>= test_start & date <= test_end,c("date","hour","production")]
data2$forecast_arima = rep(0,nrow(data2))
data2$forecast_arimax = rep(0,nrow(data2))
data2$forecast_arimax3 = rep(0,nrow(data2))
data2$forecast_autoar = rep(0,nrow(data2))
data2$forecast_lm = rep(0,nrow(data2))
for(j in (seq.Date(test_start, test_end, by=1))){
  j=as.Date(j)
  data2[date==j & hour >=7 & hour <= 19]$forecast_arima=ForecastReportArima(as.Date(j),c(2,0,3))
  data2[date==j]$forecast_arimax=ForecastReportArimax(as.Date(j))
  data2[date==j]$forecast_arimax3=ForecastReportArimax3(as.Date(j))
  data2[date==j]$forecast_autoar=ForecastReportAutoAr(as.Date(j))
  data2[date==j]$forecast_lm=ForecastReportlm(as.Date(j))
  
}
naive = data[,c("date","hour","production")] 
naive$naive=lag(naive$production,48)
naive<- naive[date>=test_start & date <= test_end]
data2$naive=naive$naive
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}

melted_result=melt(data2,c('date','hour','production'),c('forecast_arima','forecast_arimax','forecast_arimax3',
                                                         'forecast_autoar','forecast_lm','naive'))

a<-melted_result[,accu(production,value),by=list(variable)]
a


