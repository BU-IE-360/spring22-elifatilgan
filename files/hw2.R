library(xlsx)
library(lubridate)
library(zoo)
library(ggplot2)
library(RcppRoll)
library(GGally)
library(skimr)
library(forecast)
library(dplyr)
library(data.table)
setwd("~/R/data")
df= read.csv("IE360_Spring22_HW2_data.csv", header=TRUE)
for(i in c(2,4,7,9,10,11)) {
  df[,i]=as.numeric(df[,i])
}
colnames(df)= c("Quarter","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV",
                "GNPA","GNPC","GNP")
df$Quarter= as.Date(as.yearqtr(df$Quarter,format="%Y_Q%q"))
ggplot(df, aes(x=Quarter,y=UGS))+geom_line()+geom_point()+
  labs(title = "Figure 1: Unleaded Gasoline Sales Over the Quarters")+theme_classic()+scale_x_date(date_breaks = "1 year", date_labels = "%Y")
mean_series=roll_mean(df$UGS[1:28],4,align='left')
var_series=roll_var(df$UGS[1:28],4,align='left')
plot(mean_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Mean",
     main = "Mean series")

plot(var_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Variance",
     main = "Variance series")
acf(df$UGS,lag.max=12,na.action=na.pass)
ggpairs(df)
Q=seq(1,4,by=1)
df=cbind(df,Q)
model0 =lm(UGS~Quarter+NLPG+NUGV+PG+PU+GNP.Agriculture+NDGV+as.factor(Q),data=df)
summary(model0)
checkresiduals(model0$residuals)
model1=lm(UGS~NUGV+as.factor(Q),data=df)
summary(model1)
checkresiduals(model1$residuals)
model2 =lm(UGS~as.factor(Q)+NLPG+NUGV+PU+NDGV,data=df)
summary(model2)
checkresiduals(model2$residuals)
df$NUGVlag1=lag(df$NUGV,1)
model3=lm(UGS~as.factor(Q)+NDGV+NUGVlag1+NUGV+PU,data=df)
summary(model3)
checkresiduals(model3$residuals)
df$UGSlag1=lag(df$UGS,1)
model4 =lm(UGS~as.factor(Q)+NDGV+UGSlag1+NUGV+PU,data=df)
summary(model4)
checkresiduals(model4$residuals)
mean(model4$residuals)
model= model4

tmp=copy(df)
tmp$actual=df$UGS
tmp$predicted=predict(model,tmp)
ggplot(tmp ,aes(x=Quarter))+
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted,color='predicted'))+
  labs(title = "Figure 2: Predicted vs Real UGS Values Over the Quarters",y="UGS (1000 m3)" )


set2007= df[29:32,c("Q","NDGV","UGSlag1","NUGV","PU")]
prediction = c(0,0,0,0)
for(i in 1:4) {
  prediction[i] = predict(model,newdata = set2007[i,])
  if(i<4){set2007[i+1,"UGSlag1"] = prediction[i] }
}
prediction
