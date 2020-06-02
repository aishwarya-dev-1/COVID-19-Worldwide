library(fpp)
library(fpp2)
library(data.table)
library(ggplot2)

data=copy(dataworld)
View(data)
setDT(data)
str(data)
data[,date:=as.Date(date,'%d/%m/%Y')]

options(scipen = 999) ##To disable sciientific notation
ts=ts(data$total_cases, start = c(2019,365), frequency = 365)
ts
autoplot(ts,color='red')+ylab("Total Cases")+xlab("Days")
death.ts=ts(as.numeric(data$total_deaths), start =c(2019,365), frequency = 365)
death.ts
autoplot(death.ts,color='blue')+ylab("Total Deaths")+xlab("Days")

ggplot()+
  geom_line(data=data,aes(x=date,y=total_cases, color="Total Cases"))+
  geom_line(data=data,aes(x=date,y=total_deaths, color="Total Deaths"))+
  xlab('Date')+
  ylab('Worldwide')

data$total_cases <- as.POSIXct(data$total_cases)
data$total_deaths <- as.numeric(data$total_deaths)
ggplot(data, aes(x = total_cases, y = total_deaths)) + geom_line() + xlab("New Cases") + ylab("Total Deaths")
trainset=data[1:119] 
testset=data[120:151]

tdata1=ts(trainset$total_cases,frequency=365,start=c(2019,365))
tdata1
tdata2=ts(trainset$total_deaths,frequency=365,start=c(2019,365))
tdata2

tesdata1=ts(testset$total_cases,frequency=365,start=c(2020,119))
tesdata1
tesdata2=ts(testset$total_deaths,frequency=365,start=c(2020,119))
tesdata2

autoplot(tdata1)+
  autolayer(tesdata1,color='red')+
  ylab("Total Cases")
View(tdata1)

autoplot(tdata2)+
  autolayer(tesdata2,color='green')+
  ylab("Total Deaths")
View(tesdata2)

#NAIVE METHOD

nfit1=naive(tdata1,h=60)
nfc1=forecast(nfit1,h=60)

autoplot(tdata1)+
  autolayer(tesdata1,series = "Test data for total cases")+
  autolayer(fitted(nfit1),series="Fitted values")+
  autolayer(nfc1,series="Naive forecasts",PI=FALSE)+
  ggtitle("NAIVE FORECASTING METHOD")

nfit2=naive(tdata2,h=60)
nfc2=forecast(nfit2,h=60)

autoplot(tdata2)+
  autolayer(tesdata2,series = "Test data for total death")+
  autolayer(fitted(nfit2),series="Fitted values")+
  autolayer(nfc2,series="Naive forecasts",PI=FALSE)+
  ggtitle("NAIVE FORECASTING METHOD")

#MEAN METHOD

mfit1=meanf(tdata1,h=60)
mfc1=forecast(mfit1,h=60)

autoplot(tdata1)+
  autolayer(tesdata1,series = "Test data for total cases")+
  autolayer(fitted(mfit1),series="Fitted values")+
  autolayer(mfc1,series="Simple Avg forecasts",PI=FALSE)+
  ggtitle("SIMPLE AVERAGE FORECASTING METHOD")

mfit2=meanf(tdata2,h=60)
mfc2=forecast(mfit2,h=60)

autoplot(tdata2)+
  autolayer(tesdata2,series = "Test data for total deaths")+
  autolayer(fitted(mfit2),series="Fitted values")+
  autolayer(mfc2,series="Simple Avg forecasts",PI=FALSE)+
  ggtitle("SIMPLE AVERAGE FORECASTING METHOD")

#ARIMA MODEL

ndiffs(tdata1)
afit1=auto.arima(tdata1,seasonal = FALSE)
afit1 #p=4,q=0
afc1=forecast(afit1,h=32)

autoplot(tdata1)+
  autolayer(tesdata1,series = "Test data for total cases")+
  autolayer(fitted(afit1),series="Fitted values")+
  autolayer(afc1,series="Arima forecasts",PI=FALSE)+
  ggtitle("ARIMA MODEL")

ndiffs(tdata2)
afit2=auto.arima(tdata2,seasonal = FALSE)
afit2 #p=1 q=1 n=2
afc2=forecast(afit2,h=32)

autoplot(tdata2)+
  autolayer(tesdata2,series = "Test data for total deaths")+
  autolayer(fitted(afit2),series="Fitted values")+
  autolayer(afc2,series="Arima forecasts",PI=FALSE)+
  ggtitle("ARIMA MODEL")

#ACCURACY COMPARISONS

fit1=accuracy(nfc1,x=tesdata1)
fit2=accuracy(mfc1,x=tesdata1)
fit3=accuracy(afc1,x=tesdata1)
fit1
fit2
fit3

autoplot(tdata1)+
  autolayer(tesdata1,series = "Test data for new cases")+
  autolayer(nfc1,series="Naive forecasts",PI=FALSE)+
  ##autolayer(sfc,series="SNaive forecasts",PI=FALSE)+
  autolayer(mfc1,series="Simple Average forecasts",PI=FALSE)+
  autolayer(afc1,series="Arima forecasts",PI=FALSE)+
  ggtitle("MODEL COMPARISONS")

fit4=accuracy(nfc2,x=tesdata2)
fit5=accuracy(mfc2,x=tesdata2)
fit6=accuracy(afc2,x=tesdata2)
fit4
fit5
fit6

autoplot(tdata2)+
  autolayer(tesdata2,series = "Test data for total deaths")+
  autolayer(nfc2,series="Naive forecasts",PI=FALSE)+
  autolayer(mfc2,series="Simple Average forecasts",PI=FALSE)+
  autolayer(afc2,series="Arima forecasts",PI=FALSE)+
  ggtitle("MODEL COMPARISONS")


