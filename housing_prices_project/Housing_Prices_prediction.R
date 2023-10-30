install.packages("urca")
install.packages("vars")
install.packages("mFilter")
install.packages("tseries")
install.packages("forecast")
install.packages("tidyverse")
install.packages("tidyquant")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("zoo")

install.packages(c("magrittr", "dplyr", "tidyr"))
library(magrittr)  # for %>%
library(dplyr)     # for dplyr functions
library(tidyr)     # for pivot_longer()

install.packages(c("ggplot2", "ggfortify"))
library(ggplot2)
library(ggfortify)

install.packages("vars")
library(vars)


library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
options(digits=4)
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)
library(ggfortify)

# loading the data
install.packages("read_excel")
library(readxl)
dataset<- read_excel("/Users/krishnabezawada/Downloads/Dataset.xls")
dataset

# Creating a dataframe of all variables

data = read_excel("/Users/krishnabezawada/Downloads/Dataset.xls")                   
dfdata = data.frame(data)
dfdata  <- dfdata [,colSums(is.na(dfdata))<nrow(dfdata)]
dfdata[,2:6] <- scale(dfdata[,2:6])
dfdata
View(dfdata)

# Explanatory data analysis

#1. ##Histogram
 
pivot<-dataset[,2:7]%>%
  pivot_longer(colnames(dataset[,2:7]))%>%
  as.data.frame()
gg1<-ggplot(pivot,aes(x=value, y = after_stat(density)))+
  geom_histogram()+geom_density(aes(fill = name), alpha = 0.2)+ facet_wrap(~name,scales = "free")
gg1

#2. ##box plots
gg3<-ggplot(pivot,aes(x=value))+
  geom_boxplot(col = "#00cfcc")+
  facet_wrap(~name,scales = "free")
gg3+theme(plot.background = element_rect(fill = "#002845"))


#3. ##Q-Q plots## -> ask professor?
gg4<-ggplot(pivot,aes(sample=value))+
  geom_qq()+stat_qq()+geom_qq_line()+
  facet_wrap(~name,scales = "free")
gg4


#4. Summary Stats

sats<-summary(dataset)
sats

# Creating time-series of all variables
inflation <- ts(dfdata$Inflation, start = c(2010-01-01,1), frequency=12)
interest <- ts(dfdata$InterestRate, start = c(2010-01-01,1), frequency=12)
unemployment <- ts(dfdata$UnemploymentRate, start = c(2010-01-01,1), frequency=12)
spread <- ts(dfdata$YieldSpread, start = c(2010-01-01,1), frequency=12)
gdp <- ts(dfdata$GDPGrowthRate, start = c(2010-01-01,1), frequency=12)
prices <- ts(dfdata$RealEstatePrices, start = c(2010-01-01,1), frequency=12)
View(gdp)
timeseries <- c(inflation,interest,unemployment,spread,gdp,prices)
timeseries

# plot the series of individual variables

autoplot(cbind(inflation,interest,unemployment, gdp, prices, spread))

# plotting all the variables in one graph
install.packages("reshape2")                 # Install reshape2 package
library("reshape2")                          # Load reshape2 package
data_long <- melt(dfdata, id.vars = "Date")  # Reshaping data to long format
View(data_long)                              # Head of long data
ggplot(data_long,                            # Draw ggplot2 time series plot
       aes(x = Date,
           y = value,
           col = variable)) +
  geom_line()

# MODEL 1 : "Multiple regression"
mr <- lm(prices ~ inflation + interest + unemployment + gdp +spread, data=dfdata);summary(mr)


## MODEL 2 : VAR

## STEP 1: Model selection and estimation to know the optimal number of lags.

dat.bv <- data.frame(cbind(prices,inflation,interest,gdp,unemployment))
colnames(dat.bv) <- c("prices","inflation","interest","gdp","unemployment")
info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection

## STEP 2: Building VAR 

ModelVar <- VAR(dat.bv, p = 3, type = "const", season = NULL, 
              exog = NULL)
summary(ModelVar)

# STEP 3: Diagonsing the VAR

#a. Serial Correlation
serial <- serial.test(ModelVar, lags.pt = 12, type = "PT.asymptotic")
serial # for declan - if p value is greater than 0.05 then there's no serial corr in residuals#

#b. Heteroscadasticity
heteroscadasticity <- arch.test(ModelVar, lags.multi = 12, multivariate.only = TRUE)
heteroscadasticity # for declan - if p value is greater than 0.05 then there's no heteroscas in residuals#

#c.Normal distribution of the residual
normal <- normality.test(ModelVar, multivariate.only = TRUE)
normal # for declan - if p value is greater than 0.05 then residuals are normally distributed#


## STEP 4: Determining the persistence of the model
par(mfrow=c(2,1))
acf(as.vector(inflation),xlim=c(1,20), main = "ACF for inflation")
pacf(as.vector(inflation), main = "PACF for inflation") 

par(mfrow=c(2,1))
acf(as.vector(interest),xlim=c(1,20), main = "ACF for interest")
pacf(as.vector(interest), main = "PACF for interest") 

par(mfrow=c(2,1))
acf(as.vector(unemployment),xlim=c(1,20), main = "ACF for unemployment")
pacf(as.vector(unemployment), main = "PACF for unemployment") 

par(mfrow=c(2,1))
acf(as.vector(spread),xlim=c(1,20), main = "ACF for spread")
pacf(as.vector(spread), main = "PACF for spread") 

par(mfrow=c(2,1))
acf(as.vector(gdp),xlim=c(1,20), main = "ACF for gdp")
pacf(as.vector(gdp), main = "PACF for gdp") 

par(mfrow=c(2,1))
acf(as.vector(prices),xlim=c(1,20), main = "ACF for prices")
pacf(as.vector(prices), main = "PACF for prices") 

### Granger causality ###

library(lmtest)
attach(timeseries)
prices = diff(prices)
inflation = diff(inflation)
interest= diff(interest) 
unemployment= diff(unemployment)
spread= diff(spread)
gdp= diff(gdp)

## Two series to be tested for granger casuality

## (1) Whether GDP granger cause prices or vice versa

## Null:GDP does not granger cause price, then
grangertest(prices~gdp, order = 3)
## for reverse direction
grangertest(gdp~prices, order = 3)


## (2) Whether inflation granger cause prices or vice versa

## Null:inflation does not granger cause prices, then
grangertest(prices~inflation, order = 3)
## for reverse direction
grangertest(inflation~prices, order = 3)


## (3) Whether unemployment granger cause prices or vice versa

## Null:unemployment does not granger cause prices, then
grangertest(prices~unemployment, order = 3)
## for reverse direction
grangertest(unemployment~prices, order = 3)


## (4) Whether interest granger cause prices or vice versa

## Null:interest does not granger cause prices, then
grangertest(prices~interest, order = 3)
## for reverse direction
grangertest(interest~prices, order = 3)


## (5) Whether spread granger cause prices or vice versa

## Null:spread does not granger cause prices, then
grangertest(prices~spread, order = 3)
## for reverse direction
grangertest(spread~prices, order = 3)



####--------------------------------------COINTEGRATION---------------------------------------####
# STEP 1: To perform the respective unit root tests on these variables we proceeded with augmented Dickey-Fuller test.

dfdataset = data.frame(dataset) #Using raw-data to check for co-integration.
view(dfdataset)

adfprices <- ur.df(dataset$RealEstatePrices, type = "trend", selectlags = c("AIC"))
summary(adfprices)

adfinflation <- ur.df(dataset$Inflation, type = "trend", selectlags = c("AIC"))
summary(adfinflation)

adfinterest <- ur.df(dataset$InterestRate, type = "trend", selectlags = c("AIC"))
summary(adfinterest)

adfunemployment <- ur.df(dataset$UnemploymentRate, type = "trend", selectlags = c("AIC"))
summary(adfunemployment)

adfspread <- ur.df(dataset$YieldSpread, type = "trend", selectlags = c("AIC"))
summary(adfspread)

adfgdp <- ur.df(dataset$GDPGrowthRate, type = "trend", selectlags = c("AIC"))
summary(adfgdp)


# We would have the same test with 1st difference to know whether the price is integrated with order 1 or not,
# but since we are taking raw data and not the time series we will not execute this test, otherwise we would have used the foolowing command;
#adfprices2 <- ur.df((prices), selectlags = c("AIC"))
#summary(adfprices2)


#A. linear equation to know the residuals

data <- ts.union(prices, inflation)
prices.inf <- lm(prices ~ inflation, data = data)
summary(prices.inf)
plot.ts(prices.inf$residuals)

data <- ts.union(prices, interest)
prices.int <- lm(prices ~ interest, data = data)
summary(prices.int)
plot.ts(prices.int$residuals)

data <- ts.union(prices, unemployment)
prices.unemp <- lm(prices ~ unemployment, data = data)
summary(prices.unemp)
plot.ts(prices.unemp$residuals)

data <- ts.union(prices, spread)
prices.spread <- lm(prices ~ spread, data = data)
summary(prices.spread)
plot.ts(prices.spread$residuals)

data <- ts.union(prices, gdp)
prices.gdp <- lm(prices ~ gdp, data = data)
summary(prices.gdp)
plot.ts(prices.gdp$residuals)

### To confirm that the residuals are stationary we subject them to a unit root test;

error.prices <- ur.df(prices.eq$residuals, lags = 1, type = "none")
summary(error.prices)

### constructing the error correction model.

dat_ecm <- tibble(
  price_d = diff(dataset$prices)[-1],
  error_ecm1 = dataset$prices$residuals[-1:-2],
  price_d1 = diff(dataset$prices)[-(length(dataset$prices) - 1)]
  )


## Experimenting with Impulse Response.
irf.inflation <- irf(dat.bv, impulse = "prices", response = "inflation", 
               n.ahead = 40, boot = TRUE)
plot(irf.inflation, ylab = "ouput", main = "Shock from inflation")


***********-----------------------THE END------------------------------************






























