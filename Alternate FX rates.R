install.packages('imfr')
install.packages('tidyverse')
install.packages("glmnet", dependencies=TRUE)

library(imfr)
library(tidyverse)
library(dplyr)
library(GGally)
library(car)
library(leaps)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(glmnet)
library(Metrics)
library(caret)
library(lme4)
library(corrr)
library(glmnet)
#---------------------ALL Function Need to be run First---------------------
#---------------------Make sure you put China interest rate Data in your active directory---------------------

getBoP = function(countryCode, startingYear, freq){
  indicators = c('BCAXF_BP6_USD','BKAA_BP6_USD','BFXF_BP6_USD',
                 'BOPFR_BP6_USD','BTRUE_BP6_USD')
  BoP_country = imf_data(database_id = 'BOP', indicator = indicators,
                         start = startingYear, country = c(countryCode), freq = freq)
  names(BoP_country) = c('countryCode', 'yearQuarter', 'currentAccount','capitalAccount', 
                         'financialAccount','statDisc','offcialReserve')
  return(BoP_country)
}

getFX = function(countryCode, startingYear, freq){
  fxData = imf_data(database_id = 'IFS', 
                    indicator = 'ENDE_XDC_USD_RATE', start = startingYear,
                    country = countryCode, freq = freq)
  names(fxData) = c('countryCode', 'yearQuarter','PerUSD')
  return(fxData)
}

getIR = function(countryCode, startingYear, freq){
  interestRates <- imf_data(database_id = 'IFS', indicator = 'FPOLM_PA', start = startingYear,
                            country = countryCode, freq = freq)
  names(interestRates) = c('countryCode', 'yearQuarter','interestRates')
  return(interestRates)
}

getRsvExclGold = function(countryCode, startingYear, freq){
  ReserveExclGold <- imf_data(database_id = 'IFS', indicator = 'RAXG_USD', start = startingYear,
                              country = countryCode, freq = freq)
  names(ReserveExclGold) = c('countryCode', 'yearQuarter','RsvExclGold')
  return(ReserveExclGold)
}

getChgInAllVars = function(countryData){
  countryData <- mutate(countryData, chgInCurrAcct=currentAccount - lag(currentAccount))
  countryData <- mutate(countryData, chgInCapAcct=capitalAccount - lag(capitalAccount))
  countryData <- mutate(countryData, chgInFinAcct=financialAccount - lag(financialAccount))
  countryData <- mutate(countryData, chgInStatDisc=statDisc - lag(statDisc))
  countryData <- mutate(countryData, chgInOffRsv=offcialReserve - lag(offcialReserve))
  countryData <- mutate(countryData, chgInintRates=interestRates - lag(interestRates))
  countryData <- mutate(countryData, chgInRsvExclGold=RsvExclGold - lag(RsvExclGold))
  countryData <- mutate(countryData, chgInperUSD=PerUSD - lag(PerUSD))
}

getChgInAllVarsPercentage = function(countryData){
  countryData <- mutate(countryData, chgInCurrAcctPercent=100*((currentAccount/lag(currentAccount))-1))
  countryData <- mutate(countryData, chgInCapAcctPercent=100*((capitalAccount/lag(capitalAccount))-1))
  countryData <- mutate(countryData, chgInFinAcctPercent=100*((financialAccount/lag(financialAccount))-1))
  countryData <- mutate(countryData, chgInStatDiscPercent=100*((statDisc/lag(statDisc))-1))
  countryData <- mutate(countryData, chgInOffRsvPercent=100*((offcialReserve/lag(offcialReserve))-1))
  countryData <- mutate(countryData, chgInintRatesPercent=100*((interestRates/lag(interestRates))-1))
  countryData <- mutate(countryData, chgInRsvExclGoldPercent=100*((RsvExclGold/lag(RsvExclGold))-1))
  countryData <- mutate(countryData, chgInperUSDPercent=100*((PerUSD/lag(PerUSD))-1))
}

addGBIntRates = function(){
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2016-Q4',interestRates=0.25)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2017-Q1',interestRates=0.25)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2017-Q2',interestRates=0.25)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2017-Q3',interestRates=0.25)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2017-Q4',interestRates=0.50)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2018-Q1',interestRates=0.50)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2018-Q2',interestRates=0.50)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2018-Q3',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2018-Q4',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2019-Q1',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2019-Q2',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2019-Q3',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2019-Q4',interestRates=0.75)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2020-Q1',interestRates=0.10)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2020-Q2',interestRates=0.10)
  Britain_IR <- add_row(Britain_IR,countryCode='GB',yearQuarter='2020-Q3',interestRates=0.10)
  return(Britain_IR)
}

addEUIntRates = function(){
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2012-Q4',interestRates=0.75)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2013-Q1',interestRates=0.75)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2013-Q2',interestRates=0.75)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2015-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2015-Q3',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2015-Q4',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2016-Q1',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2016-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2016-Q3',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2016-Q4',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2017-Q1',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2017-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2017-Q3',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2017-Q4',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2018-Q1',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2018-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2018-Q3',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2018-Q4',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2019-Q1',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2019-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2019-Q3',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2019-Q4',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2020-Q1',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2020-Q2',interestRates=0.00)
  EU_IR <- add_row(EU_IR,countryCode='U2',yearQuarter='2020-Q3',interestRates=0.00)
  return(EU_IR)
}

#-------------US-------------

US_BOP = getBoP('US',1990,'Q')
US_IR = getIR('US',1990,'Q')
US_IR = US_IR[-c(124),]
US_RsvExclGold = getRsvExclGold('US',1990,'Q')
US_RsvExclGold = US_RsvExclGold[-c(124),]
US_USD = getFX('US',1990,'Q')
US_USD = US_USD[-c(124),]

US_Data = cbind(US_BOP,US_IR['interestRates'],US_RsvExclGold['RsvExclGold'],US_USD['PerUSD'])
US_Data = getChgInAllVars(US_Data)
US_Data = getChgInAllVarsPercentage(US_Data)
US_Data[is.na(US_Data)]=0
View(US_Data)
write.csv(US_Data,'US Data.csv')

#-------------UK-------------
Britain_BoP = getBoP('GB',1990,'Q')
Britain_IR = getIR('GB',1990,'Q')
#manual add from https://bankofengland.co.uk/boeapps/database/Bank-Rate.asp  Q4 2016 t0 Q3 2020
Britain_IR = addGBIntRates()
Britain_RsvExclGold = getRsvExclGold('GB',1990,'Q')
Britain_RsvExclGold = Britain_RsvExclGold[-c(124),]
GBP_USD = getFX('GB',1990,'Q')
GBP_USD = GBP_USD[-c(123),]


Britain_BoP=Britain_BoP[1:123,]
GBP_USD=GBP_USD[1:123,]
Britain_RsvExclGold=Britain_RsvExclGold[1:123,]
GB_Data = cbind(Britain_BoP,Britain_IR['interestRates'],Britain_RsvExclGold['RsvExclGold'],GBP_USD['PerUSD'])
GB_Data = getChgInAllVars(GB_Data)
#GB_Data = getChgInAllVarsPercentage(GB_Data)
GB_Data[is.na(GB_Data)]=0
View(GB_Data)
write.csv(GB_Data,'UK Data.csv')

#-------------Linear Model 1-------------
lm_GBP = lm(chgInperUSD ~ chgInCurrAcct  + chgInCapAcct +
              chgInFinAcct + chgInStatDisc +
              chgInintRates + chgInRsvExclGold, data = GB_Data)
summary(lm_GBP)
data_lasso1 = cbind.data.frame(GB_Data['chgInCurrAcct'], GB_Data['chgInCapAcct'], GB_Data['chgInFinAcct'],
                               GB_Data['chgInStatDisc'], GB_Data['chgInintRates'],
                               GB_Data['chgInRsvExclGold'],GB_Data['chgInperUSD'])

predlm = predict(lm_GBP,data_lasso1)
rmse(predlm, data_lasso1$chgInperUSD)

#Improve with LASSO

x1 = model.matrix(chgInperUSD~.,data_lasso1)[,-1]
y1 = data_lasso1$chgInperUSD

cv = cv.glmnet(x1,y1,alpha=1, type.measure = 'mse', nfolds = 5)


plot(model_lasso1)
coef(model_lasso1)
model_lasso1$dev.ratio
predictions = model_lasso1 %>% predict(x1) %>% as.vector()
rmse(predictions, data_lasso1$chgInperUSD)

#-------------Linear Model 2-------------
lm_GBP2 = lm(chgInperUSD ~ chgInintRates + chgInRsvExclGold, data = GB_Data)
summary(lm_GBP2)

GB_Data = read.csv("UK Data.csv")
#Improve with LASSO
data_lasso2 = cbind.data.frame(GB_Data['chgInperUSD'],GB_Data['chgInintRates'], GB_Data['chgInRsvExclGold'])

x2 = model.matrix(chgInperUSD~.,data_lasso2)[,-1]
y2 = data_lasso2$chgInperUSD

cv2 = cv.glmnet(x2,y2,alpha=1,type.measure = 'deviance', nfolds = 12)
model_lasso2 = glmnet(x2,y2, lambda = cv2$lambda.min)
coef(model_lasso2)
model_lasso2$dev.ratio

plot(cv2)
predictions = model_lasso2 %>% predict(x2) %>% as.vector()
rmse(predictions, data_lasso2$chgInperUSD)
lines(predictions,type = 'l', col = 'red')
plot(data_lasso1$chgInperUSD,type = 'l', col = 'blue')


#--------------Test using EUR------------------
EU_BoP = getBoP('U2',1999,'Q')
EU_IR = read_csv('EU Interest Rates.csv')
#EU_IR = getIR('U2',1999,'Q')
#EU_IR = addEUIntRates()
#write_csv(EU_IR,'EU Interest Rates.csv')
EU_RsvExclGold = getRsvExclGold('U2',1999,'Q')
EU_RsvExclGold = EU_RsvExclGold[-c(88),]
EU_USD = getFX('U2',1999,'Q')
EU_USD = EU_USD[-c(88),]
EU_USD=EU_USD[1:87,]
EU_BoP=EU_BoP[1:87,]
EU_RsvExclGold=EU_RsvExclGold[1:87,]
EU_Data = cbind(EU_BoP,EU_IR['interestRates'],EU_RsvExclGold['RsvExclGold'],EU_USD['PerUSD'])
EU_Data = getChgInAllVars(EU_Data)
EU_Data[is.na(EU_Data)]=0
View(EU_Data)
write.csv(EU_Data,'EU Data.csv')

EU_Data = read.csv("EU Data.csv")

EU_predict_data1 = cbind.data.frame(EU_Data['chgInCurrAcct'], EU_Data['chgInCapAcct'], EU_Data['chgInFinAcct'],
                                    EU_Data['chgInStatDisc'], EU_Data['chgInintRates'],
                                    EU_Data['chgInRsvExclGold'],EU_Data['chgInperUSD'])

EU_predict_data2 = cbind.data.frame(EU_Data['chgInperUSD'],EU_Data['chgInintRates'], EU_Data['chgInRsvExclGold'])

xEUR1 = model.matrix(chgInperUSD~.,EU_predict_data1)[,-1]
xEUR2 = model.matrix(chgInperUSD~.,EU_predict_data2)[,-1]

result_EUR1 = predict(model_lasso1, xEUR1)
result_EUR2 = predict(model_lasso2, xEUR2)

rmse(EU_Data$chgInperUSD, result_EUR1)

pred_EUR1 = c(0)
pred_EUR1[1] = EU_Data$PerUSD[1]
i = 2
for (i in 2:nrow(EU_Data)){
  pred_EUR1[i] = pred_EUR1[i-1] + result_EUR1[i]
}

rmse(EU_Data$PerUSD,pred_EUR1)

pred_EUR2 = c(0)
pred_EUR2[1] = EU_Data$PerUSD[1]
for (i in 2:nrow(EU_Data)){
  pred_EUR2[i] = pred_EUR2[i-1] + result_EUR2[i]
}

EUR_graph = cbind(EU_Data['yearQuarter'], EU_Data['PerUSD'],pred_EUR1)
names(EUR_graph) =c ('year_Quarter','Actual','Predicted')
EUR_graph[,1] = seq(as.Date("1999/1/1"), as.Date("2020/9/1"), by = "quarter")
EUR_graph[,2] =  ts(EUR_graph[,2], start = c(1999,1), frequency = 4)
EUR_graph2 = melt(EUR_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
EURplot1 = ggplot(EUR_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("EUR/USD")

EUR_graph = cbind(EU_Data['yearQuarter'], EU_Data['PerUSD'],pred_EUR2)
names(EUR_graph) =c ('year_Quarter','Actual','Predicted')
EUR_graph[,1] = seq(as.Date("1999/1/1"), as.Date("2020/9/1"), by = "quarter")
EUR_graph[,2] =  ts(EUR_graph[,2], start = c(1999,1), frequency = 4)
EUR_graph2 = melt(EUR_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
EURplot2 = ggplot(EUR_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("EUR/USD")

grid.arrange(EURplot1, EURplot2, nrow = 1)

#-------------VN-------------
VN_BoP = getBoP('VN',1996,'Q')
VN_IR = getIR('VN', 1996,'Q')
VN_IR = VN_IR[-c(98,99,100),]
VN_RsvExclGold = getRsvExclGold('VN',1996,'Q')
VN_RsvExclGold = VN_RsvExclGold[-c(98,99,100),]
VND_USD = getFX('VN',1996,'Q')
VND_USD = VND_USD[-c(98,99,100),]

VN_Data = cbind(VN_BoP,VN_IR['interestRates'],VN_RsvExclGold['RsvExclGold'],VND_USD['PerUSD'])
VN_Data = getChgInAllVars(VN_Data)
VN_Data[is.na(VN_Data)]=0
write_csv(VN_Data,'VN Data.csv')

VN_predict_data1 = cbind.data.frame(VN_Data['chgInCurrAcct'], VN_Data['chgInCapAcct'], VN_Data['chgInFinAcct'],
                                    VN_Data['chgInStatDisc'], VN_Data['chgInintRates'],
                                    VN_Data['chgInRsvExclGold'],VN_Data['chgInperUSD'])

VN_predict_data2 = cbind.data.frame(VN_Data['chgInperUSD'],VN_Data['chgInintRates'], VN_Data['chgInRsvExclGold'])

xVND1 = model.matrix(chgInperUSD~.,VN_predict_data1)[,-1]
xVND2 = model.matrix(chgInperUSD~.,VN_predict_data2)[,-1]

result_VND1 = predict(model_lasso1, xVND1)
result_VND2 = predict(model_lasso2, xVND2)

pred_VND1 = c(0)
pred_VND1[1] = VN_Data$PerUSD[1]
scaleVND = mean(VN_Data$PerUSD)
i = 2
for (i in 2:nrow(VN_Data)){
  pred_VND1[i] = pred_VND1[i-1] + 10000*result_VND1[i]
}

pred_VND2 = c(0)
pred_VND2[1] = VN_Data$PerUSD[1]
for (i in 2:nrow(VN_Data)){
  pred_VND2[i] = pred_VND2[i-1] + 10000*result_VND2[i]
}


VND_graph = cbind(VN_Data['yearQuarter'], VN_Data['PerUSD'],pred_VND1)
names(VND_graph) =c ('year_Quarter','Actual','Predicted')
VND_graph[,1] = seq(as.Date("1996/1/1"), as.Date("2020/3/1"), by = "quarter")
VND_graph[,2] =  ts(VND_graph[,2], start = c(1996,1), frequency = 4)
VND_graph2 = melt(VND_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
VNDplot1 = ggplot(VND_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("VND/USD")

VND_graph = cbind(VN_Data['yearQuarter'], VN_Data['PerUSD'],pred_VND2)
names(VND_graph) =c ('year_Quarter','Actual','Predicted')
VND_graph[,1] = seq(as.Date("1996/1/1"), as.Date("2020/3/1"), by = "quarter")
VND_graph[,2] =  ts(VND_graph[,2], start = c(1996,1), frequency = 4)
VND_graph2 = melt(VND_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
VNDplot2 = ggplot(VND_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("VND/USD")

grid.arrange(VNDplot1, VNDplot2, nrow = 1)


#-------------Switzerland--------------
CH_BoP = getBoP('CH',2000,'Q')
CH_BoP=CH_BoP[1:83,]
CH_IR = getIR('CH',2000,'Q')
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2019-Q2',interestRates=-0.25)
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2019-Q3',interestRates=-0.25)
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2019-Q4',interestRates=-0.25)
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2020-Q1',interestRates=-0.25)
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2020-Q2',interestRates=-0.25)
CH_IR <- add_row(CH_IR,countryCode='CH',yearQuarter='2020-Q3',interestRates=-0.25)
CH_RsvExclGold  = getRsvExclGold('CH',2000,'Q')
CH_RsvExclGold = CH_RsvExclGold[-c(84),]
CH_USD = getFX('CH',2000,'Q')
CH_USD = CH_USD[-c(84),]
CH_USD=CH_USD[1:83,]

CH_Data = cbind(CH_BoP,CH_IR['interestRates'],CH_RsvExclGold['RsvExclGold'],CH_USD['PerUSD'])
CH_Data = getChgInAllVars(CH_Data)
write_csv(CH_Data, "CH Data.csv")

CH_predict_data1 = cbind.data.frame(CH_Data['chgInCurrAcct'], CH_Data['chgInCapAcct'], CH_Data['chgInFinAcct'],
                                    CH_Data['chgInStatDisc'], CH_Data['chgInintRates'],
                                    CH_Data['chgInRsvExclGold'], CH_Data['chgInperUSD'])

CH_predict_data2 = cbind.data.frame(CH_Data['chgInperUSD'], CH_Data['chgInintRates'], CH_Data['chgInRsvExclGold'])

xCHF1 = model.matrix(chgInperUSD~.,CH_predict_data1)[,-1]
xCHF2 = model.matrix(chgInperUSD~.,CH_predict_data2)[,-1]

result_CHF1 = predict(model_lasso1, xCHF1)
result_CHF2 = predict(model_lasso2, xCHF2)

pred_CHF1 = c(0)
pred_CHF1[1] = CH_Data$PerUSD[1]
i = 2
for (i in 2:nrow(CH_Data)){
  pred_CHF1[i] = pred_CHF1[i-1] + result_CHF1[i]
}

pred_CHF2 = c(0)
pred_CHF2[1] = CH_Data$PerUSD[1]
for (i in 2:nrow(CH_Data)){
  pred_CHF2[i] = pred_CHF2[i-1] + result_CHF2[i]
}

CHF_graph = cbind(CH_Data['yearQuarter'], CH_Data['PerUSD'],pred_CHF1)
names(CHF_graph) =c ('year_Quarter','Actual','Predicted')
CHF_graph[,1] = seq(as.Date("2000/1/1"), as.Date("2020/9/1"), by = "quarter")
CHF_graph[,2] =  ts(CHF_graph[,2], start = c(2000,1), frequency = 4)
CHF_graph2 = melt(CHF_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
CHFplot1 = ggplot(CHF_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("CHF/USD")

CHF_graph = cbind(CH_Data['yearQuarter'], CH_Data['PerUSD'],pred_CHF2)
names(CHF_graph) =c ('year_Quarter','Actual','Predicted')
CHF_graph[,1] = seq(as.Date("2000/1/1"), as.Date("2020/9/1"), by = "quarter")
CHF_graph[,2] =  ts(CHF_graph[,2], start = c(2000,1), frequency = 4)
CHF_graph2 = melt(CHF_graph, id.vars = "year_Quarter", value.name = "FX_Rate")
CHFplot2 = ggplot(CHF_graph2, aes(x= year_Quarter, y= FX_Rate)) + geom_line(aes(color = variable))+
  xlab("Year") + ylab("CHF/USD")

grid.arrange(CHFplot1, CHFplot2, nrow = 1)

grid.arrange(VNDplot1, VNDplot2, CHFplot1, CHFplot2, nrow = 3)
