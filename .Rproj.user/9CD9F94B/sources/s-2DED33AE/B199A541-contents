library(lubridate) #Package for working with date data. This saves us so much headaches with base R date management
library(fpp2)# a package that loads a number of useful time series packages
#This is clearly a time-series problem. I have no used time-series methodologies before, but this will be my best attempt!

# Making date continious  -------------------------------------------------
#For future analysis, it will be convenient to have a single date variable 
Transactions_KnownVendor$date <- #we use the function ymd, which takes a string as an input and converts it to a date format
  ymd(paste(2020,Transactions_KnownVendor$month , Transactions_KnownVendor$day , sep = "-" ))#paste takes any vector as an input and converts it to a string.
#in the above, we are inputting 2020 as the year as this data contains Feb 29, which is only defined for leap years

#Lets start with a *bit more* EDA.
#Lets collapse the individual transactions into the days, so we have a revenue time plot
DailyAdjRev <- Transactions_KnownVendor %>% group_by(date) %>% summarize(DailyRevenue = sum(AdjRev))

DailyAdjRev$DailyRevenue %>% ts %>% autoplot(xlab = "day", ylab = "Daily Adjusted Revenue") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#so we can see what appears as a general downward trend
#Note that since we only have data for 6 months, this looks like a general downward trend but with more
#data, say data going back a year or two, we may find that this is a seasonal pattern for the first and second quarters
#We can also see what may be an inflection point around day 150, where the trend starts to increase again
#as we move into the summer months, but again without more data we cant be certain

#Our goal is to predict the adjusted revenue, based on if vendor37 had remained open or not
#Adjusted revenue is a function of the revenue subtracting the cut the vendors take.
#As the adjusted revenue variable I created already takes the subtraction from each vendor, we can model
#daily revenue as a function of the count of transactions from the different vendors

#Lets create variables that are the count of the vendor transactions for a given day
DailyRevAndVendors <-
  Transactions_KnownVendor %>% 
  group_by(date) %>% 
  summarize(DailyRevenue = sum(AdjRev), 
            vendor11 = length(which(vendor=="vendor11")),
            vendor26 = length(which(vendor=="vendor26")),
            vendor33 = length(which(vendor=="vendor33")),
            vendor36 = length(which(vendor=="vendor36")),
            vendor37 = length(which(vendor=="vendor37")),
            vendor42 = length(which(vendor=="vendor42")), ) %>% 
  .[,-1] %>%  ts #these last two calls removes the date from the table and converts the table to a ts object

#lets look at a time plot for daily transaction volume of the vendors
autoplot(DailyRevAndVendors[,-1], main = "Daily Volume by Vendor", ylab = "# Transactions", xlab = "Day")

#lets split the data into the test/train sets again. In this case, I am going to split the data 
#at the same days; the training set will be the subset of days before 72, while the test 
#set will be the days between 72 and the day vendor37 closes. This will let me test the accuracy 
#of the model prediction over a period where we have data

trainset <- DailyRevAndVendors %>% window(start = 1, end = 72) #grab the first 80% of the series
testset <-  DailyRevAndVendors %>% window(start = 73, end = 91) #grab the last 20% for training

#lets start with building a linear regression with revenue as the outcome and vendor count as the predictors.
#we'll also include the trend of the revenue as a predictor
linmod <- tslm(DailyRevenue ~vendor11+
     vendor26+
     vendor33+
     vendor36+
     vendor37+
     vendor42 + trend, data = trainset)
summary(linmod)
checkresiduals(linmod)
#So it looks like there is a problem with autocorrelation in the residuals
#We can make a better model by modeling the error as an ARIMA(p,d,q) series
#This can be done by using the auto.arima function, where we provide the first argument as the daily revenue
#and specify the predictors with xreg. The arguments stepwise and approximation being false results in a 
#more accurate model, at computational cost

fit <- auto.arima(trainset[,1],xreg = trainset[,-1],stepwise = FALSE, approximation = FALSE)
fit

checkresiduals(fit)
#There is an positive outlier residual, and the boxplot looks somewhat skewed. There is also some significant
#autocorrelation in the residuals, so the prediction intervals may not provide accurate coverage
#still, lets forecast the next 18 days and see how well that matches the actual data
RevForecast1 <- forecast(fit, xreg = testset[,-1])

autoplot(window(DailyRevAndVendors[,1], start = 1, end = 91),xlab = "Day", ylab = "Adjusted Revenue",main = "Forecasted revenue vs Actual revenue")+
  autolayer(RevForecast1) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#so this actually predicts the trend quite well
#now lets apply this method past the date where vendor37 closed

##This will be simple, since we already have the data for this period
##We'll start by using the data where vendor37 counts are 0, then compare this to a situation where we 
##estimate some values of vendor37

predictionset <- DailyRevAndVendors %>% window(start = 73, end = 121) #lets grab points 30 days after vendor closure

RevForecast2 <- forecast(fit, xreg = predictionset[,-1])
autoplot(RevForecast2)

#Lets see how this compares to the actual values. We wont display the prediction error, as that would cover the observed values
autoplot(window(DailyRevAndVendors[,1], start = 1, end = 121),xlab = "Day", ylab = "Adjusted Revenue",main = "Forecasted revenue vs Actual revenue")+
  autolayer(RevForecast2,PI = F, series = ) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#this predicts the shape of the trend very well, if underestimating the effect

#Now, lets try and forecast the daily adjusted revenue if vendor 37 hadnt closed
#there are a couple potential ways to do this. For simplicity, we'll just take the mean count of the transactions 
#from vendor37 before closure
window(DailyRevAndVendors[,"vendor37"],start = 1, end =91) %>% mean 
#approximately 21
predictionset2 <- predictionset
predictionset2[,"vendor37"][predictionset2[,"vendor37"]==0] <- 21

#forecast the data where vendor37 didn't close
RevForecast3 <- forecast(fit, xreg = predictionset2[,-1])
autoplot(RevForecast3)

#lets superimpose everything onto one plot
autoplot(window(DailyRevAndVendors[,1], start = 1, end = 121),xlab = "Day", ylab = "Adjusted Revenue",main = "Forecasted revenue vs Actual revenue")+
  autolayer(RevForecast2,PI = F, series = "Vendor 37 closed") +
  autolayer(RevForecast3,PI = F, series = "Vendor 37 open")
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  
#We can see that Vendor 37 remaining open would not have meaningfully impacted our revenue going forward
#this is consistent with my earlier hypothesis, that the closure of vendor 37 simply coincided with a general 
#decrease in sales
