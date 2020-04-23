#==================Single exponential smoothing=========================#

# importing data set
rain = scan("https://robjhyndman.com/tsdldata/hurst/precip1.dat",skip = 1)
plot(rain)
class(rain)
      
# converting to ts class
rain = ts(rain, start = c(1813))
class(rain)
plot.ts(rain) #no trend and no seasonality

# frecast using holt winters(alpha respresents random fluctuation,
# beta is for trend and gamma is for seasinality)
forecast = HoltWinters(rain,beta = FALSE, gamma = FALSE)
forecast
forecast$fitted
plot(forecast)

library(forecast)

# forecasting for 8 more time points h=8 (here it's 8 more years)
forecast1 = forecast:::forecast.HoltWinters(forecast,h=8)
forecast1

# visualising forecast
forecast:::plot.forecast(forecast1)

# checking residual pattern
plot(forecast1$residuals) #no pattern, showing heteroscedasticity

# evaluating residuals normality
Box.test(forecast1$residuals,type = "Ljung-Box")

# checking auto correlation between residuals
Acf(forecast1$residuals,lag.max = 20)

# conclusion:: model has performed well

#==================Double exponential smoothing=========================#

# importing data set
skirts = scan("https://robjhyndman.com/tsdldata/roberts/skirts.dat",skip = 5)

# converting data set to ts class
skirtseries = ts(skirts,start = c(1866))

plot.ts(skirtseries) 
#decreasing trend after 1880 this implies that skirts hem diameter 
#has reduced over the time
#no seasonality observed

# holtwinter model for trend and random fluctuation
skirt_forecast = HoltWinters(skirtseries, gamma = FALSE)

skirt_forecast
# alpha = 0.8383 
# 0 is total smoothing and 1 is no smoothing
# 0 implies less importance to most recent data
# 1 implies more importance to most recent data
# skirts with large hem will go out of fashion

plot(skirt_forecast)
# actual and forecasted values are close to each other

# forecasting for the time period beyod the actual data
skirt_forecast_2 = forecast:::forecast.HoltWinters(skirt_forecast,h=19)
# 19 years beyond 1910
skirt_forecast_2

forecast:::plot.forecast(skirt_forecast_2)
plot(skirt_forecast_2$fitted)

# forecasted values
skirt_forecast_2$mean
skirt_forecast_2$upper
skirt_forecast_2$lower

# check for auto correlation
Box.test(skirt_forecast_2$residuals, lag = 20, type = 'Ljung-Box') 
# p-value > 0.05 auto correlation between residuals is not significant

Acf(skirt_forecast_2$residuals,lag.max = 20) 
# correlation seems <40%
# no auto correlation between residuals, does not need further smoothing

# checking normality of residuals
plot(skirt_forecast_2$residuals)
hist(skirt_forecast_2$residuals)

# residuals are randomly distributed
# model performs well


#==================Tripple exponential smoothing=========================#
# importing data set
sales = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
# data set of souvenir sales 

# converting data to ts class
sales_series = ts(sales,frequency = 12,start = c(1987,1))
plot.ts(sales_series)

# pattern seems multiplicative
# seasonal fluctuation and random fluctuation seems 
# increasing with level of the time series

# log transformation of time series to convert it to additive
logsales = log(sales_series)
plot.ts(logsales)

sales_forecast = HoltWinters(logsales)
# there is trend and also the seasonality

sales_forecast
# alpha is 0.4 (not close to 0 nor to 1)

plot(sales_forecast)
# forecast seems closer to actual values

# forecasting for next 48 months
sales_forecast2 = forecast:::forecast.HoltWinters(sales_forecast, h=48)
forecast:::plot.forecast(sales_forecast2)

sales_forecast2$mean

# checking residuals auto correlation
Box.test(sales_forecast2$residuals,type = "Ljung-Box",lag = 20)
# P value is >0.05 implies no serial auto correlation

Acf(sales_forecast2$residuals,lag.max = 20)
# no auto correlation observed 

# checking residuals pattern
plot(sales_forecast2$residuals) # random fluctuation observed





