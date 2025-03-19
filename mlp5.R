library(readxl)
library(keras)
library(dplyr)

electricity_consumption <- read_xlsx("uow_consumption.xlsx")

#subtask 1
#part b
hourly_consumption_20_Hour <- electricity_consumption[c("0.83333333333333337")]

#t-1 as input and t as output
t1 <- bind_cols(previous_t1 = lag(hourly_consumption_20_Hour,1),
                      current = hourly_consumption_20_Hour)

#t-2, t-1 as input and t as output
t2 <- bind_cols(previous_t2 = lag(hourly_consumption_20_Hour,2),
                      previous_t1 = lag(hourly_consumption_20_Hour,1),
                      current = hourly_consumption_20_Hour_train)




#real Vs predicted NN plot
par(mfrow=c(1,1))
plot(test_t1$t, t1_predict_denorm ,col="red",main='Real vs predicted NN',pch=18,cex=0.7)
abline(a=0, b=1, h=90, v=90)

#electricity consumption prediction plot
x = 1:length(test_t1$t)
plot(x, test_t1$t, col = "red", type = "l", lwd=2,
     main = "Electricity Consumption prediction")
lines(x, t1_predict_denorm, col = "blue", lwd=2)
legend("topright",  legend = c("original-consumption", "predicted-consumption"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid() 