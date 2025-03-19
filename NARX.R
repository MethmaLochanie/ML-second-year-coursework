library(readxl)
library(neuralnet)
library(MLmetrics)
library(Metrics)
library(dplyr)

#NARX approch

electricity_consumption <- read_excel("uow_consumption.xlsx")
#dividing the dataset into training and testing
training_dataset <- electricity_consumption[1:380,]
testing_dataset <- electricity_consumption[380:nrow(electricity_consumption),]

#part b
hourly_consumption_train <- training_dataset[2:4]
hourly_consumption_test <- testing_dataset[2:4]
#t-1 as input(1) and t as output
#for training
train_t1 <- bind_cols(previous_t1 = lag(hourly_consumption_train,1),
                      current = hourly_consumption_train)
#for testing
test_t1 <- bind_cols(previous_t1 = lag(hourly_consumption_test,1),
                     current = hourly_consumption_test)

#t-2, t-1 as inputs(2) and t as output
#for training
train_t2 <- bind_cols(previous_t2 = lag(hourly_consumption_train,2),
                      previous_t1 = lag(hourly_consumption_train,1),
                      current = hourly_consumption_train)
#for testing
test_t2 <- bind_cols(previous_t2 = lag(hourly_consumption_test,2),
                     previous_t1 = lag(hourly_consumption_test,1),
                     current = hourly_consumption_test)


#t-3,t-2, t-1 as inputs(3) and t as output
#for training
train_t3 <- bind_cols(previous_t3 = lag(hourly_consumption_train,3),
                      previous_t2 = lag(hourly_consumption_train,2), 
                      previous_t1 = lag(hourly_consumption_train,1),
                      current = hourly_consumption_train)
#for testing
test_t3 <- bind_cols(previous_t3 = lag(hourly_consumption_test,3),
                     previous_t2 = lag(hourly_consumption_test,2),
                     previous_t1 = lag(hourly_consumption_test,1),
                     current = hourly_consumption_test)


#t-4, t-3,t-2, t-1 as inputs(4) and t as output
#for training
train_t4 <- bind_cols(previous_t4 = lag(hourly_consumption_train,4), 
                      previous_t3 = lag(hourly_consumption_train,3),
                      previous_t2 = lag(hourly_consumption_train,2),
                      previous_t1 = lag(hourly_consumption_train,1),
                      current = hourly_consumption_train)
#for testing
test_t4 <- bind_cols(previous_t4 = lag(hourly_consumption_test,4),
                     previous_t3 = lag(hourly_consumption_test,3),
                     previous_t2 = lag(hourly_consumption_test,2),
                     previous_t1 = lag(hourly_consumption_test,1),
                     current = hourly_consumption_test)


#t-7, t-6, t-5,t-4, t-3,t-2, t-1 as inputs(7) and t as output
#for training
train_t7 <- bind_cols(previous_t7 = lag(hourly_consumption_train,7),
                      previous_t6 = lag(hourly_consumption_train,6),
                      previous_t5 = lag(hourly_consumption_train,5),
                      previous_t4 = lag(hourly_consumption_train,4), 
                      previous_t3 = lag(hourly_consumption_train,3),
                      previous_t2 = lag(hourly_consumption_train,2), 
                      previous_t1 = lag(hourly_consumption_train,1),
                      current = hourly_consumption_train)
#for testing
test_t7 <- bind_cols(previous_t7 = lag(hourly_consumption_test,7),
                     previous_t6 = lag(hourly_consumption_test,6),
                     previous_t5 = lag(hourly_consumption_test,5),
                     previous_t4 = lag(hourly_consumption_test,4),
                     previous_t3 = lag(hourly_consumption_test,3),
                     previous_t2 = lag(hourly_consumption_test,2),
                     previous_t1 = lag(hourly_consumption_test,1),
                     current = hourly_consumption_test)


#removing current 18th and 19th value
train_t1 <- train_t1[, -c(4, 5)]
test_t1 <- test_t1[, -c(4, 5)]

train_t2 <- train_t2[,-c(7,8)]
test_t2 <- test_t2[,-c(7,8)]

train_t3 <- train_t3[,-c(10,11)]
test_t3 <- test_t3[,-c(10,11)]

train_t4 <- train_t4[,-c(13,14)]
test_t4 <- test_t4[,-c(13,14)]

train_t7 <- train_t7[,-c(22,23)]
test_t7 <- test_t7[,-c(22,23)]

#complete.cases function to remove those rows with NA
train_t1 <- train_t1[complete.cases(train_t1),]
test_t1 <- test_t1[complete.cases(test_t1),]

train_t2 <- train_t2[complete.cases(train_t2),]
test_t2 <- test_t2[complete.cases(test_t2),]

train_t3 <- train_t3[complete.cases(train_t3),]
test_t3 <- test_t3[complete.cases(test_t3),]

train_t4 <- train_t4[complete.cases(train_t4),]
test_t4 <- test_t4[complete.cases(test_t4),]

train_t7 <- train_t7[complete.cases(train_t7),]
test_t7 <- test_t7[complete.cases(test_t7),]

#rename the datasets I/O matrices columns
colnames(train_t1) <- c("t_18_1","t_19_1","t_20_1","current_hour_20")
colnames(test_t1) <- c("t_18_1","t_19_1","t_20_1","current_hour_20")

colnames(train_t2) <- c("t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")
colnames(test_t2) <- c("t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")

colnames(train_t3) <- c("t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")
colnames(test_t3) <- c("t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")

colnames(train_t4) <- c("t_18_4","t_19_4","t_20_4","t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")
colnames(test_t4) <- c("t_18_4","t_19_4","t_20_4","t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")

colnames(train_t7) <- c("t_18_7","t_19_7","t_20_7","t_18_6","t_19_6","t_20_6","t_18_5","t_19_5","t_20_5","t_18_4","t_19_4","t_20_4","t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")
colnames(test_t7) <- c("t_18_7","t_19_7","t_20_7","t_18_6","t_19_6","t_20_6","t_18_5","t_19_5","t_20_5","t_18_4","t_19_4","t_20_4","t_18_3","t_19_3","t_20_3","t_18_2","t_19_2","t_20_2","t_18_1","t_19_1","t_20_1","current_hour_20")

#normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#normalizing each I/O matrix column by column in both training and testing data sets
train_t1_norm <- apply(train_t1, 2, normalize)
test_t1_norm <- apply(test_t1, 2, normalize)

train_t2_norm <- apply(train_t2, 2, normalize)
test_t2_norm <- apply(test_t2, 2, normalize)

train_t3_norm <- apply(train_t3, 2, normalize)
test_t3_norm <- apply(test_t3, 2, normalize)

train_t4_norm <- apply(train_t4, 2, normalize)
test_t4_norm <- apply(test_t4, 2, normalize)

train_t7_norm <- apply(train_t7, 2, normalize)
test_t7_norm <- apply(test_t7, 2, normalize)

#creating models to predict consumption outputs using I/O matrices of normalize training data sets
t1_model_1 <- neuralnet(current_hour_20 ~ t_18_1 + t_19_1 + t_20_1, hidden = 6,
                        data = train_t1_norm, linear.output = TRUE)
t2_model_2 <- neuralnet(current_hour_20 ~ t_18_1 + t_19_1 + t_20_1+ t_18_2 + t_19_2 + t_20_2,  hidden = 5,
                        data = train_t2_norm, linear.output= TRUE)
t3_model_3 <- neuralnet(current_hour_20 ~ t_18_1 + t_19_1 + t_20_1 + t_18_2 + t_19_2 + t_20_2 + t_18_3 + t_19_3 + t_20_3, hidden = 6, 
                        data = train_t3_norm,linear.output = FALSE)
t4_model_4 <- neuralnet(current_hour_20 ~ t_18_1 + t_19_1 + t_20_1+ t_18_2 + t_19_2 + t_20_2 + t_18_3 + t_19_3 + t_20_3 + t_18_4 + t_19_4 + t_20_4, hidden = 4, 
                        data = train_t4_norm, linear.output = TRUE)
t7_model_5 <- neuralnet(current_hour_20 ~ t_18_1 + t_19_1 + t_20_1+ t_18_2 + t_19_2 + t_20_2 + t_18_3 + t_19_3 + t_20_3 + t_18_4 + t_19_4 + t_20_4 + t_18_5 + t_19_5 + t_20_5 + t_18_6 +
                          t_19_6 + t_20_6 + t_18_7 + t_19_7 + t_20_7, hidden = 8, 
                        data = train_t7_norm, linear.output = TRUE)

#evaluating model
#getting minimum and maximum values from the original data set
min_data <- min(hourly_consumption_train)
max_data <- max(hourly_consumption_train)

#Function to un-normalize(Min-max normalization)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
#t1
######################## use only the testing input variables for model 1 ########################
  
t1_results_1 <- predict(t1_model_1, newdata = test_t1_norm)

#de-normalize the normalized NN’s output
t1_predict_denorm_1 <- unnormalize(t1_results_1, min_data,max_data)

#RMSE function
t1_rmse_1 <- rmse(test_t1$current_hour_20,t1_predict_denorm_1)
t1_rmse_1
# Calculate MAE
t1_mae_1 <- mae(test_t1$current_hour_20,t1_predict_denorm_1)
t1_mae_1
# Calculate MAPE
t1_mape_1 <- mape(test_t1$current_hour_20,t1_predict_denorm_1)
t1_mape_1
# Calculate sMAPE
t1_smape_1 <- smape(test_t1$current_hour_20,t1_predict_denorm_1)
t1_smape_1

#clear structure of actual and predicted output 
t1_cleanoutput_1 <- cbind(test_t1$current_hour_20,t1_predict_denorm_1)
print("Model 1")
colnames(t1_cleanoutput_1) <- c("Acual Consumption ", "Predicted Consumption")
head(t1_cleanoutput_1)

######################## use only the testing input variables for model 2 ########################

t2_results_2 <- predict(t2_model_2, newdata = test_t2_norm)

#de-normalize the normalized NN’s output
t2_predict_denorm_2 <- unnormalize(t2_results_2, min_data,max_data)

#RMSE function
t2_rmse_2 <- rmse(test_t2$current_hour_20,t2_predict_denorm_2)
t2_rmse_2
# Calculate MAE
t2_mae_2 <- mae(test_t2$current_hour_20,t2_predict_denorm_2)
t2_mae_2
# Calculate MAPE
t2_mape_2 <- mape(test_t2$current_hour_20,t2_predict_denorm_2)
t2_mape_2
# Calculate sMAPE
t2_smape_2 <- smape(test_t2$current_hour_20,t2_predict_denorm_2)
t2_smape_2

#clear structure of actual and predicted output 
t2_cleanoutput_2 <- cbind(test_t2$current_hour_20,t2_predict_denorm_2)
print("Model 2")
colnames(t2_cleanoutput_2) <- c("Acual Consumption ", "Predicted Consumption")
head(t2_cleanoutput_2)

######################## use only the testing input variables for model 3 ########################

t3_results_3 <- predict(t3_model_3, newdata = test_t3_norm)

#de-normalize the normalized NN’s output
t3_predict_denorm_3 <- unnormalize(t3_results_3, min_data,max_data)

#RMSE function
t3_rmse_3 <- rmse(test_t3$current_hour_20,t3_predict_denorm_3)
t3_rmse_3
# Calculate MAE
t3_mae_3 <- mae(test_t3$current_hour_20,t3_predict_denorm_3)
t3_mae_3
# Calculate MAPE
t3_mape_3 <- mape(test_t3$current_hour_20,t3_predict_denorm_3)
t3_mape_3
# Calculate sMAPE
t3_smape_3 <- smape(test_t3$current_hour_20,t3_predict_denorm_3)
t3_smape_3

#clear structure of actual and predicted output 
t3_cleanoutput_3 <- cbind(test_t3$current_hour_20,t3_predict_denorm_3)
print("Model 3")
colnames(t3_cleanoutput_3) <- c("Acual Consumption ", "Predicted Consumption")
head(t3_cleanoutput_3)

######################## use only the testing input variables for model 4 ########################

t4_results_4 <- predict(t4_model_4, newdata = test_t4_norm)

#de-normalize the normalized NN’s output
t4_predict_denorm_4 <- unnormalize(t4_results_4, min_data,max_data)

#RMSE function
t4_rmse_4 <- rmse(test_t4$current_hour_20,t4_predict_denorm_4)
t4_rmse_4
# Calculate MAE
t4_mae_4 <- mae(test_t4$current_hour_20,t4_predict_denorm_4)
t4_mae_4
# Calculate MAPE
t4_mape_4 <- mape(test_t4$current_hour_20,t4_predict_denorm_4)
t4_mape_4
# Calculate sMAPE
t4_smape_4 <- smape(test_t4$current_hour_20,t4_predict_denorm_4)
t4_smape_4

#clear structure of actual and predicted output 
t4_cleanoutput_4 <- cbind(test_t4$current_hour_20,t4_predict_denorm_4)
print("Model 4")
colnames(t4_cleanoutput_4) <- c("Acual Consumption ", "Predicted Consumption")
head(t4_cleanoutput_4)

######################## use only the testing input variables for model 5 ########################

t7_results_5 <- predict(t7_model_5, newdata = test_t7_norm)

#de-normalize the normalized NN’s output
t7_predict_denorm_5 <- unnormalize(t7_results_5, min_data,max_data)

#RMSE function
t7_rmse_5 <- rmse(test_t7$current_hour_20,t7_predict_denorm_5)
t7_rmse_5
# Calculate MAE
t7_mae_5 <- mae(test_t7$current_hour_20,t7_predict_denorm_5)
t7_mae_5
# Calculate MAPE
t7_mape_5 <- mape(test_t7$current_hour_20,t7_predict_denorm_5)
t7_mape_5
# Calculate sMAPE
t7_smape_5 <- smape(test_t7$current_hour_20,t7_predict_denorm_5)
t7_smape_5

#clear structure of actual and predicted output 
t7_cleanoutput_5 <- cbind(test_t7$current_hour_20,t7_predict_denorm_5)
print("Model 5")
colnames(t7_cleanoutput_5) <- c("Acual Consumption ", "Predicted Consumption")
head(t7_cleanoutput_5)



#real Vs predicted NN plot
par(mfrow=c(1,1))
plot(test_t2$current_hour_20, t2_predict_denorm_2 ,col="red",main='Real vs predicted NN',pch=18,cex=0.7)
abline(a=0, b=1, h=90, v=90)