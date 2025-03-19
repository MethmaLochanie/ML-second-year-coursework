library(readxl)
library(keras)
library(dplyr)
library(neuralnet)
library(Metrics)

electricity_consumption <- read_xlsx("uow_consumption.xlsx")
#dividing the dataset into training and testing
training_dataset <- electricity_consumption[1:380,]
testing_dataset <- electricity_consumption[380:nrow(electricity_consumption),]

#subtask 1
#part b
hourly_consumption_20_Hour_train <- training_dataset["0.83333333333333337"]
hourly_consumption_20_Hour_test <- testing_dataset["0.83333333333333337"]

#t-1 as input(1) and t as output
#for training
train_t1 <- bind_cols(previous_t1 = lag(hourly_consumption_20_Hour_train,1),
                     current = hourly_consumption_20_Hour_train)
#for testing
test_t1 <- bind_cols(previous_t1 = lag(hourly_consumption_20_Hour_test,1),
                      current = hourly_consumption_20_Hour_test)


#t-2, t-1 as inputs(2) and t as output
#for training
train_t2 <- bind_cols(previous_t2 = lag(hourly_consumption_20_Hour_train,2),
                      previous_t1 = lag(hourly_consumption_20_Hour_train,1),
                      current = hourly_consumption_20_Hour_train)
#for testing
test_t2 <- bind_cols(previous_t2 = lag(hourly_consumption_20_Hour_test,2),
                     previous_t1 = lag(hourly_consumption_20_Hour_test,1),
                     current = hourly_consumption_20_Hour_test)


#t-3,t-2, t-1 as inputs(3) and t as output
#for training
train_t3 <- bind_cols(previous_t3 = lag(hourly_consumption_20_Hour_train,3),
                      previous_t2 = lag(hourly_consumption_20_Hour_train,2), 
                      previous_t1 = lag(hourly_consumption_20_Hour_train,1),
                      current = hourly_consumption_20_Hour_train)
#for testing
test_t3 <- bind_cols(previous_t3 = lag(hourly_consumption_20_Hour_test,3),
                     previous_t2 = lag(hourly_consumption_20_Hour_test,2),
                     previous_t1 = lag(hourly_consumption_20_Hour_test,1),
                     current = hourly_consumption_20_Hour_test)


#t-4, t-3,t-2, t-1 as inputs(4) and t as output
#for training
train_t4 <- bind_cols(previous_t4 = lag(hourly_consumption_20_Hour_train,4), 
                      previous_t3 = lag(hourly_consumption_20_Hour_train,3),
                      previous_t2 = lag(hourly_consumption_20_Hour_train,2),
                      previous_t1 = lag(hourly_consumption_20_Hour_train,1),
                      current = hourly_consumption_20_Hour_train)
#for testing
test_t4 <- bind_cols(previous_t4 = lag(hourly_consumption_20_Hour_test,4),
                     previous_t3 = lag(hourly_consumption_20_Hour_test,3),
                     previous_t2 = lag(hourly_consumption_20_Hour_test,2),
                     previous_t1 = lag(hourly_consumption_20_Hour_test,1),
                     current = hourly_consumption_20_Hour_test)


#t-7, t-6, t-5,t-4, t-3,t-2, t-1 as inputs(7) and t as output
#for training
train_t7 <- bind_cols(previous_t7 = lag(hourly_consumption_20_Hour_train,7),
                      previous_t6 = lag(hourly_consumption_20_Hour_train,6),
                      previous_t5 = lag(hourly_consumption_20_Hour_train,5),
                      previous_t4 = lag(hourly_consumption_20_Hour_train,4), 
                      previous_t3 = lag(hourly_consumption_20_Hour_train,3),
                      previous_t2 = lag(hourly_consumption_20_Hour_train,2), 
                      previous_t1 = lag(hourly_consumption_20_Hour_train,1),
                      current = hourly_consumption_20_Hour_train)
#for testing
test_t7 <- bind_cols(previous_t7 = lag(hourly_consumption_20_Hour_test,7),
                     previous_t6 = lag(hourly_consumption_20_Hour_test,6),
                     previous_t5 = lag(hourly_consumption_20_Hour_test,5),
                     previous_t4 = lag(hourly_consumption_20_Hour_test,4),
                     previous_t3 = lag(hourly_consumption_20_Hour_test,3),
                     previous_t2 = lag(hourly_consumption_20_Hour_test,2),
                     previous_t1 = lag(hourly_consumption_20_Hour_test,1),
                     current = hourly_consumption_20_Hour_test)


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
colnames(train_t1) <- c("t_1", "t")
colnames(test_t1) <- c("t_1", "t")

colnames(train_t2) <- c("t_2","t_1", "t")
colnames(test_t2) <- c("t_2","t_1", "t")

colnames(train_t3) <- c("t_3","t_2","t_1", "t")
colnames(test_t3) <- c("t_3","t_2","t_1", "t")

colnames(train_t4) <- c("t_4","t_3","t_2","t_1", "t")
colnames(test_t4) <- c("t_4","t_3","t_2","t_1", "t")

colnames(train_t7) <- c("t_7","t_6","t_5","t_4","t_3","t_2","t_1", "t")
colnames(test_t7) <- c("t_7","t_6","t_5","t_4","t_3","t_2","t_1", "t")

#part c
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

#part d
#creating models to predict consumption outputs using I/O matrices of normalize training data sets
t1_model_1 <- neuralnet(t~t_1, hidden = 2, 
                      data = train_t1_norm, linear.output = TRUE)
t2_model_2 <- neuralnet(t~t_1 + t_2, hidden = 2, 
                      data = train_t2_norm, linear.output = TRUE)
t3_model_3 <- neuralnet(t~t_1+ t_2 + t_3, hidden = 2, 
                      data = train_t3_norm, linear.output = TRUE)
t4_model_4 <- neuralnet(t~t_1+ t_2 + t_3 + t_4, hidden = 2, 
                      data = train_t4_norm, linear.output = TRUE)
t7_model_5 <- neuralnet(t~t_1+ t_2 + t_3 + t_4 + t_5 + t_6 +t_7, hidden = 2, 
                      data = train_t7_norm, linear.output = TRUE)

# find original data sets maximum & minimum value
train_t1_min <- min(train_t1$t)
train_t1_max <- max(train_t1$t)

train_t2_min <- min(train_t2$t)
train_t2_max <- max(train_t2$t)

train_t3_min <- min(train_t3$t)
train_t3_max <- max(train_t3$t)

train_t4_min <- min(train_t4$t)
train_t4_max <- max(train_t4$t)

train_t7_min <- min(train_t7$t)
train_t7_max <- max(train_t7$t)

#Create the reverse of normalized function – de-normalized
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

######################## use only the testing input variables for model 1 ########################
t1_results_1 <- compute(t1_model_1, test_t1_norm)  
# obtain predicted consumption values
t1_prediction_1 <- t1_results_1$net.result    

#de-normalize the normalized NN’s output
t1_predict_denorm_1 <- unnormalize(t1_prediction_1, train_t1_min, train_t1_max)

#RMSE function
t1_rmse_1 <- rmse(test_t1$t, t1_predict_denorm_1)
t1_rmse_1
# Calculate MAE
t1_mae_1 <- mae(test_t1$t , t1_predict_denorm_1)
t1_mae_1
# Calculate MAPE
t1_mape_1 <- mape(test_t1$t, t1_predict_denorm_1)
t1_mape_1
# Calculate sMAPE
t1_smape_1 <- smape(test_t1$t, t1_predict_denorm_1)
t1_smape_1

#clear structure of actual and predicted output 
t1_cleanoutput_1 <- cbind(test_t1$t, t1_predict_denorm_1)
print("Model 1")
colnames(t1_cleanoutput_1) <- c("Acual Consumption ", "Predicted Consumption")
head(t1_cleanoutput_1)

######################## use only the testing input variables for model 2 ########################
t2_results_2 <- compute(t2_model_2, test_t2_norm)  
# obtain predicted consumption values
t2_prediction_2 <- t2_results_2$net.result    

#de-normalize the normalized NN’s output
t2_predict_denorm_2 <- unnormalize(t2_prediction_2, train_t2_min, train_t2_max)

#RMSE function
t2_rmse_2 <- rmse(test_t2$t, t2_predict_denorm_2)
t2_rmse_2
# Calculate MAE
t2_mae_2 <- mae(test_t2$t, t2_predict_denorm_2)
t2_mae_2
# Calculate MAPE
t2_mape_2 <- mape(test_t2$t, t2_predict_denorm_2)
t2_mape_2
# Calculate sMAPE
t2_smape_2 <- smape(test_t2$t, t2_predict_denorm_2)
t2_smape_2

#clear structure od actual and predicted output 
t2_cleanoutput_2 <- cbind(test_t2$t, t2_predict_denorm_2)
print("Model 2")
colnames(t2_cleanoutput_2) <- c("Acual Consumption", "Predicted Consumption")
head(t2_cleanoutput_2)

######################## use only the testing input variables for model 3 ########################
t3_results_3 <- compute(t3_model_3, test_t3_norm)  
# obtain predicted consumption values
t3_prediction_3 <- t3_results_3$net.result    

#de-normalize the normalized NN’s output
t3_predict_denorm_3 <- unnormalize(t3_prediction_3, train_t3_min, train_t3_max)

#RMSE function
t3_rmse_3 <- rmse(test_t3$t, t3_predict_denorm_3)
t3_rmse_3
# Calculate MAE
t3_mae_3 <- mae(test_t3$t, t3_predict_denorm_3)
t3_mae_3
# Calculate MAPE
t3_mape_3 <- mape(test_t3$t, t3_predict_denorm_3)
t3_mape_3
# Calculate sMAPE
t3_smape_3 <- smape(test_t3$t, t3_predict_denorm_3)
t3_smape_3

#clear structure od actual and predicted output 
t3_cleanoutput_3 <- cbind(test_t3$t, t3_predict_denorm_3)
print("Model 3")
colnames(t3_cleanoutput_3) <- c("Acual Consumption", "Predicted Consumption")
head(t3_cleanoutput_3)

######################## use only the testing input variables for model 4 ########################
t4_results_4 <- compute(t4_model_4, test_t4_norm)  
# obtain predicted consumption values
t4_prediction_4 <- t4_results_4$net.result    

#de-normalize the normalized NN’s output
t4_predict_denorm_4 <- unnormalize(t4_prediction_4, train_t4_min, train_t4_max)

#RMSE function
t4_rmse_4 <- rmse(test_t4$t, t4_predict_denorm_4)
t4_rmse_4
# Calculate MAE
t4_mae_4 <- mae(test_t4$t, t4_predict_denorm_4)
t4_mae_4
# Calculate MAPE
t4_mape_4 <- mape(test_t4$t, t4_predict_denorm_4)
t4_mape_4
# Calculate sMAPE
t4_smape_4 <- smape(test_t4$t, t4_predict_denorm_4)
t4_smape_4

#clear structure od actual and predicted output 
t4_cleanoutput_4 <- cbind(test_t4$t, t4_predict_denorm_4)
print("Model 4")
colnames(t4_cleanoutput_4) <- c("Acual Consumption", "Predicted Consumption")
head(t4_cleanoutput_4)

######################## use only the testing input variables for model 5 ########################
t7_results_5 <- compute(t7_model_5, test_t7_norm)  
# obtain predicted consumption values
t7_prediction_5 <- t7_results_5$net.result    

#de-normalize the normalized NN’s output
t7_predict_denorm_5 <- unnormalize(t7_prediction_5, train_t7_min, train_t7_max)

#RMSE function
t7_rmse_5 <- rmse(test_t7$t, t7_predict_denorm_5)
t7_rmse_5
# Calculate MAE
t7_mae_5 <- mae(test_t7$t, t7_predict_denorm_5)
t7_mae_5
# Calculate MAPE
t7_mape_5 <- mape(test_t7$t, t7_predict_denorm_5)
t7_mape_5
# Calculate sMAPE
t7_smape_5 <- smape(test_t7$t, t7_predict_denorm_5)
t7_smape_5

#clear structure od actual and predicted output 
t7_cleanoutput_5 <- cbind(test_t7$t, t7_predict_denorm_5)
print("Model 5")
colnames(t7_cleanoutput_5) <- c("Acual Consumption", "Predicted Consumption")
head(t7_cleanoutput_5)

#part f
#creating models to predict consumption outputs using I/O matrices of normalize training data sets
t1_model_6 <- neuralnet(t~t_1, hidden = c(3,5), 
                      data = train_t1_norm, linear.output = TRUE)
t2_model_7 <- neuralnet(t~t_1 + t_2, hidden = c(3,5), 
                      data = train_t2_norm, linear.output = TRUE)
t3_model_8 <- neuralnet(t~t_1+ t_2 + t_3, hidden = c(3,5), 
                      data = train_t3_norm, linear.output = TRUE)
t4_model_9 <- neuralnet(t~t_1+ t_2 + t_3 + t_4, hidden = c(3,5), 
                      data = train_t4_norm, linear.output = TRUE)
t7_model_10 <- neuralnet(t~t_1+ t_2 + t_3 + t_4 + t_5 + t_6 +t_7, hidden = c(3,5), 
                      data = train_t7_norm, linear.output = TRUE)


t1_model_11 <- neuralnet(t~t_1, hidden = c(4,2), 
                        data = train_t1_norm, linear.output = FALSE)
t2_model_12 <- neuralnet(t~t_1 + t_2, hidden =c(4,2), 
                        data = train_t2_norm, linear.output = FALSE)
t3_model_13 <- neuralnet(t~t_1+ t_2 + t_3, hidden = c(4,2), 
                        data = train_t3_norm, linear.output = FALSE)
t4_model_14 <- neuralnet(t~t_1+ t_2 + t_3 + t_4, hidden = c(4,2), 
                        data = train_t4_norm, linear.output = FALSE)
t7_model_15 <- neuralnet(t~t_1+ t_2 + t_3 + t_4 + t_5 + t_6 +t_7, hidden = c(4,2), 
                         data = train_t7_norm, linear.output = FALSE)

######################## use only the testing input variables for model 6 ########################
t1_results_6 <- compute(t1_model_6, test_t1_norm)  
# obtain predicted consumption values
t1_prediction_6 <- t1_results_6$net.result    

#de-normalize the normalized NN’s output
t1_predict_denorm_6 <- unnormalize(t1_prediction_6, train_t1_min, train_t1_max)

#RMSE function
t1_rmse_6 <- rmse(test_t1$t, t1_predict_denorm_6)
t1_rmse_6
# Calculate MAE
t1_mae_6 <- mae(test_t1$t, t1_predict_denorm_6)
t1_mae_6
# Calculate MAPE
t1_mape_6 <- mape(test_t1$t, t1_predict_denorm_6)
t1_mape_6
# Calculate sMAPE
t1_smape_6 <- smape(test_t1$t,t1_predict_denorm_6)
t1_smape_6

#clear structure od actual and predicted output 
t1_cleanoutput_6 <- cbind(test_t1$t, t1_predict_denorm_6)
print("Model 6")
colnames(t1_cleanoutput_6) <- c("Acual Consumption ", "Predicted Consumption")
head(t1_cleanoutput_6)

######################## use only the testing input variables for model 7 ########################
t2_results_7 <- compute(t2_model_7, test_t2_norm)  
# obtain predicted consumption values
t2_prediction_7 <- t2_results_7$net.result    

#de-normalize the normalized NN’s output
t2_predict_denorm_7 <- unnormalize(t2_prediction_7, train_t2_min, train_t2_max)

#RMSE function
t2_rmse_7 <- rmse(test_t2$t, t2_predict_denorm_7)
t2_rmse_7
# Calculate MAE
t2_mae_7 <- mae(test_t2$t, t2_predict_denorm_7)
t2_mae_7
# Calculate MAPE
t2_mape_7 <- mape(test_t2$t, t2_predict_denorm_7)
t2_mape_7
# Calculate sMAPE
t2_smape_7 <- smape(test_t2$t, t2_predict_denorm_7)
t2_smape_7

#clear structure od actual and predicted output 
t2_cleanoutput_7 <- cbind(test_t2$t, t2_predict_denorm_7)
print("Model 7")
colnames(t2_cleanoutput_7) <- c("Acual Consumption", "Predicted Consumption")
head(t2_cleanoutput_7)

######################## use only the testing input variables for model 8 ########################
t3_results_8 <- compute(t3_model_8, test_t3_norm)  
# obtain predicted consumption values
t3_prediction_8 <- t3_results_8$net.result    

#de-normalize the normalized NN’s output
t3_predict_denorm_8 <- unnormalize(t3_prediction_8, train_t3_min, train_t3_max)

#RMSE function
t3_rmse_8 <- rmse(test_t3$t, t3_predict_denorm_8)
t3_rmse_8
# Calculate MAE
t3_mae_8 <- mae(test_t3$t, t3_predict_denorm_8)
t3_mae_8
# Calculate MAPE
t3_mape_8 <- mape(test_t3$t, t3_predict_denorm_8)
t3_mape_8
# Calculate sMAPE
t3_smape_8 <- smape(test_t3$t, t3_predict_denorm_8)
t3_smape_8

#clear structure od actual and predicted output 
t3_cleanoutput_8 <- cbind(test_t3$t, t3_predict_denorm_8)
print("Model 3")
colnames(t3_cleanoutput_8) <- c("Acual Consumption", "Predicted Consumption")
head(t3_cleanoutput_8)

######################## use only the testing input variables for model 9 ########################
t4_results_9 <- compute(t4_model_9, test_t4_norm)  
# obtain predicted consumption values
t4_prediction_9 <- t4_results_9$net.result    

#de-normalize the normalized NN’s output
t4_predict_denorm_9 <- unnormalize(t4_prediction_9, train_t4_min, train_t4_max)

#RMSE function
t4_rmse_9 <- rmse(test_t4$t, t4_predict_denorm_9)
t4_rmse_9
# Calculate MAE
t4_mae_9 <- mae(test_t4$t, t4_predict_denorm_9)
t4_mae_9
# Calculate MAPE
t4_mape_9 <- mape(test_t4$t, t4_predict_denorm_9)
t4_mape_9
# Calculate sMAPE
t4_smape_9 <- smape(test_t4$t, t4_predict_denorm_9)
t4_smape_9

#clear structure od actual and predicted output 
t4_cleanoutput_9 <- cbind(test_t4$t, t4_predict_denorm_9)
print("Model 9")
colnames(t4_cleanoutput_9) <- c("Acual Consumption", "Predicted Consumption")
head(t4_cleanoutput_9)

######################## use only the testing input variables for model 10 ########################
t7_results_10 <- compute(t7_model_10, test_t7_norm)  
# obtain predicted consumption values
t7_prediction_10 <- t7_results_10$net.result    

#de-normalize the normalized NN’s output
t7_predict_denorm_10 <- unnormalize(t7_prediction_10, train_t7_min, train_t7_max)

#RMSE function
t7_rmse_10 <- rmse(test_t7$t, t7_predict_denorm_10)
t7_rmse_10
# Calculate MAE
t7_mae_10 <- mae(test_t7$t, t7_predict_denorm_10)
t7_mae_10
# Calculate MAPE
t7_mape_10 <- mape(test_t7$t, t7_predict_denorm_10)
t7_mape_10
# Calculate sMAPE
t7_smape_10 <- smape(test_t7$t, t7_predict_denorm_10)
t7_smape_10

#clear structure od actual and predicted output 
t7_cleanoutput_10 <- cbind(test_t7$t, t7_predict_denorm_10)
print("Model 10")
colnames(t7_cleanoutput_10) <- c("Acual Consumption", "Predicted Consumption")
head(t7_cleanoutput_10)

######################## use only the testing input variables for model 11 ########################
t1_results_11 <- compute(t1_model_11, test_t1_norm)  
# obtain predicted consumption values
t1_prediction_11 <- t1_results_11$net.result    

#de-normalize the normalized NN’s output
t1_predict_denorm_11 <- unnormalize(t1_prediction_11, train_t1_min, train_t1_max)

#RMSE function
t1_rmse_11 <- rmse(test_t1$t, t1_predict_denorm_11)
t1_rmse_11
# Calculate MAE
t1_mae_11 <- mae(test_t1$t, t1_predict_denorm_11)
t1_mae_11
# Calculate MAPE
t1_mape_11 <- mape(test_t1$t, t1_predict_denorm_11)
t1_mape_11
# Calculate sMAPE
t1_smape_11 <- smape(test_t1$t, t1_predict_denorm_11)
t1_smape_11

#clear structure od actual and predicted output 
t1_cleanoutput_11 <- cbind(test_t1$t, t1_predict_denorm_11)
print("Model 11")
colnames(t1_cleanoutput_11) <- c("Acual Consumption ", "Predicted Consumption")
head(t1_cleanoutput_11)

######################## use only the testing input variables for model 12 ########################
t2_results_12 <- compute(t2_model_12, test_t2_norm)  
# obtain predicted consumption values
t2_prediction_12 <- t2_results_12$net.result    

#de-normalize the normalized NN’s output
t2_predict_denorm_12 <- unnormalize(t2_prediction_12, train_t2_min, train_t2_max)

#RMSE function
t2_rmse_12 <- rmse(test_t2$t, t2_predict_denorm_12)
t2_rmse_12
# Calculate MAE
t2_mae_12 <- mae(test_t2$t, t2_predict_denorm_12)
t2_mae_12
# Calculate MAPE
t2_mape_12 <- mape(test_t2$t, t2_predict_denorm_12)
t2_mape_12
# Calculate sMAPE
t2_smape_12 <- smape(test_t2$t, t2_predict_denorm_12)
t2_smape_12

#clear structure od actual and predicted output 
t2_cleanoutput_12 <- cbind(test_t2$t, t2_predict_denorm_12)
print("Model 2")
colnames(t2_cleanoutput_12) <- c("Acual Consumption", "Predicted Consumption")
head(t2_cleanoutput_12)

######################## use only the testing input variables for model 13 ########################
t3_results_13 <- compute(t3_model_13, test_t3_norm)  
# obtain predicted consumption values
t3_prediction_13 <- t3_results_13$net.result    

#de-normalize the normalized NN’s output
t3_predict_denorm_13 <- unnormalize(t3_prediction_13, train_t3_min, train_t3_max)

#RMSE function
t3_rmse_13 <- rmse(test_t3$t, t3_predict_denorm_13)
t3_rmse_13
# Calculate MAE
t3_mae_13 <- mae(test_t3$t, t3_predict_denorm_13)
t3_mae_13
# Calculate MAPE
t3_mape_13 <- mape(test_t3$t, t3_predict_denorm_13)
t3_mape_13
# Calculate sMAPE
t3_smape_13 <- smape(test_t3$t, t3_predict_denorm_13)
t3_smape_13

#clear structure od actual and predicted output 
t3_cleanoutput_13 <- cbind(test_t3$t, t3_predict_denorm_13)
print("Model 13")
colnames(t3_cleanoutput_13) <- c("Acual Consumption", "Predicted Consumption")
head(t3_cleanoutput_13)

######################## use only the testing input variables for model 14 ########################
t4_results_14 <- compute(t4_model_14, test_t4_norm)  
# obtain predicted consumption values
t4_prediction_14 <- t4_results_14$net.result    

#de-normalize the normalized NN’s output
t4_predict_denorm_14 <- unnormalize(t4_prediction_14, train_t4_min, train_t4_max)

#RMSE function
t4_rmse_14 <- rmse(test_t4$t, t4_predict_denorm_14)
t4_rmse_14
# Calculate MAE
t4_mae_14 <- mae(test_t4$t, t4_predict_denorm_14)
t4_mae_14
# Calculate MAPE
t4_mape_14 <- mape(test_t4$t, t4_predict_denorm_14)
t4_mape_14
# Calculate sMAPE
t4_smape_14 <- smape(test_t4$t, t4_predict_denorm_14)
t4_smape_14

#clear structure od actual and predicted output 
t4_cleanoutput_14 <- cbind(test_t4$t, t4_predict_denorm_14)
print("Model 14")
colnames(t4_cleanoutput_14) <- c("Acual Consumption", "Predicted Consumption")
head(t4_cleanoutput_14)

######################## use only the testing input variables for model 15 ########################
t7_results_15 <- compute(t7_model_15, test_t7_norm)  
# obtain predicted consumption values
t7_prediction_15 <- t7_results_15$net.result    

#de-normalize the normalized NN’s output
t7_predict_denorm_15 <- unnormalize(t7_prediction_15, train_t7_min, train_t7_max)

#RMSE function
t7_rmse_15 <- rmse(test_t7$t, t7_predict_denorm_15)
t7_rmse_15
# Calculate MAE
t7_mae_15 <- mae(test_t4$t, t4_predict_denorm_14)
t7_mae_15
# Calculate MAPE
t7_mape_15 <- mape(test_t4$t, t4_predict_denorm_14)
t7_mape_15
# Calculate sMAPE
t7_smape_15 <- smape(test_t4$t, t4_predict_denorm_14)
t7_smape_15

#clear structure od actual and predicted output 
t7_cleanoutput_15 <- cbind(test_t7$t, t7_predict_denorm_15)
print("Model 15")
colnames(t7_cleanoutput_15) <- c("Acual Consumption", "Predicted Consumption")
head(t7_cleanoutput_15)


#real Vs predicted NN plot
par(mfrow=c(1,1))
plot(test_t1$t, t1_predict_denorm_1 ,col="red",main='Real vs predicted NN',pch=18,cex=0.7)
abline(a=0, b=1, h=90, v=90)
