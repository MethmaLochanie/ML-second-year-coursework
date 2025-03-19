library(readxl)
library(keras)

electricity <- read_xlsx("uow_consumption.xlsx")
electricity <- electricity[,-1]

#subtask 1
#part b
hourly_consumption_20_Hour <- electricity[c("0.83333333333333337")]

# Extract the first 366 samples as training data, and the remaining samples as testing data
# x_train: input training data with 20th hour attribute values and t-7 load values
x_train <- unlist(hourly_consumption_20_Hour[1:366, "0.83333333333333337"])
# y_train: output training data with electricity consumption values for t+1 to t+4
y_train <- unlist(hourly_consumption_20_Hour[366:nrow(hourly_consumption_20_Hour), "0.83333333333333337"])

# create input matrix
input_matrix <- matrix(nrow = length(x_train) - 11, ncol = 10)
for (i in 8:(length(x_train) - 4)) {
  
  input_matrix[i - 7, ] <- c(x_train[i - 7], x_train[i - 4:i - 1])
}

# create output matrix
output_matrix <- matrix(nrow = length(y_train) - 11, ncol = 4)
for (i in 8:(length(y_train) - 4)) {
  output_matrix[i - 7, ] <- y_train[i:i + 3]
}



# define MLP model
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(10)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 4)

# compile model
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae")
)

# fit model to training data
history <- model %>% fit(
  x = input_matrix,
  y = output_matrix,
  batch_size = 32,
  epochs = 50,
  validation_split = 0.2
)

# evaluate model on test data
test_input_matrix <- matrix(nrow = length(x_test) - 11, ncol = 10)
for (i in 8:(length(x_test) - 4)) {
  test_input_matrix[i - 7, ] <- c(x_test[i - 7], x_test[i - 4:i - 1])
}
test_output_matrix <- matrix(nrow = length(y_test) - 11, ncol = 4)
for (i in 8:(length(y_test) - 4)) {
  test_output_matrix[i - 7, ] <- y_test[i:i + 3]
}
test_metrics <- model %>% evaluate(
  x = test_input_matrix,
  y = test_output_matrix
)
