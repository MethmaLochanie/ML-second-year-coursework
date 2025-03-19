library(readxl)
library(keras)
library(dplyr)

electricity_consumption <- read_xlsx("uow_consumption.xlsx")
training_dataset <- electricity_consumption[1:380,]
testing_dataset <- electricity_consumption[380:nrow(electricity_consumption),]

#subtask 1
#part b
hourly_consumption_20_Hour <- training_dataset[c("date", "0.83333333333333337")]

# Function to create time-delayed input matrix
createInputMatrix <- function(delay){
  input <- matrix(0, nrow = nrow(hourly_consumption_20_Hour) - delay, ncol = delay)
  for (i in 1:delay){
    input[i] <- hourly_consumption_20_Hour[(delay + 1 - i):(nrow(hourly_consumption_20_Hour) - i), 2]
  }
  output <- hourly_consumption_20_Hour[(delay + 1):nrow(hourly_consumption_20_Hour), 2]
   output
  #if(nrow(input) != length(output)){
   # stop("Input and output have different number of rows.")
  #}
  concatenated <- c(input, output)
  return(matrix(concatenated, nrow = nrow(hourly_consumption_20_Hour) - delay, byrow = FALSE))
  #return(list(input = input, output=output))
}

# Create time-delayed input matrices for different delays
delay <- c(1, 2, 3, 4)
delay
input_matrices <- lapply(delay, createInputMatrix)
input_matrices


