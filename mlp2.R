library(readxl)
library(keras)
library(dplyr)

electricity_consumption <- read_xlsx("uow_consumption.xlsx")
training_dataset <- electricity_consumption[1:381]
testing_dataset <- electricity_consumption[381:nrow(electricity_consumption)]

#subtask 1
#part b
hourly_consumption_20_Hour <- training_dataset[c("date", "0.83333333333333337")]

load_t1_data <- hourly_consumption_20_Hour[c(456:470),]
load_t2_data <- hourly_consumption_20_Hour[c(425:455),]
load_t3_data <- hourly_consumption_20_Hour[c(397:424),]
load_t4_data <- hourly_consumption_20_Hour[c(366:396),]
load_t5_data <- hourly_consumption_20_Hour[c(335:365),]
load_t6_data <- hourly_consumption_20_Hour[c(305:334),]
load_t7_data <- hourly_consumption_20_Hour[c(274:304),]
load_t8_data <- hourly_consumption_20_Hour[c(244:275),]

#traindata <- hourly_consumption_20_Hour[244:470]

#inout_output_matrix <- matrix(c())

################ Training Model ######################
input_output_matrix <- matrix(nrow = 5, ncol = 2)
traindata_input <- c() #matrix( ,nrow = 0, ncol = 4) #matrix to extract input values
traindata_output <- c() #vector to store output value
for (i in 373:length(hourly_consumption_20_Hour)) {
  end_value <- i + 3 #find last record number for each input set
  
  #break loop if output value is out of bounds
  if (i +1 > length(hourly_consumption_20_Hour)) {
    break
  }
  
  #collect new records set to the existing records
  new_input <- hourly_consumption_20_Hour[i] #store 1st 4 records of iteration as new input set
  new_output <- hourly_consumption_20_Hour[i+1] #store 5th record as output value
  
  traindata_input <- append(new_input) #add the new input vector to inputs 
  traindata_output <- append(new_output) #add new output value to output vector
}
#create a data frame with all inputs and outputs
traindata_input_output_df <- cbind(as.data.frame(traindata_input), traindata_output)