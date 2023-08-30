require("readxl")
require("stringr")
require("caret")
library(ROSE)

load_data <- function(base_data_path){

  
# Read the raw data
read_data <- function(base_data_path){

  Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)

  target <- Inputdata$playoffs
  Inputdata <- Inputdata[, -ncol(Inputdata)]

  Inputdata <- apply(Inputdata, 2, as.numeric)
  Inputdata <- as.data.frame(Inputdata)

  Inputdata = cbind("target"=as.factor(target), Inputdata)
  
  set.seed(987)
  Inputdata<-ROSE(target~.,data=Inputdata)$data

  print(table(Inputdata$target))
  
  Inputdata <- as.data.frame(Inputdata)
  
  target <- Inputdata$target
  target <- make.names(target)
  Inputdata$target <- target

  return(Inputdata)
}


# read_data <- function(base_data_path){
#   
#   base_data_path <- file.path(base_path, "data", input_file_name)
#   
#   Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)
#   
#   Inputdata <- apply(Inputdata, 2, as.numeric)
#   Inputdata <- as.data.frame(Inputdata)
#   
#   target <- Inputdata$target
#   target <- make.names(target)
#   Inputdata$target <- target
#   
#   return(Inputdata)
# }


Inputdata <- read_data(base_data_path)


# Create training and test data
set.seed(555)
train_indices <- createDataPartition(y = as.factor(Inputdata$target), p = 0.80, list = FALSE)
training_data <- as.data.frame(Inputdata[train_indices, ])
test_data <- as.data.frame(Inputdata[-train_indices, ])


print("Data read-in successfully")
print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))
out = list("train_data" = training_data, "test_data" = test_data)


return(out)

}


