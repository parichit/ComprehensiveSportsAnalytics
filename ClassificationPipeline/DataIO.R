require("readxl")
require("stringr")
require("caret")

load_data <- function(base_data_path, upsample){

  
# Read the raw data
read_data <- function(base_data_pathm, upsample){
  
  Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)
  
  target <- Inputdata$playoffs
  Inputdata <- Inputdata[, -ncol(Inputdata)]
  
  Inputdata <- apply(Inputdata, 2, as.numeric)
  Inputdata <- as.data.frame(Inputdata)
  
  Inputdata = cbind("target"=as.factor(target), Inputdata)
  
  return(Inputdata)
}



Inputdata <- read_data(base_data_path, upsample)


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


