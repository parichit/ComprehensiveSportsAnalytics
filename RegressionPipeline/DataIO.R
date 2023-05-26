require("caret")
require("readxl")
require("stringr")

load_data <- function(base_data_path){

  
read_data <- function(base_data_path){
  
  Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)
  
  Inputdata <- apply(Inputdata, 2, as.numeric)
  Inputdata <- as.data.frame(Inputdata)
  
  target = Inputdata$Votes
  Inputdata <- Inputdata[, -ncol(Inputdata)]
  Inputdata <- cbind(target=target, Inputdata)
  
  return(Inputdata)
}



Inputdata <- read_data(base_data_path)

# Create training and test data
set.seed(8745)
train_indices <- createDataPartition(y = as.factor(Inputdata$target), p = 0.80, list = FALSE)
training_data <- Inputdata[train_indices, ]
test_data <- Inputdata[-train_indices, ]

print("Data read-in successfully")
print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))

out = list("train_data" = training_data, "test_data" = test_data)

return(out)

}