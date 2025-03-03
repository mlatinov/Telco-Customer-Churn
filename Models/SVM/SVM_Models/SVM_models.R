
#### Libraries ####
library(tidyverse)
library(caret)
library(kernlab)

#### SVM  ####

# Load the training set 
train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")
                     
## SVM Radial ##

sigest(churn ~ .,data = train_set,frac =1) # sigma of 0.0147.. and C 0.25

# Define a train control for cross validations
train_cr <- trainControl(method = "cv",
                         number = 10,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         sampling = "smote" # Synthetic Minority Over-sampling Technique
                         )

## Tune a SVM Radial Kernel
svm_radial <- train(
   churn ~.,
   data = train_set,
   method ="svmRadial",
   trControl = train_cr,
   tuneLength = 10,
   metric = "ROC"
   )
# Save the best parameters
svm_radial_params <- svm_radial$bestTune

## Tune SVM Polynomial kernel

# Define a grid search for 
svm_poly <- train(
  churn ~.,
  data = train_set,
  method = "svmPoly",
  trControl = train_cr,
  tuneLenght = 10,
  metric = "ROC"
  )
# Save the best parameters
svm_poly_params <-svm_poly$bestTune

## Tune SVM Linear karnel
svm_linear <- train(
  churn~.,
  data = train_set,
  method = "svmLinear",
  tuneLenght = 10,
  trControl = train_cr,
  metric = "ROC"
  )

## Save the models for Evaluation
saveRDS(svm_radial,file = "svm_radial_model.rds")
saveRDS(svm_poly,file = "svm_poly_model.rds" )
saveRDS(svm_linear,file = "svm_linear_model.rds")

