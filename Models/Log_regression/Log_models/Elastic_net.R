
##### Libraries ####
library(tidyverse)
library(caret)
library(glmnet)

## Load the testing set 
train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")

### Logistic regression ###
set.seed(123)

# Define a train control
tr_control <- trainControl(
  method =  "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "smote"
)

## Train a Regular Logistic Regression
logistic_reg <- train(
  churn ~.,
  data = train_set,
  method = "glm",
  family = "binomial",
  trControl = tr_control,
  metric = "ROC"
)

## Regularized logistic regression Elastic Net ##

## Train a Elactic net model

# Set the response
Y <- train_set$churn

# Set the predictors
X <- model.matrix(churn ~ ., train_set)[,-1]

# Define a train control for the Elastic net without sampling
tr_control_net <- trainControl(
  method =  "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  )
  
# Run the elastic net 
elastic_net <- train(
  x = X,
  y = Y,
  method = "glmnet",
  family = "binomial",
  trControl = tr_control,
  metric = "ROC",
  tuneLength = 10
)

# Save the best params
best_params<-elastic_net$bestTune

## Save the Elastic net model
saveRDS(elastic_net,"elastic_net.rds")








