
#### Libraries ####
library(tidyverse)
library(h2o)
library(pROC)

#### Evaluation ####

# Start h2o 
h2o.init()

# Load the models
h2o_model <- h2o.loadModel("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/Random_Forest/Random_Forest_models/h2o_rf_tune")
ranger_model <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/Random_Forest/Random_Forest_models/ranger_RF_tune")

# Load the testing set
test_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_test_set.rds")
test_set_h2o <- as.h2o(test_set)


## Make predictions with h2o
h2o_pred <- h2o.predict(h2o_model,test_set_h2o)

## Extract the probs for the posivite class "Yes"
prob_pos_class <- h2o_pred[,2]

## Calculate AUC
perf_h2o <- h2o.performance(h2o_model,newdata = test_set_h2o )
auc_h2o <-perf_h2o@metrics$AUC # 0.8555

# Plot the AUC
plot(perf_h2o,type = "roc")

## Confusion Matrix
cm_h2o <- h2o.confusionMatrix(perf_h2o)

## Evaluation ranger

# Make predictions
ranger_pred <- predict(ranger_model,test_set)

# Extract the probs for the positive class "Yes"
ranger_pos_class <- ranger_pred$predictions[,2]

# Calculate AUC
auc_ranger <- roc(test_set$churn,ranger_pos_class) ## 0.8534

