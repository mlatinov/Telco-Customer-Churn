
#### Libraries ####
library(tidyverse)
library(pROC)
library(h2o)

#### GBMs Evaluation ####

# Load the testing set 
test_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_test_set.rds")

# Convert the factors into num 0 and 1 
test_set$churn <- as.numeric(test_set$churn)-1

## Load the models 
weighted_gbm <- readRDS("C:\\Users\\Huawei\\OneDrive\\Telco_Churn_project\\Telco-Customer-Churn\\Models\\GBMs\\GBMs_Models\\weighted_gbm")
gbm_model_tuned <- readRDS("C:\\Users\\Huawei\\OneDrive\\Telco_Churn_project\\Telco-Customer-Churn\\Models\\GBMs\\GBMs_Models\\gbm_model_tuned")

# Start h2o
h2o.init()

# Load the h2o model
gbm_h20 <- h2o.loadModel(path = "C:\\Users\\Huawei\\OneDrive\\Telco_Churn_project\\Telco-Customer-Churn\\Models\\GBMs\\GBMs_Models\\grid_gbm_model_27")

## Weighted Model##

## Predict on the testing data 
weighted_predictions <- predict(weighted_gbm,test_set,type = "response")

# Compute the auc and roc
weighted_roc <- roc(test_set$churn,weighted_predictions)
weighted_auc <- auc(weighted_roc)

# Extract the tpr and fpr
tpr <- weighted_roc$sensitivities
fpr <- 1 - weighted_roc$specificities

# Combine into a data frame for plotting
roc_data <- data.frame(FPR = fpr, TPR = tpr)

# Plot
plot(
  roc_data$FPR, roc_data$TPR,
  type = "l", col = "blue", lwd = 2,
  xlab = "False Positive Rate (FPR)",
  ylab = "True Positive Rate (TPR)",
  main = "ROC Curve Weighted GBM
  AUC: 0.8563",
  )

## GBM Model Tuned

# Predict on the testing data 
tune_predictions <- predict(gbm_model_tuned,test_set,type = "response")

# Compute auc and Roc
tune_roc <- roc(test_set$churn,tune_predictions)
auc_tune <- auc(tune_roc)

# Extract the tpr and fpr
tune_tpr <- tune_roc$sensitivities
tune_fpr <- 1 - tune_roc$specificities

## Combine into a data frame for plotting
roc_data_tune <- data.frame(FPR = tune_fpr, TPR = tune_tpr)

# Plot
plot(
  roc_data_tune$FPR,roc_data_tune$TPR,
  type = "l", col = "purple", lwd = 2,
  xlab = "False Positive Rate (FPR)",
  ylab = "True Positive Rate (TPR)",
  main = "ROC Curve Tuned GBM
  AUC: 0.8577",
  ) 

## H2o GBM ##

# Convert the testing set into h2o object 
test_set_h2o <- as.h2o(test_set)

# Make the predictions 
h2o_predictions <- h2o.predict(gbm_h20,test_set_h2o)

# Extract the probs of class YES or 1 
h2o_pred_pos <- as.data.frame(h2o_predictions)$p1

# Get a performance object
h2o_performance <- h2o.performance(gbm_h20,test_set_h2o)

# Extract ROC curve data
roc_data <- as.data.frame(h2o_performance@metrics$thresholds_and_metric_scores)

# Plot ROC curve using ggplot2
ggplot(roc_data, aes(x = fpr, y = tpr)) + 
  geom_line(color = "blue") + 
  geom_abline(linetype = "dashed", color = "red") +  # Diagonal reference line
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()
