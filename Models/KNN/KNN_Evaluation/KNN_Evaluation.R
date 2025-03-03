
#### Libraries ####
library(tidyverse)
library(caret)
library(ROCR)
library(viridis)

#### KNN Evaluation ####

# Load the model 
knn_model <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/KNN/KNN_Models/knn_model.rds")

# Load the testing set 
test_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_test_set.rds")

## Predict on the testing set
knn_predictions <- predict(knn_model,test_set,type = "prob")

# Extract the prob = Yes
knn_prob_pos <- knn_predictions[,"Yes"]

# Create a prediction object
knn_pred <- prediction(knn_prob_pos,test_set$churn)

# Compute the performance
knn_perf <- performance(knn_pred,measure = "tpr",x.measure = "fpr")

## Plot the ROC
plot(knn_perf, main = "KNN ROC Curve")
mtext("AUC = 0.85", side = 3, line = 0.5, cex = 0.9, font = 2)

## Compute the AUC
knn_auc <- performance(knn_pred,measure  = "auc")
knn_auc@y.values[[1]] ## 0.8550269

## Cofusion matrix

# Predict the classes
knn_class_pred <- predict(knn_model,test_set)

# Create a confusion matrix
knn_conf <- confusionMatrix(knn_class_pred,test_set$churn)

## Heat map Conf Matrix

# Convert to conf matrix into data frame
knn_conf_df <- as.data.frame(as.table(knn_conf))

# Plot the Cofusion matrix
ggplot(data = knn_conf_df,aes(x = Prediction,y = Reference,fill = Freq))+
  geom_tile(col = "black")+
  theme_minimal()+
  scale_fill_viridis(option = "plasma")+
  geom_text(aes(label = Freq), color = "black", size = 5)+
  labs(title = "KNN Cofusion matrix Heatmap")


















