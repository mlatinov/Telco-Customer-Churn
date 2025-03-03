
## Libraries ##
library(tidyverse)
library(caret)
library(ROCR)
library(viridis)
## Log Models Evaluation Elastic Net

# Load the testing set 
test_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_test_set.rds")

# Load the model
elastic_net <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/Log_regression/Log_models/elastic_net.rds")

## Make predictions 
prediction_net <- predict(elastic_net,test_set,type = "prob")

# Extract the prob = YES 
pred_net_pos <- prediction_net[,"Yes"]

# Make a prediction object
pred_net <- prediction(pred_net_pos,test_set$churn)

# Compute the performance
perf_net <- performance(pred_net,measure = "tpr",x.measure = "fpr")

# Plot the ROC curve
plot(perf_net, col = "red", lty = 2,main = "ROC Curve for Elastic Net Model")
legend("bottomright",
       legend = c("Elastic Net = 0.85"), # Added correct legend text
       col = "red",                      # Corrected col placement
       lty = 2)    

# Compute the AUC
auc_net <- performance(pred_net,measure = "auc")
auc_net_best<-auc_net@y.values[[1]]

## Confusion matrix

# Compute the predictions type class
pred_net_2 <- predict(elastic_net,test_set)

# Make a confusion matrix
conf_net <-confusionMatrix(pred_net_2,test_set$churn)

# Trasform the matrix into data frame
conf_net_df <- as.data.frame(as.table(conf_net))

# Plot the Heat map confusion matrix
ggplot(data = conf_net_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis") +  # Corrected scale_fill_viridis
  theme_classic() +
  labs(
    title = "Heatmap of Elastic Net Confusion Matrix",
    x = "Prediction", 
    y = "Actual")











