
#### Libraries ####
library(tidyverse)
library(caret)
library(ROCR)

## Load the models 
svm_radial_model <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/SVM/SVM_Models/svm_radial_model.rds")
svm_poly_model <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/SVM/SVM_Models/svm_poly_model.rds")
svm_linear_model <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/SVM/SVM_Models/svm_linear_model.rds")

## Load the testing set 
test_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_test_set.rds")

### SVM Evaluation ##

## Radial Model

## Predictions on the test data 
svm_rad_pred <- predict(svm_radial_model,test_set,type = "prob")

## Performance radial ROC

# Extract the prob = YES
svm_rad_pred_pos <- svm_rad_pred[,"Yes"]

# Create a prediction object
pred_radial <- prediction(svm_rad_pred_pos, test_set$churn)

# Plot the ROC curve
radial_perf <- performance(pred_radial,measure = "tpr",x.measure = "fpr")
plot(radial_perf,legend(title = ""))

# Calcualte AUC
auc_radial <-performance(pred,measure = "auc")
auc_radial<-auc_radial@y.values[[1]] ## 0.8212675

## Poly SVM ##

# Make predictions 
pred_svm_poly <- predict(svm_poly_model,test_set,type = "prob")

# Extract the prob = YES
svm_poly_pred_pos <- pred_svm_poly[,"Yes"]

# Make a prediction object 
pred_poly <- prediction(svm_poly_pred_pos,test_set$churn)

# Plot the ROC
poly_perf <- performance(pred_poly,measure = "tpr",x.measure = "fpr")
plot(poly_perf)

# Calculate AUC
auc_poly <- performance(pred_poly,measure = "auc")
auc_poly<- auc_poly@y.values[[1]] ## 0.8515291

## Linear SVM ##

# Make predictions on the testing set
svm_pred_linear <- predict(svm_linear_model,test_set,type = "prob")

# Extract the class prob = YES
svm_pred_linear_pos <- svm_pred_linear[,"Yes"]

# Make a pred object 
pred_linear <- prediction(svm_pred_linear_pos,test_set$churn)

# Plot the ROC 
perf_linear <- performance(pred_linear,measure = "tpr","fpr")
plot(perf_linear)

# Calculate AUC
auc_linear <- performance(pred_linear,measure = "auc")
auc_linear<-auc_linear@y.values[[1]] ## 0.8482523


## Plot the three ROC together
plot(perf_linear,col = "blue",lty = 2)
plot(poly_perf,col = "red",lty = 3,add = TRUE)
plot(radial_perf,col = "green",lty =1,add = TRUE)
legend("bottomright",
       legend = c(paste("Linear SVM AUC = 0.84"),
                 paste("Polynomial SVM AUC = 0.85"),
                 paste("Radial SVM AUC = 0.82")), 
       col = c("blue","red","green"),
       lty = c(2,3,1),
       title = "SVM Models ROC",
       box.lwd = 0.9,)

## Confusion matrix

# Radial
pred_svm_rad_2 <- predict(svm_radial_model,test_set)
conf_radial <- confusionMatrix(pred_svm_rad_2,test_set$churn)

# Polynomial
pred_svm_poly_2 <- predict(svm_poly_model,test_set)
conf_poly <- confusionMatrix(pred_svm_poly_2,test_set$churn)

# Linear 
pred_svm_lin_2 <- predict(svm_linear_model,test_set)
conf_lin <- confusionMatrix(pred_svm_lin_2,test_set$churn)
 
# Convet the matrix into dataframe
conf_poly_df <- as.data.frame(as.table(conf_poly))


# Plot 
ggplot(conf_poly_df, aes(Prediction, Reference, fill = Freq)) +
  geom_tile(color = "black") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", x = "Predicted", y = "Actual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







