
#### Libraries ####
library(tidyverse)
library(caret)

#### KNN ####

# Load the traning data 
train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")

## Define a train control
train_cr <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  )

## Create a hyper grid to search for k
hyper_grid <- expand.grid(
  k = seq(1,101,2)
  )

## Tune the model 
knn_model <- train(
  churn ~.,
  data = train_set,
  method ="knn",
  trControl = train_cr,
  tuneGrid = hyper_grid,
  metric = "ROC"
  )

## Save the best params
k_best <- knn_model$bestTune

# Plot the Optimal K
ggplot(knn_model)+
  theme_minimal()+
  geom_vline(xintercept = 89,colour = "red",linetype = 2)+
  annotate("text",x = 80,y = 0.76,label ="Optimal K")+
  labs(
    title = "KNN Optimal K",
    subtitle = "K = 89"
  )

## Save the model 
saveRDS(knn_model,"knn_model.rds")




