
#### Libraries ####
library(tidyverse)
library(caret)
library(recipes)

## Load the clean data 
telco_clean <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/telco.clean.rds")

# Remove the customer id
telco_clean <- telco_clean %>% select(-customer_id)
## Split the data 

# Create a partition
paritition <- createDataPartition(telco_clean$churn,p = 0.7,list = FALSE)

# Create a training set
telco_train <- telco_clean[paritition,]

# Create a testing set 
telco_test <- telco_clean[-paritition,]

## Make a recipe blueprint
blueprint <- recipe(churn ~ .,data = telco_train) %>%
  
  # Fitler out near zero variance predictors
  step_nzv(all_predictors()) %>%
  
  # Normalize all numerical predictors with Yeo Johnson transfomration
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Standardize all numerical predictor
  step_center(all_numeric_predictors())%>%
  step_scale(all_numeric_predictors()) %>%
  
  # One hot encode all nominal predictors
  step_dummy(all_nominal_predictors(),one_hot = TRUE)
  
## Prep the blueprint on the training data only to prevent data leaks
preparation <- prep(blueprint,training = telco_train)

## Bake the data (Apply the changes to training and testing set)

# Create train set ready for Ml
train_set <- bake(preparation,telco_train)

# Create testing set ready for ML
test_set <- bake(preparation,telco_test)

## Export the final sets 
saveRDS(test_set,"telco_test_set.rds")
saveRDS(train_set,"telco_train_set.rds")




