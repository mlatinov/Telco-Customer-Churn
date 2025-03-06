

#### Libraries ####
library(tidyverse)
library(h2o)
library(gbm)

##### GBMs ####

# Load the training set 
train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")

train_set$churn <- as.numeric(train_set$churn)-1

## Run a basic gradient boosting ##
gbm_b <- gbm(
  churn ~ .,
  distribution = "bernoulli", # Logistic regression for 0-1 outcomes
  data = train_set,
  n.tree = 3000,
  interaction.depth	 = 2,     # Maximum depth of each tree
  shrinkage = 0.01,
  n.minobsinnode = 10,        # Minimum number of observations in the terminal nodes of the trees
  cv.folds = 10               # Number of cross-validation folds to perform.
  )

# Save the best number of trees
best_gbm_b <- which.min(gbm_b$cv.error)

## Plot the error curve 
gbm.perf(gbm_b,method = "cv")

## Run a weighted gradint boosting ##

# Assign higher weights to the minority class ["Yes] or 1
weights <- ifelse(train_set$churn == 1,1.88,0.68)

## Run the model
weighted_gbm_b <- gbm(
  churn ~ .,
  data = train_set,
  distribution = "bernoulli",
  n.trees = 4000,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  interaction.depth = 2, 
  weights = weights,  # Assign higher weights  t 
  cv.folds = 10,
)
# Save the best number of trees
weighted_gbm_best <- which.min(weighted_gbm_b$cv.error)

# Plot the error curve
gbm.perf(weighted_gbm_b,method = "cv")

## Tune the GBM ##

# Create a hypergrid for learing rate (shrinkage)
hyper_grid_learning_rate <- expand.grid(
  shrinkage = c(0.3,0.1,0.05,0.01,0.005),
  cv_error = NA,
  trees = NA
  )

# Execute the grid 
for ( i  in seq_len(nrow(hyper_grid_learning_rate)) ) {
  
  # Fit the model
  grid_gbm_tune <- gbm(
    formula = churn ~.,
    distribution = "bernoulli",
    data = train_set,
    n.trees = 4000,
    interaction.depth = 2,
    shrinkage = hyper_grid_learning_rate$shrinkage[i], # Take shrinkage value from hyper grid
    n.minobsinnode = 10,
    weights = weights,   # Assign higher weights to the minority class ["Yes" or 1]
    cv.folds = 10
  )
  
  # Save the number of trees and the cv.error
  hyper_grid_learning_rate$trees[i] <- which.min(grid_gbm_tune$cv.error)
  hyper_grid_learning_rate$cv_error[i] <- min(grid_gbm_tune$cv.error)
}

# Save the best performance
best_gbm_rate<- arrange(hyper_grid_learning_rate,cv_error)%>%head(1)

## Tune the tree specific params 

# Tree sp grid search
tree_sp_grid <- expand.grid(
  ntree = 4000,
  shrinkage = 0.010,
  interaction.depth = c(1,3,5),
  n.minobsinnode = c(5,10,15)
)

## Create a model function
gbm_model_tune <- function(ntree,shrinkage,interaction.depth,n.minobsinnode){
  
  # Fit the model 
  m <- gbm(
    formula = churn ~ .,
    distribution = "bernoulli",
    data = train_set,
    weights = weights,
    cv.folds = 10,
    
    # Fuction input params
    n.trees = ntree,
    shrinkage = shrinkage ,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode
  )
  # Return the cv.error
  min(m$cv.error)
}

## Perform the search grid
tree_sp_grid$cv_error <- pmap_dbl(  # pmap_dbl() runs model_fit() on each row of hyper_grid
  tree_sp_grid,
  ~ gbm_model_tune(
    ntree = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4)
  )

## Save the best params
gbm_tune_params <- tree_sp_grid %>% arrange(cv_error) %>% head(1)

## Fit the final model

gbm_model_final <- gbm(
  formula = churn ~ .,
  distribution = "bernoulli",
  data = train_set,
  weights = weights,
  cv.folds = 10,
  n.trees = 1600,
  n.minobsinnode = gbm_tune_params$n.minobsinnode,
  shrinkage = gbm_tune_params$shrinkage,
  interaction.depth = gbm_tune_params$interaction.depth
)

## Stochastic GBM  with h2o ##

# Start h2o
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-11.0.26.4-hotspot")
h2o.init(max_mem_size = "4G")

## Convert the training data into h2o object and assign higher weights to the minority class

train_h2o_prep <- train_set
train_h2o_prep$weights <- ifelse(train_set$churn == 1,1.88,0.68)
  
train_h2o <- as.h2o(train_h2o_prep)

# Set the Y response and X predictors
Y <- "churn"
X <- setdiff(colnames(train_h2o),"churn")

## Define a hyper grid 
hyper_grid_h2o <- list(
  sample_rate = c(0.5,0.75,1),
  col_sample_rate = c(0.5,0.75,1),
  col_sample_rate_per_tree = c(0.5,0.75,1)
  )

## Define a search strategy
search_strategy <- list(
  strategy = "RandomDiscrete", # Random Grid Search
  stopping_metric = "auc",
  stopping_tolerance = 0.001,
  max_runtime_secs = 60 * 30,
  stopping_rounds = 10
  )

## Perform the grid search 
h2o_grid_gbm <- h2o.grid(
  algorithm = "gbm",
  grid_id = "grid_gbm",
  x = X,
  y = Y,
  distribution = "bernoulli",
  weights_column = "weights", # Assign higher weights to the minority class
  training_frame = train_h2o,
  hyper_params = hyper_grid_h2o,
  search_criteria = search_strategy,
  
  # Take the best params from gbm_tune
  ntrees = 2000,
  learn_rate = gbm_tune_params$shrinkage,
  max_depth = gbm_tune_params$interaction.depth,
  min_rows = gbm_tune_params$n.minobsinnode,
  
  # Stopping Criteria
  nfolds = 5,
  stopping_rounds = 10,
  stopping_tolerance = 0
  )

## Extract the results
h2o_grid_perf <- h2o.getGrid(grid_id = "grid_gbm")

# Take the best model id
best_model_id <- h2o_grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

## Save the models
saveRDS(weighted_gbm_b,file = "weighted_gbm")
saveRDS(gbm_model_final,file = "gbm_model_tuned")
h2o.saveModel(best_model, path = "C:\\Users\\Huawei\\OneDrive\\Telco_Churn_project\\Telco-Customer-Churn\\Models\\GBMs\\GBMs_Models")

h2o.removeAll()




