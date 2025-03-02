
#### Libraries ####
library(tidyverse)
library(ranger)
library(h2o)


#### Random Forest ####

# Load the training set
train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")

## Fit a baseline untune model

# Define the number of features p
n_features <- length(setdiff(names(train_set),"churn"))

model_rf_untune <- ranger(formula = churn ~ .,
                          data = train_set,
                          num.trees = n_features * 10, # p * 10    Recomended
                          mtry = sqrt(n_features),     # sqrt of p Recomended
                          respect.unordered.factors = "order")

## Get the error
defualt_error <- model_rf_untune$prediction.error


## Fit a tuned RF model 

# Define a grid search
rf_grid <- expand.grid(
          mtry = floor(n_features * c(.05, .15, .25, .333, .4)),  # Number of variables to possibly split at in each node
          min.node.size = c(1,3,5,10),                            # Minimal node size to split at.
          replace = c(TRUE,FALSE),                                # Sample with replacement.
          sample.fraction = c(0.5,0.7,0.8),                       # Fraction of observations to sample
          pred_error = NA)

  
# Execite a grid search 
for ( i in seq_len(nrow(rf_grid))){
  
  # Fit the model with Ranger
  rf_fit <- ranger(
    churn ~ .,
    data = train_set,
    num.trees = n_features * 10,
    respect.unordered.factors = "order",
    probability = TRUE,
    
    # Grid params to search
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min.node.size[i],
    replace = rf_grid$replace[i],
    sample.fraction = rf_grid$sample.fraction[i]
  )
  
  # Get the error
  rf_grid$pred_error[i] <- tuned_ranger$prediction.error
}
# Arrange  models and save the best model
hyperparams <- rf_grid %>%
  arrange(pred_error) %>%
  head(1)

## Re-Train the RF with tuned hyperparameters
rf_tune <- ranger(
  churn ~ .,
  data = train_set,
  num.trees = 1200, # Increase the ntree 
  respect.unordered.factors = "order",
  probability = TRUE,
  
  # Re- Train with tuned parameters
  mtry = hyperparams$mtry,
  min.node.size = hyperparams$min.node.size,
  replace = hyperparams$replace,
  sample.fraction = hyperparams$sample.fraction
)
rf_tune$prediction.error

## Tune RF with H2o and random grid search 

# Start h2o
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-11.0.26.4-hotspot")
h2o.init()

# Convert the training data into h2o object
train_h2o <- as.h2o(train_set)

# Define the x predictors and y responce
y <- "churn"
x <- setdiff(colnames(train_h2o),y)

# h2o hyperparameter grid
h2o_hyper_grid <- list(
  max_depth = c(5,10,15,20),
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows =c(1, 3,5, 10),
  sample_rate = c(.55, .632, .70, .80)
)

# h2o Random gird Search Criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "AUC",
  stopping_tolerance = 0.001,
  stopping_rounds = 10,
  max_runtime_secs = 60 * 10
)

## Execute h2o random grid search 
h2o_grid_search <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "h2o_grid_rf",
  x = x,                       #  vector containing the names of the predictor variables
  y = y,                       #  name of the response variable
  training_frame = train_h2o,
  hyper_params = h2o_hyper_grid,
  search_criteria = search_criteria,
  ntrees = n_features * 10,
  stopping_metric = "AUC",    # Metric to use for early stopping AUC
  stopping_rounds = 10,       # Early stopping after 10 trees aded if 
  stopping_tolerance = 0.005  # Relative improvement is not at least 0.05 % 
)

# Collect the results
h2o_params <- h2o.getGrid(
  grid_id = "h2o_grid_rf",
  sort_by = "AUC",
  decreasing = TRUE) 

## Re-Train the model with the hyperparameters
h2o_rf_tune <- h2o.randomForest(
  x = x,
  y = y,
  training_frame = train_h2o,
  model_id = "h2o_rf_tune",
  max_depth = 10,
  min_rows = 10,
  mtries = 6,
  sample_rate = 0.55,
  ntrees = 1200,              # Increase the ntree to 1200
  stopping_metric = "AUC",    # Use AUC for early stopping
  stopping_rounds = 20,       # Stop after 20 rounds of no improvement
  stopping_tolerance = 0.01,  # Stop if AUC improvement is less than 0.1 %
  score_tree_interval = 10,   # Score the model after every 10 trees
  score_each_iteration = TRUE # Evaluate after each iteration (tree)
)

## Save the models
saveRDS(rf_tune,file = "ranger_RF_tune")
h2o.saveModel(h2o_rf_tune, path = "C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/Models/Random_Forest/Random_Forest_models")

## Remove the cluster
h2o.removeAll()












