
#### Libraries ####
library(h2o)

## Connect to h2o
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-11.0.26.4-hotspot")
h2o.init()

## Load the training set 
telco_train_set <- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/data/clean_data/telco_train_set.rds")

#### Auto ML ####

## Convert the training set into h2o object
train_h2o <- as.h2o(telco_train_set)

# Set up the predictors and responce as x and y 
y <- "churn"
x <- setdiff(colnames(train_h2o),y)

## Make a auto ml ##
auto_ml <-h2o.automl(
  seed = 123,
  x = x,
  y = y,
  training_frame = train_h2o,
  nfolds = 5,                                  # 5-fold cross-validation
  max_models = 20,                             # train up to 20 different models
  keep_cross_validation_predictions = TRUE,
  max_runtime_secs = 60 * 180,                 # 3h run time 
  stopping_metric = "AUC",                     # The process will stop when the AUC doesn't improve
  stopping_rounds = 50,                        # It will stop if AUC doesn't improve for 50 consecutive rounds
  stopping_tolerance = 0.01,                   # Allow for very small improvements in the AUC
  max_runtime_secs_per_model = 60 * 10   
)

h2o.removeAll()



