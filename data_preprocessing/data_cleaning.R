

#### Libraries ####
library(tidyverse)
library(mice)

## Data Cleaning ####

# Load the dataset
WA_Fn_UseC_Telco_Customer_Churn <- read_csv("data/raw_data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

## Clean the names 

# Make a function proper that makes the colnames tidy with _ separation and all lower case
proper <- function(df){
  df %>%
    rename_with(~gsub("([a-z])([A-Z])", "\\1_\\2", .))%>%
    rename_with(~str_to_lower(.))
  }

#  Clean the names
telco_clean_names <- proper(WA_Fn_UseC_Telco_Customer_Churn)

## Mice imputaion for Na

# Create a mice mice imputaion blueprint with "rf" = Random Forest
imputaion_mice <- mice(telco_clean_names,m = 3,method = "rf")

## Complete the dataset
telco_churn_imputed <- complete(imputaion_mice,3)

### Correct the data types 

## Convet char into factors 
telco_proper_types <- telco_churn_imputed %>%
  mutate_if(is.character, factor)

telco_proper_types$senior_citizen <- factor(telco_proper_types$senior_citizen)

## Export the cleaned data 
write.csv(telco_proper_types,"telco.cleaned.csv")
write_rds(telco_proper_types,"telco.clean.rds")


