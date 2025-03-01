
#### LIbraries ####
library(tidyverse)
library(corrplot)
library(viridis)
#### EDA ####

# Load the telco_dataset
telco_churn<- readRDS("C:/Users/Huawei/OneDrive/Telco_Churn_project/Telco-Customer-Churn/telco.clean.rds")

## Plot the distribution of categories for each variable

telco_churn %>%
  select(-tenure,-customer_id,-monthly_charges,-total_charges) %>%  # Remove numerical column
  pivot_longer(
    cols = everything(),
    names_to = "Category",
    values_to = "Value"
    ) %>%
  # Plot a Barchart
  ggplot(aes(x = Value,fill = Value))+
  scale_fill_viridis_d(option = "plasma")+    # Replace the default colorts with plasma pallete
  geom_bar()+                                # Create a Bar_chart
  facet_wrap(~Category,scales = "free_y" )+ # Devide the chart by type of category 
  coord_flip()+
  theme_classic()+
  
  # Add Title and text size
  labs(title = "Distribution of categories for each variable",
       x = "Category",
       y = "Count")+
  theme(
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 15),
    legend.position = "none",
  )

## Plot rhe distribution of all numerical features 
telco_churn %>%
  select(monthly_charges,total_charges,tenure) %>%
  pivot_longer(cols = everything(),
               names_to = "Features",
               values_to = "Value") %>%
  
  # Plot the Histogram 
  ggplot(aes(x = Value,fill = after_stat(count)))+
  facet_grid(~Features,scales = "free_x")+
  geom_histogram(bins = 20,color = "black")+
  scale_fill_viridis_c(option = "magma")+
  theme_minimal()+
  
  # Add title and text 
  labs(
    title = "Histogram of all numerical features",
    x = "Features",
    y = "Count"
  )+
  theme(
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 10)
  )

## Correlation Matrix

# Calculate the correlation between numerical features
telco_churn %>%
  select(monthly_charges,total_charges,tenure) %>%
  cor()%>%
  
  # Plot the Correlation Matrix
  corrplot(method ="color",
           col = viridis(200,option ="magma"),
           addCoef.col = "black",
           number.cex = 0.9,
           tl.cex = 0.7,
           order = "hclust",
           tl.srt = 45 
           )

## Stacked Bar Plot for Visualizing Relationships
ggplot(data = telco_churn,aes(x = payment_method, fill = churn))+
  geom_bar()+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("No" = "grey","Yes" = "red"))+
  labs(
    title = "Churn and Payment Methods",
    x = "Count",
    y = "Payment Method")+
  theme(
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 10)
  )

## Stacked bar plot Churn and tevh support 
ggplot(data = telco_churn,aes(x = tech_support,fill = churn))+
  geom_bar()+
  scale_fill_manual(values = c("No" = "grey","Yes" = "red"))+
  theme_minimal()+
  coord_flip()+
  labs(
    title = "Churn and Tech Support",
    x = "Count",
    y = "Tech Support")+
  theme(
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 10)
  )

## Stacked bar plot Churn and Contract
ggplot(data = telco_churn,aes(x = contract,fill = churn))+
  geom_bar()+
  scale_fill_manual(values = c("No" = "grey","Yes" = "red"))+
  coord_flip()+
  theme_classic()+
  labs(
    title = "Churn and Contract Type",
    x = "Count",
    y = "Contract Type")+
  theme(
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 10)
  )
  
## Stacked bar plot Churn and Seniors

# Label the column senior citizen
telco_churn$senior_citizen <- factor(telco_churn$senior_citizen,levels = c("0","1"),labels = c("Yes","No"))

# Plot the Bar Chart
ggplot(data = telco_churn,aes(x = senior_citizen,fill = churn))+
  geom_bar()+
  scale_fill_manual(values = c("No" = "grey","Yes" = "red"))+
  coord_flip()+
  theme_classic()+
  labs(
    title = "Churn and Senior",
    x = "Count",
    y = "Senior Citizen")+
  theme(
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 10)
  )








  
  