# Imports 
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caTools)
library(naniar)
library(mice)
library(caret)

# Set default plot theme
theme_set(theme_bw())

# Load train and test data
train.data <- read.csv('train.csv')
test.data <- read.csv('test.csv')

## Data Cleaning
# Check for missing values on data
any(is.na(train.data))
any(is.na(test.data))

# Check percentage of missing data
prop_miss(train.data)
prop_miss(test.data)

# Summarize which variables are affected
miss_var_summary(train.data)
miss_var_summary(test.data)

# Get visual representation of data missingness of the test dataframe
gg_miss_var(test.data)

# Remove rows with missing MasVnrArea feature
train.data <- train.data %>% filter(!is.na(MasVnrArea))

# Data inputation with mice package
tempData <- mice(train.data,m=5,maxit=50,meth='pmm',seed=42)
train.data <- complete(tempData,1)
# summary(tempData)

# Inspecting the distribution of original and imputed data
densityplot(tempData)

# Split data in numeric and non-numeric 
num.train.data <- select_if(train.data, is.numeric)
cat.train.data <- select_if(train.data, function(x){!is.numeric(x)})

# Factor categorical data
col.names <- colnames(cat.train.data)
cat.train.data <- cat.train.data %>% mutate_at(col.names, factor)

## Exploratory data analysis
# Join all clean data into one single dataframe again
train.data <- cbind(num.train.data, cat.train.data)

# Check sale price distribution
pl <- ggplot(cat.train.data, aes(x = SalePrice)) + geom_bar(aes(fill = SalePrice))

## Model training and testing
# Using multinomial Logistic Regression
model <- multinom(SalePrice ~., train.data)
summary(model)

