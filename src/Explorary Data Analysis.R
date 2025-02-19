#loading wanted libraries
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(skimr)
library(plotly)
library(ggbeeswarm)
#loading the dataset as , train,test,sample  . here i already made changes on train data in the age column as that was not convinient . so i took mean and median for age column to produce new_age .
train <- read.csv("C:/Users/prade/OneDrive/Desktop/pcos/cleantrain.csv")
test <- read.csv("C:/Users/prade/OneDrive/Desktop/pcos/test.csv")
sample <- data <- read.csv("C:/Users/prade/OneDrive/Desktop/pcos/sample_submission.csv")
#Explorary  Data Analysis
# Calculate summary statistics for New_age
mean_age <- mean(train$New_age, na.rm = TRUE)
median_age <- median(train$New_age, na.rm = TRUE)
min_age <- min(train$New_age, na.rm = TRUE)
max_age <- max(train$New_age, na.rm = TRUE)
quantiles_age <- quantile(train$New_age, na.rm = TRUE)

# Print summary statistics for New_age
print("Summary statistics for New_age:")
print(mean_age)
print(median_age)
print(min_age)
print(max_age)
print(quantiles_age)

# Calculate summary statistics for Weight_kg
mean_weight <- mean(train$Weight_kg, na.rm = TRUE)
median_weight <- median(train$Weight_kg, na.rm = TRUE)
min_weight <- min(train$Weight_kg, na.rm = TRUE)
max_weight <- max(train$Weight_kg, na.rm = TRUE)
quantiles_weight <- quantile(train$Weight_kg, na.rm = TRUE)

# Print summary statistics for Weight_kg
print("Summary statistics for Weight_kg:")
print(mean_weight)
print(median_weight)
print(min_weight)
print(max_weight)
print(quantiles_weight)