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
#head() function to view sample data
head(train)
#to get summary of the train data
summary(train)
# Check for missing values in the entire dataset
colSums(is.na(train))

# Replace NA values in 'New_age' column with the mean
train$New_age[is.na(train$New_age)] <- mean(train$New_age, na.rm = TRUE)

# Replace NA values in 'Weight_kg' column with the mean
train$Weight_kg[is.na(train$Weight_kg)] <- mean(train$Weight_kg, na.rm = TRUE)
# Verify that NA values have been handled
colSums(is.na(train))

# Round the values in 'New_age' column
train$New_age <- round(train$New_age)

# Round the values in 'Weight_kg' column
train$Weight_kg <- round(train$Weight_kg)

#arrange in order by age
train %>% arrange(Age)