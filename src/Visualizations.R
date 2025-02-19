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
#visualizations
# Create a stacked bar chart
custom_colors <- c( "#FFC0CB", "#E6E6FA", "#FFDAB9","#98FF98", "#ADD8E6")
custom_colors2 <- c("hotpink","violet","indianred2","plum3")
#age vs pcos status 
ggplot(data = train, aes(x = New_age, fill = PCOS)) +
  geom_bar(position = "fill") +
  labs(title = "Age Distribution by PCOS Status",x = "Age",y = "Proportion",fill = "PCOS Status") +
  scale_fill_manual(values = custom_colors) + theme_minimal()

#Excericise habit  analysis
ggplot(data=train) + geom_tile(mapping = aes(x = Exercise_Frequency, y = Exercise_Type,  fill = PCOS)) +
  labs(title = "Heatmap of Exercise Habits and PCOS Count",x = "Exercise Frequency",y = "Exercise Type",fill = "PCOS Count") +
  theme_minimal() + scale_fill_manual(values = custom_colors) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#sleep hours vs health indicators
#sleep hours vs Insulin Resistance
sh <- ggplot(train, aes(x = Sleep_Hours, y = Insulin_Resistance, color = Insulin_Resistance)) +
  geom_beeswarm() +
  labs(title = "Sleep Hours by Insulin Resistance Status",
       x = "Insulin Resistance",
       y = "Sleep Hours") +
  scale_color_manual(values = custom_colors2) +
  theme_minimal()

# Print the plot
print(sh)

#sleep hours vs  hyperandrogenism

sha  <- ggplot(train,aes(x = Sleep_Hours, fill = Hormonal_Imbalance )) +  geom_bar() +
  labs(title = "Sleep Hours vs Hormonal Imbalance",x = "Sleep Hours", y = "PCOS count") +
  scale_fill_manual(values = custom_colors2)
print(sha)


# weight distribution vs exercise type
palette1 <- brewer.pal(9, "PuRd") # 9 colors from PuRd
palette2 <- brewer.pal(9, "RdPu") # 9 colors from RdPu

# Combine the two palettes, and select the first 16 unique colors
weight_plot <- ggplot(train, aes(x = factor(Exercise_Type), y = Weight_kg, fill = Exercise_Type)) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Exercise Type",
       x = "Exercise Type",
       y = "Weight (kg)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_blank(), # Hide x-axis text
        axis.ticks.x = element_blank(), # Hide x-axis ticks
        strip.text.x = element_blank(), # Adjust size of facet labels
        plot.title = element_text(hjust = 0.5), panel.spacing = unit(1, "lines")) + # Center the title
  scale_fill_manual(values = custom_colors3) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(train$Weight_kg) - 5, max(train$Weight_kg) + 5)) +
  facet_wrap(~ Exercise_Type, ncol = 4, scales = "free")

# Print the plot
print(weight_plot)


#hormonal imbalance across various age groups
line_chart <- ggplot(train, aes(x = Age, y = Hormonal_Imbalance , group = 1)) +
  geom_line(color = "violet", size = 1) +
  geom_point(color = "darkmagenta", size = 3) +
  labs(title = "Hormonal Imbalance Across Age Groups",
       x = "Age Group",
       y = "Proportion of Individuals with Hormonal Imbalance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(line_chart)

#Conception Analaysis  
library(treemap)
# Summarize the data
train_f <- train %>% filter(Conception_Difficulty !=  "No, Yes, not diagnosed by a doctor")
conception_summary <- train_f %>%
  group_by(PCOS, Conception_Difficulty) %>%
  summarise(Count = n(), .groups = 'drop')

simple_bar_chart <- ggplot(conception_summary, aes(x = PCOS, y = Count, fill = Conception_Difficulty)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Conception Difficulty Across PCOS Status",
       x = "PCOS Status",
       y = "Count of Individuals") +
  scale_fill_manual(values = custom_colors2) +
  theme_minimal()

# Print the plot
print(simple_bar_chart)