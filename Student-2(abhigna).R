rm(list = ls())

Shopping_data <- read.csv("Shopping_CustomerData.csv", header = TRUE)

head(Shopping_data)
getwd()
str(Shopping_data)

head(Shopping_data, 2)

install.packages("ggplot2")

install.packages("dplyr")

# Load required libraries 
library(ggplot2)
library(MASS)




# Question-1(A):

# Scatterplot with Linear Trendline
ggplot(Shopping_data, aes(x = AnnualIncome, y = SpendingScore)) +
  geom_point() +  # scatter plot
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # linear trendline
  labs(title = "Scatterplot of Annual Income vs Spending Score",
       x = "Annual Income (in USD)",
       y = "Spending Score") +
  theme_minimal()

# Question-1(B):

# Histogram of Spending Score with Normal Curve Overlay
ggplot(Shopping_data, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(Shopping_data$SpendingScore), sd = sd(Shopping_data$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()


# Question-2(A):

# Load required library
library(ggplot2)

# Boxplot of Spending Score by Annual Income categories
ggplot(Shopping_data, aes(x = factor(AnnualIncome), y = SpendingScore)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Spending Score by Annual Income",
       x = "Annual Income (in USD)",
       y = "Spending Score") +
  theme_minimal()


# Question-2(B):

# Histogram of Spending Score with Normal Curve Overlay
ggplot(Shopping_data, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(Shopping_data$SpendingScore), sd = sd(Shopping_data$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()