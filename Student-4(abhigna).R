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

# Question-3:

# Load required library
library(ggplot2)
library(dplyr)

# Create a new variable to categorize SpendingScore into categories (Low, Medium, High)
Shopping_data$SpendingScoreCategory <- cut(Shopping_data$SpendingScore, 
                                           breaks = c(0, 33, 66, 100), 
                                           labels = c("Low", "Medium", "High"),
                                           right = FALSE)

# Normalize data for a fair comparison (convert to percentage)
normalized_data <- Shopping_data %>%
  group_by(AnnualIncome, SpendingScoreCategory) %>%
  summarise(count = n()) %>%
  group_by(AnnualIncome) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot normalized stacked bar chart
ggplot(normalized_data, aes(x = factor(AnnualIncome), y = percentage, fill = SpendingScoreCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Spending Score Proportions by Annual Income",
       x = "Annual Income (in USD)",
       y = "Percentage (%)",
       fill = "Spending Score Category") +
  theme_minimal()



# Question-4(A):

install.packages("reshape2")
# Install and load necessary libraries
install.packages("reshape")
install.packages("pheatmap")
library(reshape)
library(pheatmap)

# Calculate Spearman's correlation matrix for multiple variables
cor_matrix <- cor(Shopping_data[, c("AnnualIncome", "SpendingScore")], method = "spearman")

# Print the correlation matrix
print(cor_matrix)

# Convert correlation matrix to a long format for ggplot2 (using reshape package)
cor_matrix_melted <- melt(cor_matrix)

# Create a correlation heatmap using pheatmap
pheatmap(cor_matrix, display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
         color = colorRampPalette(c("red", "white", "green"))(100), 
         main = "Spearman's Correlation Heatmap")


# Calculate Spearman's Rho for correlation
correlation_result <- cor(Shopping_data$AnnualIncome, Shopping_data$SpendingScore, method = "spearman")

# Display the correlation result
print(correlation_result)




# Question-4(B):


# Rename your dataset (replace 'Shopping_data' with your actual dataset name)
my_data <- Shopping_data

# Load necessary library
library(ggplot2)

# Check histogram and normal curve overlay
ggplot(my_data, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(my_data$SpendingScore), sd = sd(my_data$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()

# Conduct Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(my_data$SpendingScore)
print(shapiro_test)

# Decide the test based on p-value
if (shapiro_test$p.value > 0.05) {
  cat("Data appears normal (p > 0.05). Using parametric t-test.\n")
  # Perform t-test
  t_test <- t.test(SpendingScore ~ CustomerGender, data = my_data)
  print(t_test)
} else {
  cat("Data does not appear normal (p <= 0.05). Using non-parametric Wilcoxon test.\n")
  # Perform Wilcoxon test
  wilcox_test <- wilcox.test(SpendingScore ~ CustomerGender, data = my_data)
  print(wilcox_test)
}



