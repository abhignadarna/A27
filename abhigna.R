rm(list = ls())

#setwd("E:/Documents/Abhigna")
abhigna <- read.csv("Shopping_CustomerData.csv", header = TRUE)

head(abhigna)
getwd()
str(abhigna)

head(abhigna, 2)

install.packages("ggplot2")

install.packages("dplyr")


# Load required libraries
library(ggplot2)
library(MASS) 

# Scatterplot with Linear Trendline
ggplot(abhigna, aes(x = AnnualIncome, y = SpendingScore)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Scatterplot of Annual Income vs Spending Score",
       x = "Annual Income (in USD)",
       y = "Spending Score") +
  theme_minimal()


# Histogram of Spending Score with Normal Curve Overlay
ggplot(abhigna, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(abhigna$SpendingScore), sd = sd(abhigna$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()








# Load required library
library(ggplot2)

# Boxplot of Spending Score by Annual Income categories
ggplot(abhigna, aes(x = factor(AnnualIncome), y = SpendingScore)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Spending Score by Annual Income",
       x = "Annual Income (in USD)",
       y = "Spending Score") +
  theme_minimal()


# Histogram of Spending Score with Normal Curve Overlay
ggplot(abhigna, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(abhigna$SpendingScore), sd = sd(abhigna$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()
















# Load required library
library(ggplot2)
library(dplyr)

# Create a new variable to categorize SpendingScore into categories (Low, Medium, High)
abhigna$SpendingScoreCategory <- cut(abhigna$SpendingScore, 
                                     breaks = c(0, 33, 66, 100), 
                                     labels = c("Low", "Medium", "High"),
                                     right = FALSE)

# Normalize data for a fair comparison (convert to percentage)
normalized_data <- abhigna %>%
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

















install.packages("reshape2")
# Install and load necessary libraries
install.packages("reshape")
install.packages("pheatmap")
library(reshape)
library(pheatmap)

# Calculate Spearman's correlation matrix for multiple variables
cor_matrix <- cor(abhigna[, c("AnnualIncome", "SpendingScore")], method = "spearman")

# Print the correlation matrix
print(cor_matrix)

# Convert correlation matrix to a long format for ggplot2 (using reshape package)
cor_matrix_melted <- melt(cor_matrix)

# Create a correlation heatmap using pheatmap
pheatmap(cor_matrix, display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
         color = colorRampPalette(c("red", "white", "green"))(100), 
         main = "Spearman's Correlation Heatmap")


# Calculate Spearman's Rho for correlation
correlation_result <- cor(abhigna$AnnualIncome, abhigna$SpendingScore, method = "spearman")

# Display the correlation result
print(correlation_result)












# Rename your dataset (replace 'abhigna' with your actual dataset name)
my_data <- abhigna

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




















# Load necessary library
library(dplyr)

# Example dataset 'my_data'
# Make sure to replace 'my_data' with your actual dataset name

# Discretize 'AnnualIncome' into Low, Medium, High categories
my_data$IncomeCategory <- cut(my_data$AnnualIncome,
                              breaks = c(0, 30000, 70000, Inf),  # Adjust ranges as needed
                              labels = c("Low", "Medium", "High"),
                              right = FALSE)

# Discretize 'SpendingScore' into Low, Medium, High categories
my_data$SpendingCategory <- cut(my_data$SpendingScore,
                                breaks = c(0, 30, 60, 100),  # Adjust ranges as needed
                                labels = c("Low", "Medium", "High"),
                                right = FALSE)

# Create a contingency table (cross-tabulation)
contingency_table <- table(my_data$IncomeCategory, my_data$SpendingCategory)

# Print the contingency table
print(contingency_table)

# Perform the Chi-square Test
chi_square_test <- chisq.test(contingency_table)

# Print the Chi-square test results
print(chi_square_test)

# Check the results
if (chi_square_test$p.value < 0.05) {
  cat("There is a significant relationship between Annual Income and Spending Score (p < 0.05).\n")
} else {
  cat("There is no significant relationship between Annual Income and Spending Score (p >= 0.05).\n")
}





















# Load necessary library
library(ggplot2)

# Define the output file as a PNG
png("AnnualIncome_vs_SpendingScore.png", width = 800, height = 600)

# Create the scatter plot with a linear trendline
ggplot(abhigna, aes(x = AnnualIncome, y = SpendingScore)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trendline
  labs(
    title = "Relationship Between Annual Income and Spending Score",
    x = "Annual Income (in USD)",
    y = "Spending Score"
  ) +
  theme_minimal()

# Close the PNG device
dev.off()

# Output message
cat("Plot saved as 'AnnualIncome_vs_SpendingScore.png' in the working directory.")










# Create the histogram with a normal curve overlay
ggplot(abhigna, aes(x = SpendingScore)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(abhigna$SpendingScore), sd = sd(abhigna$SpendingScore)),
                color = "red", size = 1) +
  labs(title = "Histogram of Spending Score with Normal Curve Overlay",
       x = "Spending Score",
       y = "Density") +
  theme_minimal()

# Save the histogram as a PNG image
ggsave("normality_histogram.png", width = 8, height = 6, dpi = 300)

# The file 'normality_histogram.png' will be saved in the current working directory

