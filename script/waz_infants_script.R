
# Predictors of Weight in Romanian Infants: Weight-for-age z-scores (WAZ)

# Adrian Rus (2023)



# 1. Research Hypotheses/Questions/Objectives------------------------------------

# The objective of the present study was to explore the relationship between infants’ weight and various demographics 
# (i.e., infants’ mother location, marital status, social economic status, and age; and infants being born on term, age, anemia status, rank into the family and dietary patterns).
# Identifying the connection between such factors could help practitioners in developing public health policies and interventions in Romania.



# 2. Data Exploration------------------------------------------------------------

### 2.1. Setting Up the Environment: R Packages Installation----------------------
install.packages("tidyverse")
install.packages("moments")
install.packages(c("lmtest", "car", "carData"))

library(tidyverse)
library (moments)
library(lmtest)
library(car)
library(carData)


### 2.2. Setting the Working Directory------------------------------------------

# Check the working directory
getwd()

# For setting a new working directory use the setwd()
# setwd("C:\\Users\\adria\\Documents\\waz_infants")

# List Files in Directory To see a list of all files in the current working directory, you can use
list.files()


### 2.3. Import and check the dataset--------------------------------------------

library(readr)
waz_final <- read_csv("data/waz_final.csv")
View(waz_final)

##### 2.3.1. Basic inspection of dataset-----------------------------------------

# View the first 6 rows
head(waz_final)

# View the last 6 rows
tail(waz_final)

# View the structure of dataset
str(waz_final)

# Summarize the dataset
summary(waz_final)


##### 2.3.2. Check the tidiness of the dataset-----------------------------------

# Checking the missing values. 
# Tidy data should have a consistent structure, but it can contain missing values.
sum(is.na(waz_final))

# Tidy data should ideally not have duplicated rows unless the repetition is meaningful.        
sum(duplicated(waz_final))

# Column names should be clear and descriptive but not too lengthy.
names(waz_final)

##### 2.3.3. Check the data types and dimensionality
# Tells you whether the object is a data frame, matrix, vector, list, etc.
class(waz_final)

# Returns the class (type) of each column in the data frame.
sapply(waz_final, class)

# Returns the number of columns.
ncol(waz_final)

# Returns the number of rows and columns in a data frame.
dim(waz_final)

# Select only numerical columns
numerical_columns <- waz_final[, sapply(waz_final, is.numeric)]
head(numerical_columns) # Show the first few rows of the numerical columns
View(numerical_columns) # It shows all the variables


# Select only categorical columns (factor or character)
categorical_columns <- waz_final[, sapply(waz_final, function(col) is.factor(col) || is.character(col))]
# Show the first few rows of the categorical columns
head(categorical_columns)

                                          
##### 2.3.4. Specific variable content-------------------------------------------

# Shows unique values in a specific column.
unique(waz_final$mAge2c)

# Allows to identify the unique data in all columns
unique_values_all_columns <- lapply(waz_final, unique)
str(unique_values_all_columns)


# 3. Dependent Variable (DV) and Data Processing---------------------------------

# Dependent variable (DV): cWageZ (weight-for-age z-scores; WAZ)
# Independent variable (IV):

## 3.1. Visual check of the DV---------------------------------------------------


# Histogram for DV
library(ggplot2)
ggplot(waz_final, aes(x = cWageZ)) + 
  geom_histogram(bins = 50, col= "white") +
  ggtitle("Histogram")


# Histogram adding the normal distribution curve
# Calculate mean and standard deviation of the data
data_mean <- mean(waz_final$cWageZ, na.rm = TRUE)
data_sd <- sd(waz_final$cWageZ, na.rm = TRUE)
# Create the ggplot
ggplot(waz_final, aes(x = cWageZ)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, col= "white") + 
  stat_function(fun = dnorm, args = list(mean = data_mean, sd = data_sd), color = "red") +
  ggtitle("Histogram with Normal Distribution Curve")


# Boxplot to identify outliers and understand the data's spread.
ggplot(waz_final, aes(y = cWageZ)) + geom_boxplot()
ggplot(waz_final, aes(y = cWageZ)) + 
  geom_boxplot(outlier.colour = "red")


# Density Plot: To visualize the distribution.
ggplot(waz_final, aes(x = cWageZ)) + geom_density()

# Q-Q Plot: To assess normality.
ggplot(waz_final, aes(sample = cWageZ)) + stat_qq()


#Pair Plot: To visualize relationships between multiple numerical variables, if applicable.
# Filter only numeric columns
waz_final_numeric <- waz_final[, sapply(waz_final, is.numeric)]
# Create the pairs plot
pairs(waz_final_numeric)


## 3.2. Compute central tendency, dispersion, skewness, kurtosis, and normality (Shapiro-Wilk P-value)-----------

library(tidyverse)
library(moments)

summary_stats <- waz_final %>%
  summarise(
    mean = mean(cWageZ, na.rm = TRUE),
    median = median(cWageZ, na.rm = TRUE),
    variance = var(cWageZ, na.rm = TRUE),
    sd = sd(cWageZ, na.rm = TRUE),
    skewness = skewness(cWageZ, na.rm = TRUE),
    kurtosis = kurtosis(cWageZ, na.rm = TRUE),
    shapiro_p_value = shapiro.test(cWageZ)$p.value
  )
# View the summary statistics
summary_stats


## 3.3. Other statistics used for identifying outliers---------------------------

# Coefficient of Variation**: To compare the degree of variation if you have more than one DV.
sd(waz_final$cWageZ, na.rm = TRUE) / mean(waz_final$cWageZ, na.rm = TRUE)

# Percentiles: 25th, 50th, 75th, and other percentiles to understand distribution.
quantile(waz_final$cWageZ, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Confidence Intervals: For mean or median.
t.test(waz_final$cWageZ)$conf.int  # For mean


# Tukey's method: it uses the Interquartile Range (IQR) to identify outliers.
# Load the dplyr package
library(dplyr)

# Tukey's Method to find outliers
Q1 <- quantile(waz_final$cWageZ, 0.25, na.rm = TRUE)
Q3 <- quantile(waz_final$cWageZ, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outliers_Tukey <- waz_final %>% 
  filter(cWageZ < (Q1 - 1.5 * IQR) | cWageZ > (Q3 + 1.5 * IQR))
# View the outliers
print(outliers_Tukey)

# Group the outliers by cID
library(dplyr)
# Grouping outliers by cID
grouped_outliers_Tukey <- outliers_Tukey %>%
  group_by(cID) %>%
  summarize(
    Num_Outliers = n(),  # Number of outliers per cID
    Min_cWageZ = min(cWageZ, na.rm = TRUE),  # Minimum value of cWageZ for each cID
    Max_cWageZ = max(cWageZ, na.rm = TRUE)  # Maximum value of cWageZ for each cID
  )
# View the grouped outliers
print(grouped_outliers_Tukey)


# Z-score Method**: used to find outliers

# Load the dplyr package if not already loaded
# library(dplyr)

# Z-score Method to find outliers
mean_val <- mean(waz_final$cWageZ, na.rm = TRUE)
sd_val <- sd(waz_final$cWageZ, na.rm = TRUE)

outliers_Zscore <- waz_final %>% 
  mutate(z_score = (cWageZ - mean_val) / sd_val) %>% 
  filter(abs(z_score) > 3)
# View the outliers
print(outliers_Zscore)

# Group the outliers by cID
# Grouping outliers by cID using Z-score method and listing all outliers
grouped_outliers_Zscore_listed <- outliers_Zscore %>%
  group_by(cID) %>%
  summarize(
    Num_Outliers = n(),  # Number of outliers per cID
    Min_cWageZ = min(cWageZ, na.rm = TRUE),  # Minimum value of cWageZ for each cID
    Max_cWageZ = max(cWageZ, na.rm = TRUE),  # Maximum value of cWageZ for each cID
    All_Outliers = list(cWageZ)  # List all individual outlier values for each cID
  )
# View the grouped outliers
print(grouped_outliers_Zscore_listed)


# The Modified Z-score: it uses the median and the Median Absolute Deviation (MAD) instead of the mean and standard deviation.
# A common threshold is a Modified Z-score of +/- 3.5.
# Load the dplyr package
# library(dplyr)
# Modified Z-score Method to find outliers
median_val <- median(waz_final$cWageZ, na.rm = TRUE)
mad_val <- mad(waz_final$cWageZ, na.rm = TRUE)

outliers_ModZscore <- waz_final %>% 
  mutate(mod_z_score = 0.6745 * (cWageZ - median_val) / mad_val) %>% 
  filter(abs(mod_z_score) > 3.5)

# View the outliers
print(outliers_ModZscore)



# 4. Independent Variables (IV) and Data Processing------------------------------

## 4.1. Visual check of the IV (for numerical Variables)--------------------------

# Load the ggplot2 package 
library(ggplot2)

# Create a boxplot
ggplot(waz_final, aes(y = mAge)) +
  geom_boxplot() +
  ggtitle("Boxplot of (Mothers' Age; mAge)")

# Create a boxplot
ggplot(waz_final, aes(y = cHageZ)) +
  geom_boxplot() +
  ggtitle("Boxplot of (cHageZ)")


## 4.2. Compute central tendency, dispersion, skewness, kurtosis, and normality (Shapiro-Wilk P-value)-----

# Load the necessary libraries
library(moments)
library(dplyr)

# Calculate summary statistics
summary_stats <- waz_final %>%
  summarise(
    mean_mAge = mean(mAge, na.rm = TRUE),
    median_mAge = median(mAge, na.rm = TRUE),
    variance_mAge = var(mAge, na.rm = TRUE),
    sd_mAge = sd(mAge, na.rm = TRUE),
    skewness_mAge = skewness(mAge, na.rm = TRUE),
    kurtosis_mAge = kurtosis(mAge, na.rm = TRUE),
    shapiro_p_value_mAge = shapiro.test(mAge)$p.value,
    
    mean_cHageZ = mean(cHageZ, na.rm = TRUE),
    median_cHageZ = median(cHageZ, na.rm = TRUE),
    variance_cHageZ = var(cHageZ, na.rm = TRUE),
    sd_cHageZ = sd(cHageZ, na.rm = TRUE),
    skewness_cHageZ = skewness(cHageZ, na.rm = TRUE),
    kurtosis_cHageZ = kurtosis(cHageZ, na.rm = TRUE),
    shapiro_p_value_cHageZ = shapiro.test(cHageZ)$p.value
  )
# View the summary statistics
summary_stats


# Create a nicer table
# Load the necessary libraries
# install.packages("moments")
library(moments)
library(dplyr)
library(tidyr)
# Calculate summary statistics
summary_stats <- waz_final %>%
  summarise(
    mean_mAge = mean(mAge, na.rm = TRUE),
    median_mAge = median(mAge, na.rm = TRUE),
    variance_mAge = var(mAge, na.rm = TRUE),
    sd_mAge = sd(mAge, na.rm = TRUE),
    skewness_mAge = skewness(mAge, na.rm = TRUE),
    kurtosis_mAge = kurtosis(mAge, na.rm = TRUE),
    shapiro_p_value_mAge = shapiro.test(mAge)$p.value,
    
    mean_cHageZ = mean(cHageZ, na.rm = TRUE),
    median_cHageZ = median(cHageZ, na.rm = TRUE),
    variance_cHageZ = var(cHageZ, na.rm = TRUE),
    sd_cHageZ = sd(cHageZ, na.rm = TRUE),
    skewness_cHageZ = skewness(cHageZ, na.rm = TRUE),
    kurtosis_cHageZ = kurtosis(cHageZ, na.rm = TRUE),
    shapiro_p_value_cHageZ = shapiro.test(cHageZ)$p.value
  ) %>%
  gather(key = "Statistic", value = "Value")
# View the summary statistics
summary_stats


# Create the table above and change the format of p-value
# Load the necessary libraries
# install.packages("moments")
library(moments)
library(dplyr)
library(tidyr)

# Calculate summary statistics
summary_stats <- waz_final %>%
  summarise(
    mean_mAge = round(mean(mAge, na.rm = TRUE), 4),
    median_mAge = round(median(mAge, na.rm = TRUE), 4),
    variance_mAge = round(var(mAge, na.rm = TRUE), 4),
    sd_mAge = round(sd(mAge, na.rm = TRUE), 4),
    skewness_mAge = round(skewness(mAge, na.rm = TRUE), 4),
    kurtosis_mAge = round(kurtosis(mAge, na.rm = TRUE), 4),
    shapiro_p_value_mAge = round(shapiro.test(mAge)$p.value, 4),
    
    mean_cHageZ = round(mean(cHageZ, na.rm = TRUE), 4),
    median_cHageZ = round(median(cHageZ, na.rm = TRUE), 4),
    variance_cHageZ = round(var(cHageZ, na.rm = TRUE), 4),
    sd_cHageZ = round(sd(cHageZ, na.rm = TRUE), 4),
    skewness_cHageZ = round(skewness(cHageZ, na.rm = TRUE), 4),
    kurtosis_cHageZ = round(kurtosis(cHageZ, na.rm = TRUE), 4),
    shapiro_p_value_cHageZ = round(shapiro.test(cHageZ)$p.value, 4)
  ) %>%
  gather(key = "Statistic", value = "Value")
# View the summary statistics
summary_stats

# 5. Statistical Testing---------------------------------------------------------

## 5.1. Set the levels of IV-----------------------------------------------------
library(dplyr)
waz_final <- waz_final %>%
  mutate(
    Location = as.factor(Location) %>% relevel(ref = "Urban"),
    cBirthT = as.factor(cBirthT) %>% relevel(ref = "At term (37-40 weeks)"),
    cAgeM3c = as.factor(cAgeM3c) %>% relevel(ref = "6-11 months"),
    mMarital2 = as.factor(mMarital2) %>% relevel(ref = "Married"),
    Anemia2 = as.factor(Anemia2) %>% relevel(ref = "No (11> g/dl)"),
    mSES3c = as.factor(mSES3c) %>% relevel(ref = "Low"),
    cMDD = as.factor(cMDD) %>% relevel(ref = "Yes"),
    cRank3c = as.factor(cRank3c) %>% relevel(ref = "First")
  )
# This code will keep your original waz_final dataset unchanged and store the modifications in a new dataset called waz_final_modified.

# If you want to save this new dataset (waz_final -- I kept the original name), then use this
# write.csv(waz_final, "waz_final_modified.csv", row.names = FALSE)


## 5.2. Model 0: The intercept-only model (null model)---------------------------

# library(tidyverse)
# Run the intercept-only model (null model)
Model_0 <- lm(cWageZ ~ 1, data = waz_final)
# View the summary of the model
summary(Model_0)


## 5.3. Model 1: Diagnostic - Plotting Linearity and Normality------------------------

Model_1 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge, data = waz_final)

# Added Variable Plots for assessing linearity
avPlots(Model_1)

# Q-Q Plot for assessing normality of residuals
qqPlot(Model_1, id.n = 0)


# Model 1: Diagnostic Tests for Model Assumptions: Homoscedasticity, Independence, and Multicollinearity

# Load necessary libraries
# library(car) # for vif(), bptest(), and dwtest()
# library(lmtest) # for dwtest()

# Fit the model
Model_1 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge, data = waz_final)

# Run Breusch-Pagan test and capture p-value
bp_p_value <- bptest(Model_1)$p.value

# Run Durbin-Watson test and capture p-value
dw_p_value <- dwtest(Model_1)$p.value

# Calculate VIF for all predictors
individual_vif <- vif(Model_1)

# Calculate average VIF for all predictors
avg_vif <- mean(individual_vif)

# Round all values to 4 decimal places
bp_p_value <- round(bp_p_value, 4)
dw_p_value <- round(dw_p_value, 4)
avg_vif <- round(avg_vif, 4)
individual_vif <- round(individual_vif, 4)

# Create a table to display the diagnostics
diagnostics_table <- data.frame(
  Category = rep("General", 3),
  Test = c("Breusch-Pagan p-value", "Durbin-Watson p-value", "Average VIF"),
  Value = c(bp_p_value, dw_p_value, avg_vif)
)

# Create a table to display the general diagnostics
diagnostics_table <- data.frame(
  Test = c("Breusch-Pagan p-value", "Durbin-Watson p-value", "Average VIF"),
  Value = c(bp_p_value, dw_p_value, avg_vif)
)

# Display the diagnostics table
print("General Diagnostics:")
print(diagnostics_table)

# Create a table to display individual VIF values
vif_table <- data.frame(
  Test = paste("VIF for", names(individual_vif)),
  Value = individual_vif
)

# Display the VIF table
print("Individual VIFs:")
print(vif_table)



## 5.4 Model 1b: Variables with no outliers were included-----------------------

# The structure of the model**
# DV: cWageZ_no_outliers

# IV: Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers

## Model 1b
Model_1b_no_outliers <- lm(cWageZ_no_outliers ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers, data = waz_final)
# View the summary of the model
summary(Model_1b_no_outliers)
# Perform Omnibus Test
anova(Model_1b_no_outliers)
# Get Confidence Intervals
confint(Model_1b_no_outliers)


## 5.5. Model 2: DV (outliers included)------------------------------------------------------------------

# The structure of the model
# DV: cWageZ
# IV: Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge + cHageZ


# Model 2
Model_2 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge + cHageZ, data = waz_final)
# View the summary of the model
summary(Model_2)
# Perform Omnibus Test
anova(Model_2)
# Get Confidence Intervals
confint(Model_2)


# Model 2: Diagnostic -- Plotting Linearity and Normality
# Added Variable Plots for assessing linearity
avPlots(Model_2)
# Q-Q Plot for assessing normality of residuals
qqPlot(Model_2, id.n = 0)


# Model 2**: Diagnostic Tests for Model Assumptions: Homoscedasticity, Independence, and Multicollinearity

# Load necessary libraries
# library(car)
# library(lmtest)

# Define the model
Model_2 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge + cHageZ, data = waz_final)
# Breusch-Pagan Test for assessing homoscedasticity
bp_test <- bptest(Model_2)
# Durbin-Watson Test for assessing independence of residuals
dw_test <- dwtest(Model_2)
# Variance Inflation Factors (VIF) for assessing multicollinearity
vif_values <- vif(Model_2)
# Create a table to store the results
diagnostic_table <- tibble(
  Test = c("Breusch-Pagan Test p-value", "Durbin-Watson Test p-value"),
  Value = c(bp_test$p.value, dw_test$p.value),
  Notes = c("Assessing homoscedasticity", "Assessing independence of residuals")
)
# Add VIF to the table
vif_table <- as_tibble(vif_values, rownames = "Variable")
colnames(vif_table) <- c("Test", "Value")
vif_table$Notes <- "Assessing multicollinearity"
# Combine tables
final_table <- bind_rows(diagnostic_table, vif_table)
# Print the final table
print(final_table)


## 5.6. Model 2b: Variables with no outliers were inclued-------------------------

# The structure of the model:
# DV: cWageZ_no_outliers

# IV: Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers + cHageZ_no_outliers

# Model 2b
Model_2b_no_outliers <- lm(cWageZ_no_outliers ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers + cHageZ_no_outliers, data = waz_final)

# View the summary of the model
summary(Model_2b_no_outliers)

# Perform Omnibus Test
anova(Model_2b_no_outliers)

# Get Confidence Intervals
confint(Model_2b_no_outliers)



## 5.7 Compare Model 1 and Model 2------------------------------------------------

# Models comparison

# Summary statistics for Model_1
summary_1 <- summary(Model_1)
print(paste("AIC for Model 1: ", round(AIC(Model_1), 4)))
print(paste("BIC for Model 1: ", round(BIC(Model_1), 4)))
print(paste("Adjusted R-squared for Model 1: ", round(summary_1$adj.r.squared, 4)))

# Extract p-values and round them to 4 digits
p_values_1 <- round(summary_1$coefficients[, "Pr(>|t|)"], 4)

# Summary statistics for Model_2
summary_2 <- summary(Model_2)
print(paste("AIC for Model 2: ", round(AIC(Model_2), 4)))
print(paste("BIC for Model 2: ", round(BIC(Model_2), 4)))
print(paste("Adjusted R-squared for Model 2: ", round(summary_2$adj.r.squared, 4)))

# Extract p-values and round them to 4 digits
p_values_2 <- round(summary_2$coefficients[, "Pr(>|t|)"], 4)

# ANOVA test between Model 1 and Model 2
anova_result <- anova(Model_1, Model_2)
anova_p_value <- round(anova_result$"Pr(>F)"[2], 4)
print(paste("ANOVA p-value comparison between Model 1 and Model 2: ", anova_p_value))


# Create a table with the output

# Summary statistics for Model_1
summary_1 <- summary(Model_1)
aic_1 <- round(AIC(Model_1), 4)
bic_1 <- round(BIC(Model_1), 4)
adj_r2_1 <- round(summary_1$adj.r.squared, 4)

# Summary statistics for Model_2
summary_2 <- summary(Model_2)
aic_2 <- round(AIC(Model_2), 4)
bic_2 <- round(BIC(Model_2), 4)
adj_r2_2 <- round(summary_2$adj.r.squared, 4)

# ANOVA test between Model 1 and Model 2
anova_result <- anova(Model_1, Model_2)
anova_p_value <- round(anova_result$"Pr(>F)"[2], 4)

# Create a data frame to hold the metrics
model_comparison <- data.frame(
  Metric = c("AIC", "BIC", "Adjusted R-squared", "ANOVA p-value"),
  Model_1 = c(aic_1, bic_1, adj_r2_1, "N/A"),
  Model_2 = c(aic_2, bic_2, adj_r2_2, anova_p_value)
)

# Display the table
print(model_comparison)


## 5.8. Model 3: Only statistical significant variables from Model 2 included-----

# The structure of the model
# DV: cWageZ
# IV: cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ

# Model 3
Model_3 <- lm(cWageZ ~ cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ, data = waz_final)

# View the summary of the model
summary(Model_3)

# Perform Omnibus Test
anova(Model_3)

# Get Confidence Intervals
confint(Model_3)


# 5.10. Model 3b: Only statistical significant variables with no outliers from Model 2 included------

# The structure of the model
# DV: cWageZ_no_outliers
# IV:cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ_no_outliers

# Model 3b
Model_3b_no_outliers <- lm(cWageZ_no_outliers ~ cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ_no_outliers, data = waz_final)

# View the summary of the model
summary(Model_3b_no_outliers)

# Perform Omnibus Test
anova(Model_3b_no_outliers)

# Get Confidence Intervals
confint(Model_3b_no_outliers)


## 5.9. Comprehensive Regression Models for Analyzing Wage Determinants: Including and Excluding Outliers------------

# Model 1
Model_1 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge, data = waz_final)

# Model 1b without outliers
Model_1b_no_outliers <- lm(cWageZ_no_outliers ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers, data = waz_final)

# Model 2
Model_2 <- lm(cWageZ ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge + cHageZ, data = waz_final)

# Model 2b without outliers
Model_2b_no_outliers <- lm(cWageZ_no_outliers ~ Location + cBirthT + cAgeM3c + mMarital2 + Anemia2 + mSES3c + cMDD + cRank3c + mAge_no_outliers + cHageZ_no_outliers, data = waz_final)

# Model 3
Model_3 <- lm(cWageZ ~ cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ, data = waz_final)

# Model 3b without outliers
Model_3b_no_outliers <- lm(cWageZ_no_outliers ~ cBirthT + cAgeM3c + Anemia2 + mSES3c + cHageZ_no_outliers, data = waz_final)


# Calculate adjusted R-squared, AIC, and BIC
summary_stats <- data.frame(
  Model = c("Model_1", "Model_1b_no_outliers", "Model_2", "Model_2b_no_outliers", "Model_3", "Model_3b_no_outliers"),
  Adj_R2 = c(
    summary(Model_1)$adj.r.squared,
    summary(Model_1b_no_outliers)$adj.r.squared,
    summary(Model_2)$adj.r.squared,
    summary(Model_2b_no_outliers)$adj.r.squared,
    summary(Model_3)$adj.r.squared,
    summary(Model_3b_no_outliers)$adj.r.squared
  ),
  AIC = c(
    AIC(Model_1),
    AIC(Model_1b_no_outliers),
    AIC(Model_2),
    AIC(Model_2b_no_outliers),
    AIC(Model_3),
    AIC(Model_3b_no_outliers)
  ),
  BIC = c(
    BIC(Model_1),
    BIC(Model_1b_no_outliers),
    BIC(Model_2),
    BIC(Model_2b_no_outliers),
    BIC(Model_3),
    BIC(Model_3b_no_outliers)
  )
)

# ANOVA test (only for nested models)
anova_test_1_2 <- anova(Model_1, Model_2)
anova_test_1_3 <- anova(Model_1, Model_3)

# Display the tables
print("Summary Statistics")
print(summary_stats)
print("ANOVA for Model 1 and Model 2")
print(anova_test_1_2)
print("ANOVA for Model 1 and Model 3")
print(anova_test_1_3)



# 6. Reporting and Interpreting the Outcomes-------------------------------------

# The report of the outcomes and interpretation will be completed in a different document.

#################################################################################