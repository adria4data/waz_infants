
# Predictors of Weight in Romanian Infants: Weight-for-age z-scores (WAZ)

# Adrian Rus (2023)



# 1. Research Hypotheses/Questions/Objectives------------------------------------

# The objective of the present study was to explore the relationship between infants’ weight and various demographics 
# (i.e., infants’ mother location, marital status, social economic status, and age; and infants being born on term, age, anemia status, rank into the family and dietary patterns).
# Identifying the connection between such factors could help practitioners in developing public health policies and interventions in Romania.



# 2. Data Exploration------------------------------------------------------------

# 2.1. Setting Up the Environment: R Packages Installation----------------------
install.packages("tidyverse")
install.packages("moments")
install.packages(c("lmtest", "car", "carData"))

library(tidyverse)
library (moments)
library(lmtest)
library(car)
library(carData)


# 2.2. Setting the Working Directory------------------------------------------

# Check the working directory
getwd()

# For setting a new working directory use the setwd()
# setwd("C:\\Users\\adria\\Documents\\waz_infants")

# List Files in Directory To see a list of all files in the current working directory, you can use
list.files()


# 2.3. Import and check the dataset-----------------------------------------

library(readr)
waz_final <- read_csv("data/waz_final.csv")
View(waz_final)

# 2.3.1. Basic inspection of dataset

# View the first 6 rows
head(waz_final)

# View the last 6 rows
tail(waz_final)

# View the structure of dataset
str(waz_final)

# Summarize the dataset
summary(waz_final)


# 2.3.2. Check the tidiness of the dataset

# Checking the missing values. 
# Tidy data should have a consistent structure, but it can contain missing values.
sum(is.na(waz_final))

# Tidy data should ideally not have duplicated rows unless the repetition is meaningful.        
sum(duplicated(waz_final))

# Column names should be clear and descriptive but not too lengthy.
names(waz_final)

# 2.3.3. Check the data types and dimensionality
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

                                          
# 2.3.4. Specific variable content

# Shows unique values in a specific column.
unique(waz_final$mAge2c)

# Allows to identify the unique data in all columns
unique_values_all_columns <- lapply(waz_final, unique)
str(unique_values_all_columns)


# 3. Dependent Variable (DV) and Data Processing---------------------------------

# Dependent variable (DV): cWageZ (weight-for-age z-scores; WAZ)
# Independent variable (IV):

# 3.1. Visual check of the DV---------------------------------------------------


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


# 3.2. Compute central tendency, dispersion, skewness, kurtosis, and normality (Shapiro-Wilk P-value)-----------

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


# 3.3. Other statistics used for identifying outliers---------------------------

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

# 4.1. Visual check of the IV (for numerical Variables)--------------------------

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


# 4.2. Compute central tendency, dispersion, skewness, kurtosis, and normality (Shapiro-Wilk P-value)-----

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



# 5. Statistical Testing---------------------------------------------------------

# 6. Reporting and Interpreting the Outcomes-------------------------------------

