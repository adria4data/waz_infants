
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


# 2.3. Import and Visualize the dataset-----------------------------------------

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
<<<<<<< HEAD
# Dependent variable (DV): cWageZ (weight-for-age z-scores; WAZ)
=======
# Dependent variable (DV): 
>>>>>>> 9381d17a85dbd5c032faedaa85e51c6321ec6880
# Independent variable (IV):


# 4. Independent Variables (IV) and Data Processing------------------------------

# 5. Statistical Testing---------------------------------------------------------

# 6. Reporting and Interpreting the Outcomes-------------------------------------

