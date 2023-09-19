
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


# 3. Dependent Variable (DV) and Data Processing---------------------------------

# 4. Independent Variables (IV) and Data Processing------------------------------

# 5. Statistical Testing---------------------------------------------------------

# 6. Reporting and Interpreting the Outcomes-------------------------------------

