
library(readr)
waz_final <- read_csv("data/waz_final.csv")
View(waz_final)


library(tidyverse)
library (moments)
library(lmtest)
library(car)
library(carData)

# 5.1. Set the levels of IV-----------------------------------------------------
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
write.csv(waz_final, "waz_final_modified.csv", row.names = FALSE)



## 5.2. Model 0: The intercept-only model (null model)

library(tidyverse)

# Run the intercept-only model (null model)
Model_0 <- lm(cWageZ ~ 1, data = waz_final)
# View the summary of the model
summary(Model_0)