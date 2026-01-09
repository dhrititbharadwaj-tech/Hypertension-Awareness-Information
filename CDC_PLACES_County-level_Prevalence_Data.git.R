library(readr)
#First, we want to upload the CDC PLACES data onto this repository:
data <- read_csv("C:/Users/dhrit/Downloads/PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260108.csv")
#Now we want to filter this dataset for only hypertension-related information, or more specifically, only cells
#under the column "Measure" with the content 'High blood pressure among adults"; specifically, we are looking for age-adjusted prevalence
#only, so we also add in another filter where "Data_Value_Type" is filtered for this.
library(dplyr)
high_bp_data <- data %>%
  filter(Measure == "High blood pressure among adults",
         Data_Value_Type == "Age-adjusted prevalence")


