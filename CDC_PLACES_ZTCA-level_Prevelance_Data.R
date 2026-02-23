library(readr)
data <- read_csv("C:/Users/dhrit/Downloads/PLACES__Local_Data_for_Better_Health,_ZCTA_Data,_2025_release_20260213.csv")
library(dplyr)
ztca_data <- data %>%
  filter(Measure == "High blood pressure among adults",
         Data_Value_Type == "Crude prevalence")


