library(readr)
brfss <- read_csv("C:/Users/dhrit/Downloads/Behavioral_Risk_Factors__Selected_Metropolitan_Area_Risk_Trends_(SMART)_MMSA_Prevalence_Data_(2011_to_Present)_20260213.csv")
library(dplyr)
CountyAwareness <- brfss %>%
  filter(Class == "Hypertension Awareness",
         Response == "Yes")

#cut down to only columns you want (year, location, and hypertension awareness data value)

Only_CountyAwareness <- CountyAwareness[, c("Year", "Locationdesc", "Data_value")]



