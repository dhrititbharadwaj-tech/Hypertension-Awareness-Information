library(readr)
data <- read_csv("C:/Users/dhrit/Downloads/Behavioral_Risk_Factors__Selected_Metropolitan_Area_Risk_Trends_(SMART)_MMSA_Prevalence_Data_(2011_to_Present)_20260213.csv")
library(dplyr)
df_brfss <- data %>%
  filter(Class == "Hypertension Awareness",
         Year == "2023")


