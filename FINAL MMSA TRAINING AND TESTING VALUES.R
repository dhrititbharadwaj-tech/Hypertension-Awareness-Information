#FINAL MMSA Q GLM MODEL BUILD
# 1. Load libraries
library(dplyr)
library(ggplot2)

# 2. Read the dataset
final_mmsa_data <- read.csv("C:/Users/dhrit/Downloads/FINAL MMSA - 2-_2011_2023_merged_complete_cases.csv", check.names = FALSE)

final_mmsa_data_clean <- final_mmsa_data %>%
  rename(
    Total_Pop         = `Total population`,
    Age_25_34         = `25-34`,
    Age_35_44         = `35-44`,
    Age_45_54         = `45-54`,
    Age_55_64         = `55-64`,
    Age_65_74         = `65-74`,
    Age_75_plus       = `75+`,
    Edu_Less_HS       = `Less than high school diploma 25 years and up`,
    Edu_HS_Grad       = `High school graduate (includes equivalency)  25 years and up`,
    Edu_Some_College  = `Some college or associate's degree  25 years and up`,
    Edu_Bachelors     = `Bachelor's degree  25 years and up`,
    Labor_Force_PR    = `In labor force 16 and up as percentage of 16 and up population`,
    Unemployment_Rate = `Unemployed as percentage of people in labor force`,
    Uninsured_Rate    = `Noninstitutionalized uninsured as percent of the Civilian noninstitutionalized`,
    Unawareness_Perc  = `Hypertension unwareness as percent of the total population`
  ) %>%
  mutate(
    # Step A: Flip Unawareness to Awareness
    Awareness_Perc = 100 - Unawareness_Perc,
    # Step B: Convert to proportion (needed for GLM)
    Awareness_Prop = Awareness_Perc / 100,
    # Create a unique ID for splitting
    id = row_number()
  )
set.seed(42) # For reproducibility

# Randomly select 80% of rows for training
train_final_mmsa_data <- final_mmsa_data_clean %>% sample_frac(0.80)

# Use anti_join to put the remaining 20% into the test set
test_final_mmsa_data <- final_mmsa_data_clean %>% anti_join(train_final_mmsa_data, by = "id")

model <- glm(
  Awareness_Prop ~ 
    Age_25_34 + Age_45_54 + Age_55_64 + Age_65_74 + Age_75_plus +
    Edu_Less_HS + Edu_HS_Grad + Edu_Some_College +
    Labor_Force_PR + Unemployment_Rate + Uninsured_Rate,
  
  data = train_final_mmsa_data, 
  family = quasibinomial(link = "logit"), 
  weights = Total_Pop
)

# Generate predictions (returns 0-1 proportion)
train_final_mmsa_data$Pred_Awareness <- predict(model, newdata = train_final_mmsa_data, type = "response") * 100
test_final_mmsa_data$Pred_Awareness  <- predict(model, newdata = test_final_mmsa_data, type = "response") * 100

# Combine for plotting
plot_data <- bind_rows(
  train_final_mmsa_data %>% mutate(Dataset = "Training Set (80%)"),
  test_final_mmsa_data  %>% mutate(Dataset = "Testing Set (20%)")
)

ggplot(plot_data, aes(x = Awareness_Perc, y = Pred_Awareness, color = Dataset)) +
  geom_point(alpha = 0.6) +
  # The 45-degree line (Perfect Prediction Line)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~Dataset) +
  labs(
    title = "Model Accuracy: Actual vs. Predicted Unawareness",
    subtitle = "Red dashed line indicates a perfect 1-to-1 prediction",
    x = "Actual Unawareness (%)",
    y = "Predicted Unawareness (%)"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")