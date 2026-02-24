# 1. Load Libraries
library(dplyr)
library(readr)
library(ggplot2)

# 2. Load Data
mmsa_df <- read_csv("C:/Users/dhrit/Downloads/FINAL MMSA - 2-_2011_2023_merged_complete_cases.csv")

# --- STEP 1: CLEANUP ---
# Target: Hypertension Unawareness (converted to 0-1)
# Features: All demographic percentages
train_ready <- mmsa_df %>%
  mutate(unawareness_prop = Hypertension_unwareness_as_percent_of_the_total_population / 100) %>%
  select(
    unawareness_prop,
    `25-34`, `35-44`, `45-54`, `55-64`, `65-74`, `75+`,
    Less_than_high_school_diploma_25_years_and_up,
    High_school_graduate_includes_equivalency_25_years_and_up,
    Some_college_or_associate_s_degree_25_years_and_up,
    Bachelor_s_degree_25_years_and_up,
    In_labor_force_16_and_up_as_percentage_of_16_and_up_population,
    Unemployed_as_percentage_of_people_in_labor_force,
    Noninstitutionalized_uninsured_as_percent_of_the_Civilian_noninstitutionalized
  ) %>%
  na.omit()

# --- STEP 2: SPLIT (80/20) ---
set.seed(42)
sample_size <- floor(0.80 * nrow(train_ready))
train_indices <- sample(seq_len(nrow(train_ready)), size = sample_size)

train_data <- train_ready[train_indices, ]
test_data  <- train_ready[-train_indices, ]

# --- STEP 3: TRAIN ---
unawareness_model <- glm(unawareness_prop ~ ., 
                         data = train_data, 
                         family = quasibinomial(link = "logit"))

# --- STEP 4: PREDICT & CALCULATE ACCURACY ---
# Predict on Test Set
test_preds <- predict(unawareness_model, newdata = test_data, type = "response")

# Calculate Mean Absolute Error (MAE)
test_mae <- mean(abs(test_preds - test_data$unawareness_prop))
cat("Testing Mean Absolute Error:", round(test_mae * 100, 2), "percentage points\n")

# --- STEP 5: PLOT TEST ACCURACY ---
# Create a dataframe for plotting
plot_data <- data.frame(
  Actual = test_data$unawareness_prop * 100,
  Predicted = test_preds * 100
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "#2c7fb8") +           # Scatter plot of results
  geom_abline(intercept = 0, slope = 1,                  # 45-degree reference line
              color = "#e31a1c", linetype = "dashed", size = 1) + 
  labs(
    title = "Test Set Accuracy: Actual vs. Predicted Unawareness",
    subtitle = paste("Mean Absolute Error:", round(test_mae * 100, 2), "percentage points"),
    x = "Actual Unawareness (%)",
    y = "Predicted Unawareness (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# --- STEP 1: LOAD ZCTA DATA ---
zcta_df <- read_csv("C:/Users/dhrit/Downloads/ZCTA_ Total Pop, Age, Edu, Empl, Uninsured (RIGHT) - ZCTA_Data_Percentages (1).csv")

# --- STEP 2: ALIGN ZCTA COLUMNS TO MATCH MODEL ---
# We must rename the ZCTA columns to match the underscores and names used in the MMSA model.
zcta_for_prediction <- zcta_df %>%
  select(
    `25-34`, `35-44`, `45-54`, `55-64`, `65-74`, `75+`,
    Less_than_high_school_diploma_25_years_and_up,
    High_school_graduate_includes_equivalency_25_years_and_up,
    Some_college_or_associate_s_degree_25_years_and_up,
    Bachelor_s_degree_25_years_and_up,
    In_labor_force_16_and_up_as_percentage_of_16_and_up_population,
    Unemployed_as_percentage_of_people_in_labor_force,
    Noninstitutionalized_uninsured_as_percent_of_the_Civilian_noninstitutionalized
  ) %>%
  # Convert all columns to numeric (handles any characters like '-' or missing values)
  mutate(across(everything(), ~as.numeric(as.character(.))))

# --- STEP 3: RUN PREDICTIONS ---
# We use the 'unawareness_model' from the training step.
# type = "response" gives us the probability (0 to 1).
zcta_preds <- predict(unawareness_model, newdata = zcta_for_prediction, type = "response")

# --- STEP 4: COMBINE AND EXPORT ---
# We add the predictions back to the original ZCTA dataframe
zcta_final <- zcta_df %>%
  mutate(
    Estimated_Unawareness_Prop = zcta_preds,
    Estimated_Unawareness_Pct = round(zcta_preds * 100, 2)
  )

# View the first few estimates
head(zcta_final %>% select(`Total_population`, Estimated_Unawareness_Pct))

# Save the final estimates to a CSV
write_csv(zcta_final, "C:/Users/dhrit/Downloads/ZCTA_Hypertension_Unawareness_Estimates.csv")

# --- STEP 5: VISUALIZE ESTIMATES (Optional) ---
ggplot(zcta_final, aes(x = Estimated_Unawareness_Pct)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  labs(
    title = "Distribution of Predicted Hypertension Unawareness across ZCTAs",
    x = "Predicted Unawareness (%)",
    y = "Count of ZIP Codes"
  ) +
  theme_minimal()