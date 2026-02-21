library(dplyr)

# Load the "no overlap" dataset
df <- read.csv("C:/Users/dhrit/Downloads/education MMSA_without year and location_Less than 100_no overlap - 2-_2011_2023_merged_complete_cases.csv")

# Clean column names
df_clean <- df %>%
  rename_with(~ gsub("Estimate..EDUCATIONAL.ATTAINMENT..", "", .x)) %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  mutate(
    # Model target: 0 to 1 proportion
    Awareness_Prop = Hyp_Data_value / 100,
    # Row ID for the train/test split
    row_id = row_number()
  )
set.seed(123) # For reproducibility

# Training set: 80%
train_data <- df_clean %>% sample_frac(0.8)

# Testing set: Remaining 20%
test_data <- df_clean %>% anti_join(train_data, by = "row_id")

cat("Training rows:", nrow(train_data), "| Testing rows:", nrow(test_data))

# Fit the model using all available education categories
# We weight by population because larger cities provide more reliable survey data
quasi_model <- glm(
  Awareness_Prop ~ Less_than_high_school_diploma + 
    High_school_graduate__includes_equivalency_ + 
    Some_college_or_associate_s_degree + 
    Bachelor_s_degree,
  data = train_data,
  family = quasibinomial(link = "logit"),
  weights = Population_25_years_and_over
)

# View coefficients and statistical significance
summary(quasi_model)

# 1. Generate predictions and calculate error
results <- test_data %>%
  mutate(
    # Predict and scale back to 0-100
    Pred_Awareness = predict(quasi_model, newdata = ., type = "response") * 100,
    Error = Hyp_Data_value - Pred_Awareness
  )

# 2. Calculate Evaluation Metrics
metrics <- results %>%
  summarise(
    MAE = mean(abs(Error)),
    RMSE = sqrt(mean(Error^2))
  )

print("Evaluation Metrics (Test Set):")
print(metrics)

# Compare Actual Awareness vs. Model Predictions
#plot(results$Hyp_Data_value, results$Pred_Awareness,
 #    main = "Actual vs. Predicted Awareness (No Overlap Data)",
  #   xlab = "Actual Awareness (%)", 
   #  ylab = "Predicted Awareness (%)",
    # pch = 19, col = rgb(0.2, 0.5, 0.5, 0.6))

# Add a reference line (Perfect match line)
#abline(0, 1, col = "red", lwd = 2, lty = 2)

####ZCTA TRAINING 


# 1. Load the transformed prediction data
zcta_prediction <- read.csv("C:/Users/dhrit/Downloads/ZCTA_Education_Transformed_for_Prediction_Standard Column Names.csv")

# 2. Generate predictions (using the 'quasi_model' from the previous step)
# This will calculate the predicted 'Lack of Awareness' (unawareness) for each Zip Code
prediction_final <- zcta_prediction %>%
  mutate(
    # type = "response" returns the probability (0.0 to 1.0)
    # We multiply by 100 to return to the percentage scale
    Predicted_Unawareness = predict(quasi_model, newdata = ., type = "response") * 100,
    
    # Calculate Positive Awareness (100 - Unawareness)
    Predicted_Awareness = 100 - Predicted_Unawareness
  )

# 3. View the results
head(prediction_final %>% select(ZIP_Code, Predicted_Awareness, Predicted_Unawareness))

# 4. Save your results
write.csv(prediction_final, "C:/Users/dhrit/Downloads/ZCTA_Hypertension_Awareness_Predictions.csv", row.names = FALSE)