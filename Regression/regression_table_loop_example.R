# Regression Tabel Loop Example 


library(modelsummary)
library(broom)
library(flextable)


# Load data 


# Base Models 

Base_models <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE),
  "Lawfullmness" = lm(Lawfullness ~ Treatment, data = ESSSE),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE),
  "Police Effectivnessivness" = lm(Effectivness ~ Treatment, data = ESSSE)
)


modelsummary(
  Base_models,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = 'Baseline Estimates',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')





tidy_models <- list()  #specify list() lenght 

# Loop through each model and convert coefficients to tidy data frame
for (model_name in names(Base_models)) {
  tidy_model <- tidy(Base_models[[model_name]], conf.int = TRUE) # Include confidence intervals to compute stars
  tidy_model <- cbind(Model = model_name, tidy_model) # Add a new column for Model names
  tidy_models[[model_name]] <- tidy_model
}

tidy_data <- do.call(rbind, tidy_models)
selected_columns <- c("Model", "term", "estimate", "std.error", "p.value")
tidy_data <- tidy_data[selected_columns]

add_significance_stars <- function(p_value) {
  stars <- ifelse(p_value < 0.001, "***",
                  ifelse(p_value < 0.01, "**",
                         ifelse(p_value < 0.05, "*", "")))
  paste0(round(tidy_data$estimate, 3), stars)
}

# Add significance stars to the estimate column in the tidy_data table
tidy_data$estimate <- add_significance_stars(tidy_data$p.value)
tidy_data$p.value <- NULL


(OLS_Naive_ESSSE <-  flextable(tidy_data))

