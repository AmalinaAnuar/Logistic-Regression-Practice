# ============================================================================
# Phase 3: Univariable Logistic Regression
# ============================================================================

library(tidyverse)
library(broom)
library(gtsummary)

stroke_df <- readRDS("stroke_cleaned.rds")

# For logistic regression, outcome must be numeric 0/1
stroke_df <- stroke_df %>%
  mutate(outcome_num = as.numeric(outcome) - 1)  # alive=0, dead=1

# Independent variables (exclude rn and outcome)
iv_vars <- c("sex", "gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc",
             "na", "potas", "cbs", "urea", "age",
             "hypertension", "maritalstat", "malay", "diabetes",
             "dyslipidaemia", "referralcase", "stroketype")

# --- 3.1 & 3.2 Fit univariable logistic regression for each IV ---
univar_results <- map_dfr(iv_vars, function(var) {
  formula <- as.formula(paste("outcome_num ~", var))
  model <- glm(formula, data = stroke_df, family = binomial)
  
  tidy_res <- tidy(model, conf.int = TRUE, exponentiate = FALSE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      variable     = var,
      OR           = exp(estimate),
      OR_lower     = exp(conf.low),
      OR_upper     = exp(conf.high),
      log_odds     = estimate,
      log_odds_se  = std.error
    ) %>%
    select(variable, term, log_odds, log_odds_se, OR, OR_lower, OR_upper, p.value)
  
  return(tidy_res)
})

# --- 3.3 Display results and select variables p < 0.25 ---
cat("========== UNIVARIABLE LOGISTIC REGRESSION RESULTS ==========\n")
univar_results %>%
  mutate(across(c(log_odds, log_odds_se, OR, OR_lower, OR_upper), ~ round(., 4)),
         p.value = round(p.value, 4),
         selected = ifelse(p.value < 0.25, "YES", "")) %>%
  print(n = Inf)

selected_vars <- univar_results %>%
  filter(p.value < 0.25) %>%
  pull(variable) %>%
  unique()

cat("\n========== VARIABLES SELECTED (p < 0.25) ==========\n")
cat(paste(selected_vars, collapse = ", "), "\n")

# Save univariable results and selected variables
saveRDS(univar_results, "univar_results.rds")
saveRDS(selected_vars, "selected_vars.rds")

# Create univariable gtsummary table
univar_tables <- map(iv_vars, function(var) {
  formula <- as.formula(paste("outcome_num ~", var))
  model <- glm(formula, data = stroke_df, family = binomial)
  tbl_regression(model, exponentiate = TRUE,
                 show_single_row = if(is.factor(stroke_df[[var]]) && nlevels(stroke_df[[var]]) == 2) var else NULL)
})

# Save for later merging
saveRDS(univar_tables, "univar_tables_gtsummary.rds")
cat("\nPhase 3 complete!\n")
