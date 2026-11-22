# ============================================================================
# Phase 4: Multivariable Logistic Regression
# Phase 5: Model Fitness
# Phase 6: Diagnostic Statistics
# Phase 7: Final Reporting
# ============================================================================

library(tidyverse)
library(broom)
library(gtsummary)
library(gt)
library(car)
library(MASS)
library(pROC)
library(ResourceSelection)

stroke_df <- readRDS("stroke_cleaned.rds")
univar_results <- readRDS("univar_results.rds")
selected_vars <- readRDS("selected_vars.rds")

stroke_df <- stroke_df %>%
  mutate(outcome_num = as.numeric(outcome) - 1)

cat("========================================\n")
cat("PHASE 4: MULTIVARIABLE LOGISTIC REGRESSION\n")
cat("========================================\n\n")

# --- 4.1 Multicollinearity check ---
cat("--- 4.1 Correlation matrix (continuous selected vars) ---\n")
cont_selected <- intersect(selected_vars,
  c("gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc", "na", "potas", "cbs", "urea", "age"))
cor_matrix <- cor(stroke_df[, cont_selected], use = "complete.obs")
cat("Correlation matrix:\n")
print(round(cor_matrix, 3))

# Flag high correlations
high_cor <- which(abs(cor_matrix) > 0.8 & cor_matrix != 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("\nWARNING: High correlations (|r| > 0.8):\n")
  for (i in seq_len(nrow(high_cor))) {
    cat(sprintf("  %s - %s: r = %.3f\n",
                rownames(cor_matrix)[high_cor[i,1]],
                colnames(cor_matrix)[high_cor[i,2]],
                cor_matrix[high_cor[i,1], high_cor[i,2]]))
  }
} else {
  cat("\nNo high correlations (|r| > 0.8) detected.\n")
}

# Preliminary model for VIF
prelim_formula <- as.formula(paste("outcome_num ~", paste(selected_vars, collapse = " + ")))
prelim_model <- glm(prelim_formula, data = stroke_df, family = binomial)

cat("\nVIF for preliminary model:\n")
vif_vals <- vif(prelim_model)
print(round(vif_vals, 3))
if (any(vif_vals > 5)) {
  cat("WARNING: Variables with VIF > 5 detected!\n")
} else {
  cat("No multicollinearity issues (all VIF < 5).\n")
}

# --- 4.2 Forward LR stepwise variable selection ---
cat("\n--- 4.2 Forward LR Stepwise Selection ---\n")

null_model <- glm(outcome_num ~ 1, data = stroke_df, family = binomial)
full_formula <- as.formula(paste("outcome_num ~", paste(selected_vars, collapse = " + ")))

forward_model <- step(null_model,
                      scope = list(lower = ~ 1, upper = full_formula),
                      direction = "forward",
                      trace = 1,
                      test = "LRT")

cat("\n--- Forward model summary ---\n")
summary(forward_model)

# --- 4.3 Interaction terms ---
cat("\n--- 4.3 Interaction term exploration ---\n")
final_vars <- names(coef(forward_model))[-1]  # exclude intercept
cat("Variables in forward model:", paste(final_vars, collapse = ", "), "\n\n")

# Test clinically plausible interactions
interactions_to_test <- list(
  c("gcs", "stroketype"),
  c("wbc", "stroketype"),
  c("hb", "stroketype"),
  c("gcs", "referralcase")
)

interaction_results <- list()
for (inter in interactions_to_test) {
  v1 <- inter[1]; v2 <- inter[2]
  # Only test if both variables are in the model
  if (all(c(v1, v2) %in% selected_vars)) {
    inter_formula <- update(formula(forward_model), as.formula(paste("~ . +", v1, ":", v2)))
    inter_model <- glm(inter_formula, data = stroke_df, family = binomial)
    lr_test <- anova(forward_model, inter_model, test = "Chisq")
    p_inter <- lr_test$`Pr(>Chi)`[2]
    cat(sprintf("  %s × %s: LRT p-value = %.4f %s\n",
                v1, v2, p_inter, ifelse(p_inter < 0.05, "*** SIGNIFICANT", "")))
    interaction_results[[paste(v1, v2, sep = ":")]] <- p_inter
  }
}

# Add significant interactions to model
sig_interactions <- names(interaction_results)[interaction_results < 0.05]
if (length(sig_interactions) > 0) {
  cat("\nAdding significant interactions:", paste(sig_interactions, collapse = ", "), "\n")
  final_formula <- update(formula(forward_model),
                          as.formula(paste("~ . +", paste(sig_interactions, collapse = " + "))))
  final_model <- glm(final_formula, data = stroke_df, family = binomial)
} else {
  cat("\nNo significant interactions found. Final model = forward model.\n")
  final_model <- forward_model
}

cat("\n--- 4.4 Final Multivariable Model ---\n")
print(summary(final_model))

cat("\nFinal model VIF:\n")
tryCatch({
  print(round(vif(final_model), 3))
}, error = function(e) cat("VIF not applicable (single predictor or interaction terms)\n"))

# ============================================================================
cat("\n========================================\n")
cat("PHASE 5: MODEL FITNESS\n")
cat("========================================\n\n")

# --- 5.1 ROC Curve and AUC ---
predicted_probs <- predict(final_model, type = "response")
roc_obj <- roc(stroke_df$outcome_num, predicted_probs, quiet = TRUE)
auc_val <- auc(roc_obj)

cat(sprintf("AUC: %.4f (95%% CI: %.4f - %.4f)\n",
            auc_val, ci.auc(roc_obj)[1], ci.auc(roc_obj)[3]))

png("08_roc_curve.png", width = 700, height = 600, res = 150)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"),
     col = "#2196F3", lwd = 2, legacy.axes = TRUE,
     print.auc = TRUE, print.auc.y = 0.3)
dev.off()
cat("ROC curve saved to 08_roc_curve.png\n")

# --- 5.2 Hosmer-Lemeshow GOF ---
hl_test <- hoslem.test(stroke_df$outcome_num, predicted_probs, g = 10)
cat(sprintf("\nHosmer-Lemeshow test: chi-sq = %.4f, df = %d, p = %.4f\n",
            hl_test$statistic, hl_test$parameter, hl_test$p.value))
if (hl_test$p.value > 0.05) {
  cat("Interpretation: Model fits adequately (p > 0.05, fail to reject H0)\n")
} else {
  cat("Interpretation: Poor model fit (p <= 0.05, reject H0)\n")
}

# --- 5.3 Classification Accuracy using broom ---
cat("\n--- 5.3 Classification Accuracy (using broom) ---\n")
augmented_df <- augment(final_model, type.predict = "response")
model_stats <- glance(final_model)

cat("\nModel-level statistics (broom::glance):\n")
print(model_stats)

augmented_df <- augmented_df %>%
  mutate(predicted_class = if_else(.fitted >= 0.5, 1, 0))

conf_matrix <- table(Predicted = augmented_df$predicted_class,
                     Actual = augmented_df$outcome_num)

cat("\nConfusion Matrix:\n")
print(conf_matrix)

accuracy    <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[,2])
specificity <- conf_matrix[1,1] / sum(conf_matrix[,1])

cat(sprintf("\nAccuracy:    %.4f (%.1f%%)\n", accuracy, accuracy * 100))
cat(sprintf("Sensitivity: %.4f (%.1f%%)\n", sensitivity, sensitivity * 100))
cat(sprintf("Specificity: %.4f (%.1f%%)\n", specificity, specificity * 100))

# ============================================================================
cat("\n========================================\n")
cat("PHASE 6: DIAGNOSTIC STATISTICS\n")
cat("========================================\n\n")

# --- 6.1 Compute diagnostic measures ---
n <- nrow(stroke_df)
p <- length(coef(final_model))

diag_df <- augmented_df %>%
  mutate(
    obs_id        = row_number(),
    leverage      = .hat,
    std_resid     = .std.resid,
    cooks_d       = .cooksd,
    # Pearson residual
    pearson_resid = (outcome_num - .fitted) / sqrt(.fitted * (1 - .fitted)),
    # Delta chi-square (change in Pearson chi-square)
    delta_chisq   = pearson_resid^2 / (1 - leverage),
    # Deviance residual
    dev_resid     = sign(outcome_num - .fitted) *
                    sqrt(-2 * (outcome_num * log(.fitted) + (1 - outcome_num) * log(1 - .fitted))),
    # Delta deviance
    delta_dev     = dev_resid^2 / (1 - leverage),
    # Delta beta (approximate)
    delta_beta    = cooks_d * p
  )

# --- 6.2 Flag problematic observations ---
leverage_cutoff <- 2 * p / n
cat(sprintf("Leverage cutoff (2p/n): %.4f\n", leverage_cutoff))

flagged <- diag_df %>%
  filter(leverage > leverage_cutoff | abs(delta_beta) > 1 |
         delta_chisq > 4 | delta_dev > 4)

cat(sprintf("Flagged observations: %d out of %d\n", nrow(flagged), n))

if (nrow(flagged) > 0) {
  cat("\nFlagged cases:\n")
  flagged %>%
    dplyr::select(obs_id, leverage, delta_beta, delta_chisq, delta_dev, .fitted, outcome_num) %>%
    mutate(across(where(is.numeric), ~ round(., 4))) %>%
    print(n = 30)
}

# --- 6.3 Diagnostic plots ---
png("09_diagnostic_plots.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Plot 1: Leverage vs predicted probability
plot(diag_df$.fitted, diag_df$leverage,
     xlab = "Predicted Probability", ylab = "Leverage (hat value)",
     main = "Leverage vs Predicted Probability",
     pch = 16, col = ifelse(diag_df$leverage > leverage_cutoff, "red", "steelblue"),
     cex = 0.8)
abline(h = leverage_cutoff, lty = 2, col = "red")
legend("topright", legend = paste("Cutoff =", round(leverage_cutoff, 3)),
       lty = 2, col = "red", cex = 0.7)

# Plot 2: Delta-beta vs predicted probability
plot(diag_df$.fitted, diag_df$delta_beta,
     xlab = "Predicted Probability", ylab = "Delta-beta (Δβ)",
     main = "Delta-beta vs Predicted Probability",
     pch = 16, col = ifelse(abs(diag_df$delta_beta) > 1, "red", "steelblue"),
     cex = 0.8)
abline(h = c(-1, 1), lty = 2, col = "red")

# Plot 3: Delta chi-square vs predicted probability
plot(diag_df$.fitted, diag_df$delta_chisq,
     xlab = "Predicted Probability", ylab = "Delta Chi-Square (Δχ²)",
     main = "Delta Chi-Square vs Predicted Probability",
     pch = 16, col = ifelse(diag_df$delta_chisq > 4, "red", "steelblue"),
     cex = 0.8)
abline(h = 4, lty = 2, col = "red")

# Plot 4: Delta deviance vs predicted probability
plot(diag_df$.fitted, diag_df$delta_dev,
     xlab = "Predicted Probability", ylab = "Delta Deviance (ΔD)",
     main = "Delta Deviance vs Predicted Probability",
     pch = 16, col = ifelse(diag_df$delta_dev > 4, "red", "steelblue"),
     cex = 0.8)
abline(h = 4, lty = 2, col = "red")

dev.off()
cat("Diagnostic plots saved to 09_diagnostic_plots.png\n")

# ============================================================================
cat("\n========================================\n")
cat("PHASE 7: FINAL REPORTING\n")
cat("========================================\n\n")

# --- 7.1 Combined univariable + multivariable table ---

# Independent variables for the table (all 20 IVs)
iv_vars <- c("sex", "gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc",
             "na", "potas", "cbs", "urea", "age",
             "hypertension", "maritalstat", "malay", "diabetes",
             "dyslipidaemia", "referralcase", "stroketype")

# Build univariable table rows from raw results
univar_table_data <- univar_results %>%
  mutate(
    uni_log_odds = sprintf("%.3f", log_odds),
    uni_OR       = sprintf("%.3f", OR),
    uni_CI       = sprintf("%.3f – %.3f", OR_lower, OR_upper),
    uni_p        = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  dplyr::select(variable, term, uni_log_odds, uni_OR, uni_CI, uni_p)

# Extract multivariable results
multi_tidy <- tidy(final_model, conf.int = TRUE, exponentiate = FALSE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    multi_log_odds = sprintf("%.3f", estimate),
    multi_OR       = sprintf("%.3f", exp(estimate)),
    multi_CI       = sprintf("%.3f – %.3f", exp(conf.low), exp(conf.high)),
    multi_p        = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  dplyr::select(term, multi_log_odds, multi_OR, multi_CI, multi_p)

# Merge
combined_table <- univar_table_data %>%
  left_join(multi_tidy, by = "term") %>%
  mutate(across(starts_with("multi_"), ~ replace_na(., "—")))

cat("--- Combined Results Table ---\n")
print(combined_table, n = Inf)

# Create GT table
combined_gt <- combined_table %>%
  dplyr::select(-term) %>%
  rename(
    Variable           = variable,
    `Log-odds (β)`     = uni_log_odds,
    `OR`               = uni_OR,
    `95% CI`           = uni_CI,
    `p-value`          = uni_p,
    `Log-odds (β) `    = multi_log_odds,
    `OR `              = multi_OR,
    `95% CI `          = multi_CI,
    `p-value `         = multi_p
  ) %>%
  gt() %>%
  tab_spanner(label = "Univariable", columns = c(`Log-odds (β)`, `OR`, `95% CI`, `p-value`)) %>%
  tab_spanner(label = "Multivariable", columns = c(`Log-odds (β) `, `OR `, `95% CI `, `p-value `)) %>%
  tab_header(title = "Logistic Regression Analysis of Stroke Fatality",
             subtitle = "Univariable and Multivariable Results") %>%
  tab_footnote(footnote = "OR = Odds Ratio; CI = Confidence Interval; β = log-odds coefficient") %>%
  tab_footnote(footnote = paste("Multivariable model variables selected via forward LR stepwise (p < 0.25 from univariable)")) %>%
  opt_stylize(style = 1) %>%
  tab_options(table.font.size = "small")

gtsave(combined_gt, "07_combined_table.html")
cat("Combined table saved to 07_combined_table.html\n")

# --- 7.2 Model fitness summary table ---
fitness_gt <- tibble(
  Metric = c("AUC", "AUC 95% CI", "Hosmer-Lemeshow χ²", "Hosmer-Lemeshow p-value",
             "Accuracy", "Sensitivity", "Specificity",
             "Null Deviance", "Residual Deviance", "AIC"),
  Value = c(sprintf("%.4f", auc_val),
            sprintf("%.4f – %.4f", ci.auc(roc_obj)[1], ci.auc(roc_obj)[3]),
            sprintf("%.4f", hl_test$statistic),
            sprintf("%.4f", hl_test$p.value),
            sprintf("%.1f%%", accuracy * 100),
            sprintf("%.1f%%", sensitivity * 100),
            sprintf("%.1f%%", specificity * 100),
            sprintf("%.2f", model_stats$null.deviance),
            sprintf("%.2f", model_stats$deviance),
            sprintf("%.2f", model_stats$AIC)),
  Interpretation = c(
    ifelse(auc_val > 0.9, "Excellent", ifelse(auc_val > 0.8, "Good", ifelse(auc_val > 0.7, "Acceptable", "Poor"))),
    "", 
    "", 
    ifelse(hl_test$p.value > 0.05, "Model fits adequately", "Poor fit"),
    "", "", "", "", "", ""
  )
) %>%
  gt() %>%
  tab_header(title = "Model Fitness Summary") %>%
  opt_stylize(style = 1)

gtsave(fitness_gt, "10_model_summary.html")
cat("Model fitness summary saved to 10_model_summary.html\n")

# --- Univariable-only gtsummary table ---
univar_gts <- tbl_uvregression(
  data = stroke_df %>% dplyr::select(outcome_num, all_of(iv_vars)),
  method = glm,
  y = outcome_num,
  method.args = list(family = binomial),
  exponentiate = TRUE
)

as_gt(univar_gts) %>% gtsave("05_univariable_results.html")
cat("Univariable results table saved to 05_univariable_results.html\n")

# --- Multivariable gtsummary table ---
multi_gts <- tbl_regression(final_model, exponentiate = TRUE)
as_gt(multi_gts) %>% gtsave("06_multivariable_results.html")
cat("Multivariable results table saved to 06_multivariable_results.html\n")

# Save final model
saveRDS(final_model, "final_model.rds")

cat("\n========================================\n")
cat("ALL PHASES COMPLETE!\n")
cat("========================================\n")
cat("\nOutput files:\n")
cat("  01_descriptive_table.html\n")
cat("  02_eda_histograms.png\n")
cat("  03_eda_boxplots.png\n")
cat("  04_eda_barcharts.png\n")
cat("  05_univariable_results.html\n")
cat("  06_multivariable_results.html\n")
cat("  07_combined_table.html\n")
cat("  08_roc_curve.png\n")
cat("  09_diagnostic_plots.png\n")
cat("  10_model_summary.html\n")
