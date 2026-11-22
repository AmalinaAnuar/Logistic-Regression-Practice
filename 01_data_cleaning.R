# ============================================================================
# Phase 1: Data Cleaning & Preparation
# ============================================================================

library(haven)
library(tidyverse)
library(janitor)

# --- 1.0 Read data ---
stroke_raw <- read_dta("stroke-fatality-cooked2.dta")
cat("Raw dimensions:", dim(stroke_raw), "\n")

# --- 1.1 Fix variable names ---
stroke_df <- stroke_raw %>% clean_names()
cat("\nOriginal vs cleaned variable names:\n")
tibble(original = names(stroke_raw), cleaned = names(stroke_df)) %>% print(n = Inf)

# --- 1.2 Apply value labels and convert variable types ---
stroke_df <- stroke_df %>%
  mutate(
    # Categorical variables as factors
    sex            = factor(sex, levels = c(1, 2), labels = c("male", "female")),
    outcome        = factor(outcome, levels = c(0, 1), labels = c("alive", "dead")),
    hypertension   = factor(hypertension, levels = c(0, 1), labels = c("no", "yes")),
    maritalstat    = factor(maritalstat, levels = c(0, 1), labels = c("no", "yes")),
    malay          = factor(malay, levels = c(0, 1), labels = c("no", "yes")),
    diabetes       = factor(diabetes, levels = c(0, 1), labels = c("no", "yes")),
    dyslipidaemia  = factor(dyslipidaemia, levels = c(0, 1), labels = c("no", "yes")),
    referralcase   = factor(referralcase, levels = c(0, 1), labels = c("hospital", "GP/Home/Missing")),
    stroketype     = factor(stroketype, levels = c(0, 1), labels = c("Ischaemic", "Haemorrhagic")),
    # Numerical variables: ensure numeric (strip haven labels)
    gcs   = as.numeric(gcs),
    sbp   = as.numeric(sbp),
    dbp   = as.numeric(dbp),
    hr    = as.numeric(hr),
    hb    = as.numeric(hb),
    plat  = as.numeric(plat),
    wbc   = as.numeric(wbc),
    na    = as.numeric(na),
    potas = as.numeric(potas),
    cbs   = as.numeric(cbs),
    urea  = as.numeric(urea),
    age   = as.numeric(age)
  )

cat("\nVariable structure after cleaning:\n")
glimpse(stroke_df)

# --- 1.3 Check for missing data ---
missing_report <- stroke_df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(stroke_df) * 100, 1)) %>%
  arrange(desc(n_missing))

cat("\n========== MISSING DATA REPORT ==========\n")
print(missing_report, n = Inf)

# --- 1.4 Check for possible data entry errors ---
numerical_vars <- c("gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc", "na", "potas", "cbs", "urea", "age")

plausible_ranges <- tribble(
  ~variable, ~min_val, ~max_val,
  "age",       0,    120,
  "gcs",       3,     15,
  "sbp",      50,    300,
  "dbp",      20,    200,
  "hr",       20,    250,
  "hb",        2,     25,
  "plat",     10,   1000,
  "wbc",       1,     50,
  "na",      100,    170,
  "potas",   1.5,    8.0,
  "cbs",       1,     50,
  "urea",    0.5,     50
)

cat("\n========== DATA ENTRY ERROR CHECK ==========\n")
for (i in seq_len(nrow(plausible_ranges))) {
  var <- plausible_ranges$variable[i]
  lo  <- plausible_ranges$min_val[i]
  hi  <- plausible_ranges$max_val[i]
  vals <- stroke_df[[var]]
  flagged <- which(!is.na(vals) & (vals < lo | vals > hi))
  if (length(flagged) > 0) {
    cat(sprintf("  WARNING: %s has %d value(s) outside [%s, %s]: rows %s (values: %s)\n",
                var, length(flagged), lo, hi,
                paste(flagged, collapse = ", "),
                paste(round(vals[flagged], 2), collapse = ", ")))
  } else {
    cat(sprintf("  OK: %s — all values within [%s, %s]\n", var, lo, hi))
  }
}

# Check categorical variables for unexpected levels
cat("\n========== CATEGORICAL VARIABLE LEVELS ==========\n")
cat_vars <- c("sex", "outcome", "hypertension", "maritalstat", "malay",
              "diabetes", "dyslipidaemia", "referralcase", "stroketype")
for (v in cat_vars) {
  cat(sprintf("  %s: %s (NAs: %d)\n", v,
              paste(levels(stroke_df[[v]]), collapse = ", "),
              sum(is.na(stroke_df[[v]]))))
}

# Summary statistics for numerical variables
cat("\n========== NUMERICAL VARIABLE SUMMARY ==========\n")
stroke_df %>%
  select(all_of(numerical_vars)) %>%
  summary() %>%
  print()

# Save cleaned data for subsequent scripts
saveRDS(stroke_df, "stroke_cleaned.rds")
cat("\nCleaned data saved to stroke_cleaned.rds\n")
