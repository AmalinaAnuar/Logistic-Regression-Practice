# ============================================================================
# Phase 2: Exploratory Data Analysis (EDA)
# ============================================================================

library(tidyverse)
library(gtsummary)
library(gt)

stroke_df <- readRDS("stroke_cleaned.rds")

numerical_vars <- c("gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc", "na", "potas", "cbs", "urea", "age")
cat_vars <- c("sex", "hypertension", "maritalstat", "malay", "diabetes", "dyslipidaemia", "referralcase", "stroketype")

# --- 2.1 Descriptive Statistics Table (Table 1) ---
table1 <- stroke_df %>%
  select(-rn) %>%
  tbl_summary(
    by = outcome,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      gcs ~ "GCS (Glasgow Coma Scale)",
      sbp ~ "Systolic BP (mmHg)",
      dbp ~ "Diastolic BP (mmHg)",
      hr  ~ "Heart Rate (bpm)",
      hb  ~ "Hemoglobin (g/dL)",
      plat ~ "Platelet (×10⁹/L)",
      wbc ~ "WBC (×10⁹/L)",
      na  ~ "Sodium (mmol/L)",
      potas ~ "Potassium (mmol/L)",
      cbs ~ "Capillary Blood Sugar (mmol/L)",
      urea ~ "Urea (mmol/L)",
      age ~ "Age (years)",
      sex ~ "Sex",
      hypertension ~ "Hypertension",
      maritalstat ~ "Married",
      malay ~ "Malay ethnicity",
      diabetes ~ "Diabetes",
      dyslipidaemia ~ "Dyslipidaemia",
      referralcase ~ "Referral source",
      stroketype ~ "Stroke type"
    )
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_p() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Outcome**")

table1 %>%
  as_gt() %>%
  gtsave("01_descriptive_table.html")
cat("Table 1 saved to 01_descriptive_table.html\n")

# --- 2.2 Histograms for numerical variables ---
hist_data <- stroke_df %>%
  select(all_of(numerical_vars), outcome) %>%
  pivot_longer(cols = all_of(numerical_vars), names_to = "variable", values_to = "value")

p_hist <- ggplot(hist_data, aes(x = value, fill = outcome)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 25) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("alive" = "#2196F3", "dead" = "#F44336")) +
  labs(title = "Histograms of Numerical Variables by Outcome",
       x = "Value", y = "Frequency", fill = "Outcome") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("02_eda_histograms.png", p_hist, width = 14, height = 12, dpi = 150)
cat("Histograms saved to 02_eda_histograms.png\n")

# --- 2.3 Box-plots for numerical variables ---
p_box <- ggplot(hist_data, aes(x = outcome, y = value, fill = outcome)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("alive" = "#2196F3", "dead" = "#F44336")) +
  labs(title = "Box-plots of Numerical Variables by Outcome",
       x = "Outcome", y = "Value", fill = "Outcome") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("03_eda_boxplots.png", p_box, width = 14, height = 12, dpi = 150)
cat("Box-plots saved to 03_eda_boxplots.png\n")

# --- 2.4 Bar charts for categorical variables ---
bar_data <- stroke_df %>%
  select(all_of(cat_vars), outcome) %>%
  pivot_longer(cols = all_of(cat_vars), names_to = "variable", values_to = "level")

p_bar <- ggplot(bar_data, aes(x = level, fill = outcome)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("alive" = "#2196F3", "dead" = "#F44336")) +
  labs(title = "Bar Charts of Categorical Variables by Outcome",
       x = "", y = "Count", fill = "Outcome") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("04_eda_barcharts.png", p_bar, width = 12, height = 12, dpi = 150)
cat("Bar charts saved to 04_eda_barcharts.png\n")

cat("\nPhase 2 EDA complete!\n")
