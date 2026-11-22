# AGENTS.md — Logistic Regression Analysis of Stroke Fatality

## Project Overview

**Objective:** Identify independent variables associated with stroke fatality (`outcome`) using logistic regression analysis on the `stroke-fatality-cooked2.dta` dataset.

**Dataset:** 226 observations, 22 variables (EpiData origin, Stata 118 format)

---

## Agent Roles

### 1. Data Engineer Agent
- **Responsibility:** Data import, cleaning, variable type correction, missing data handling
- **Tools:** R, `haven`, `tidyverse`, `janitor`
- **Deliverables:**
  - Clean dataset with proper variable types (factors, numeric)
  - Missing data report
  - Data dictionary with original and cleaned variable names

### 2. Exploratory Data Analysis (EDA) Agent
- **Responsibility:** Descriptive statistics, visualizations, outlier detection
- **Tools:** R, `tidyverse`, `ggplot2`, `gtsummary`
- **Deliverables:**
  - Histograms and box-plots for numerical variables
  - Bar charts for categorical variables
  - Descriptive summary table (Table 1) stratified by outcome
  - Outlier and data entry error report

### 3. Univariable Analysis Agent
- **Responsibility:** Run simple logistic regression for each independent variable against outcome
- **Tools:** R, `tidyverse`, `broom`, `gtsummary`
- **Deliverables:**
  - Univariable logistic regression results (log-odds, OR, 95% CI, p-value)
  - List of candidate variables for multivariable model (p < 0.25)

### 4. Multivariable Modelling Agent
- **Responsibility:** Build and refine the final multivariable logistic regression model
- **Tools:** R, `tidyverse`, `MASS`, `car`
- **Deliverables:**
  - Forward LR stepwise variable selection
  - Multicollinearity assessment (VIF, correlation matrix)
  - Interaction term exploration
  - Final multivariable model with log-odds, OR, 95% CI, p-value

### 5. Model Diagnostics Agent
- **Responsibility:** Assess model fit and identify influential observations
- **Tools:** R, `pROC`, `ResourceSelection`, `broom`, `LogisticDx` (or manual computation)
- **Deliverables:**
  - ROC curve and AUC
  - Hosmer-Lemeshow goodness-of-fit test
  - Classification accuracy table
  - Diagnostic plots: leverage, Δβ (delta-beta), Δχ² (delta chi-square), ΔD (delta deviance)

### 6. Reporting Agent
- **Responsibility:** Compile final publication-ready tables and summary
- **Tools:** R, `gtsummary`, `gt`, `flextable`
- **Deliverables:**
  - Combined table: univariable + multivariable results (log-odds, OR, 95% CI, p-value)
  - Model fitness summary table
  - Final interpretation and recommendations

---

## Workflow

```
Data Engineer → EDA Agent → Univariable Agent → Multivariable Agent → Diagnostics Agent → Reporting Agent
```

## Assumptions (as specified)

1. **Binomial distribution** for the outcome variable
2. **Linearity in the logit** for continuous predictors
3. **Models are correctly specified**
