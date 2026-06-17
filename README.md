# India Crime Analytics Platform v2.1

Enterprise-grade R Shiny dashboard for crime intelligence.
Covers 29 Indian cities, 2020-2024 data, with ML forecasting to 2030.

## Quick Start (3 steps)

### Step 1 - Install R
Download from https://cran.r-project.org/

### Step 2 - Install packages
Open R or RStudio and run:
```r
source("INSTALL_PACKAGES.R")
```

### Step 3 - Launch app
```r
shiny::runApp(".")
```
App opens at http://localhost:3838

---

## Windows Users

If you see encoding errors, make sure to:
1. Open R/RStudio with Administrator rights (first time only)
2. Run `source("INSTALL_PACKAGES.R")` before launching
3. Use `shiny::runApp(".")` NOT `source("app.R")` directly from RStudio

---

## Project Structure

```
crime_analytics/
|-- app.R                    Entry point (run this)
|-- global.R                 Package loading + module sourcing
|-- ui.R                     Dark-theme dashboard UI
|-- server.R                 All server logic (11 pages)
|-- INSTALL_PACKAGES.R       Run once to install dependencies
|-- DESCRIPTION              Package metadata
|
|-- R/
|   |-- 00_utils.R           Helpers, metrics, city/state maps
|   |-- 01_data_engineering.R  Load, clean, aggregate CSV data
|   |-- 02_feature_engineering.R  39 engineered features
|   |-- 03_modeling.R        LR, RF, XGB-proxy, ETS, ARIMA
|   |-- 04_explainability.R  SHAP, anomaly detection, AI insights
|   |-- 05_visualizations.R  All plotly chart builders
|   |-- 06_reports.R         Excel (7 sheets) + HTML export
|   |-- 07_statistical_tests.R  15 hypothesis tests
|   `-- 08_stats_report.R    Stats Excel workbook generator
|
|-- data/
|   `-- crime_dataset_india.csv  (40,160 records)
|
|-- www/
|   |-- css/theme.css        Supplemental CSS
|   `-- js/dashboard.js      Nav routing + animations
|
`-- reports/                 Auto-generated on first run
```

---

## Dashboard Pages (11 total)

| Page | Description |
|------|-------------|
| Executive Overview | KPIs, national trend, category donut, city ranking |
| India Crime Map | City x Year heatmap, risk tier split |
| Forecast Center | City ETS forecast 2025-2030 with CI bands |
| State Intelligence | State ranking, risk scores, full data table |
| Crime Categories | Violent, Women, Cyber trends + weapon chart |
| AI Insights | 6 AI cards, anomaly detection, trend explanations |
| Model Evaluation | LR vs RF vs XGB comparison, actual vs predicted |
| Explainable AI | Feature importance, SHAP permutation chart |
| Statistical Tests | 5 T-tests, 5 Chi-square, 5 ANOVA + Tukey HSD |
| Report Center | Excel, HTML, CSV download |
| Raw Data | Filterable city/state/raw/yearly tables |

---

## Key Results

- Records analysed: 40,160
- Cities: 29 major Indian cities
- Engineered features: 39 per city-year
- Production model: Random Forest (R^2 = 0.986)
- Forecast horizon: 2025-2030 (6 years, 174 city forecasts)
- Statistical tests: 15 (7 significant at p < 0.05)

---
