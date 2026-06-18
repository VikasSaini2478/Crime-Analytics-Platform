# Crime Analysis and Forecasting Dashboard

An interactive R Shiny dashboard for analyzing, visualizing, and forecasting crime trends across India using statistical methods and machine learning.

## Overview

The Crime Analysis and Forecasting Dashboard helps users explore historical crime data, identify patterns, generate insights, and predict future crime trends.

The application provides:

* Interactive visualizations of crime statistics
* Crime trend analysis across states and years
* Crime type classification and comparison
* Forecasting of future crime rates
* Statistical analysis and hypothesis testing
* Automated report generation

## Features

* Interactive dashboard built with R Shiny
* Data preprocessing and feature engineering pipelines
* Exploratory data analysis (EDA)
* Crime trend visualization
* Predictive modeling and forecasting
* Model explainability and interpretation
* Statistical testing and reporting
* Exportable reports in multiple formats

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

## Technologies Used

* R
* R Shiny
* ggplot2
* dplyr
* tidyr
* plotly
* caret
* forecast
* randomForest
* xgboost
* DT

## Installation

### Clone the Repository

```bash
git clone https://github.com/YOUR_USERNAME/crime-analysis-dashboard.git
cd crime-analysis-dashboard
```

### Install Dependencies

Open the project in RStudio and run:

```r
source("INSTALL_PACKAGES.R")
```

## Running the Application

Launch the dashboard using:

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

## Dataset

The project uses historical crime data collected from publicly available sources.

Example attributes include:

* State
* Year
* Crime Category
* Total Crimes
* Crime Rate
* Population

## Machine Learning Workflow

1. Data Collection
2. Data Cleaning and Preprocessing
3. Feature Engineering
4. Exploratory Data Analysis
5. Model Training
6. Model Evaluation
7. Forecast Generation
8. Report Creation

## Results

The dashboard enables users to:

* Identify crime hotspots
* Analyze year-over-year trends
* Compare crime categories
* Forecast future crime patterns
* Generate actionable insights

## Screenshots

## Dashboard <img width="1918" height="1030" alt="Screenshot 2026-06-13 223052" src="https://github.com/user-attachments/assets/2535af7d-9aff-409a-b982-a069ae0a5399" />

## Crime Map<img width="1917" height="1020" alt="Screenshot 2026-06-13 223122" src="https://github.com/user-attachments/assets/e0dafa2d-1e8a-47a3-979a-aeead5a14da6" />

## Forecast Center <img width="1912" height="1020" alt="Screenshot 2026-06-13 223207" src="https://github.com/user-attachments/assets/4fafb87c-a596-4c0a-8a25-48c1631b0751" />

## Model Evaluation<img width="1817" height="1028" alt="Screenshot 2026-06-13 223228" src="https://github.com/user-attachments/assets/c883a166-61dd-4597-92da-f9b4f3ae7d89" />

## Statistical Test<img width="1918" height="1022" alt="Screenshot 2026-06-13 223307" src="https://github.com/user-attachments/assets/c5a3d1af-3947-4593-a277-e78cecc5da61" />

## Future Enhancements

* Real-time data integration
* Geospatial crime mapping
* Deep learning models
* User authentication
* API integration
* Automated deployment

## Contributing

Contributions are welcome.

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Open a pull request

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

## License

This project is licensed under the MIT License.

## Author

Vikas Saini

GitHub: https://github.com/VikasSaini2478
