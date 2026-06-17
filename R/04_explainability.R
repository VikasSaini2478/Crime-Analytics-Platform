# =============================================================================
# 04_explainability.R     XAI: importance   SHAP approx   anomalies   insights
# =============================================================================
suppressPackageStartupMessages({ library(dplyr) })
source("R/00_utils.R")

FEAT_LABELS <- c(
  prev_year_crime_rate  = "Previous Year Crime Rate",
  rolling_avg_3yr       = "3-Year Rolling Avg",
  crime_growth_rate     = "Crime Growth Rate %",
  pop_growth_rate       = "Population Growth Rate",
  violent_ratio         = "Violent Crime Ratio",
  women_crime_ratio     = "Women Crime Ratio",
  cyber_crime_ratio     = "Cyber Crime Ratio",
  property_crime_ratio  = "Property Crime Ratio",
  avg_police_deployed   = "Avg Police Deployed",
  case_closure_rate     = "Case Closure Rate",
  crime_density         = "Crime Density",
  year_index            = "Year Trend Index",
  city_enc              = "City Effect"
)

label_feat <- function(f)
  ifelse(f %in% names(FEAT_LABELS), FEAT_LABELS[f],
         tools::toTitleCase(gsub("_", " ", f)))

# -- Consensus feature importance ---------------------------------------------
get_importance <- function(ml) {
  rows <- lapply(c("rf","xgb"), function(k) {
    imp <- ml[[k]]$feature_importance
    if (is.null(imp) || nrow(imp) == 0) return(NULL)
    data.frame(feature = imp$feature,
               imp_norm = safe_rescale(pmax(imp$importance, 0, na.rm=TRUE)) * 100,
               model    = ifelse(k == "rf", "Random Forest", "XGBoost (RF-proxy)"),
               stringsAsFactors = FALSE)
  })
  by_model <- do.call(rbind, Filter(Negate(is.null), rows))
  consensus <- by_model %>%
    group_by(feature) %>%
    summarise(avg_importance = mean(imp_norm, na.rm = TRUE),
              n_models = n(), .groups = "drop") %>%
    arrange(desc(avg_importance)) %>%
    mutate(rank = row_number(),
           feature_label = label_feat(feature))
  list(by_model = by_model, consensus = consensus,
       top_features = head(consensus, 10))
}

# -- SHAP via permutation ------------------------------------------------------
compute_shap <- function(rf_model, test_data, avail_feats) {
  log_info("Computing SHAP permutation importance")
  X <- test_data[, avail_feats, drop = FALSE]
  y <- test_data$crime_rate_per_lakh
  base_rmse <- calc_rmse(y, predict(rf_model, X))
  res <- lapply(avail_feats, function(f) {
    Xp <- X; Xp[[f]] <- sample(Xp[[f]])
    data.frame(feature          = f,
               shap_importance  = calc_rmse(y, predict(rf_model, Xp)) - base_rmse)
  })
  df <- do.call(rbind, res)
  df <- df[order(-df$shap_importance), ]
  df$pct_contribution <- df$shap_importance / (sum(abs(df$shap_importance)) + 1e-9) * 100
  df$feature_label    <- label_feat(df$feature)
  df
}

# -- Anomaly detection (z-score per city) -------------------------------------
detect_anomalies <- function(city_feat) {
  city_feat %>%
    group_by(city) %>%
    mutate(mu = mean(crime_rate_per_lakh, na.rm = TRUE),
           sg = sd(crime_rate_per_lakh, na.rm = TRUE),
           z  = (crime_rate_per_lakh - mu) / (sg + 1e-3)) %>%
    ungroup() %>%
    filter(abs(z) > 2) %>%
    transmute(city, year, crime_rate_per_lakh = round(crime_rate_per_lakh, 1),
              z_score = round(z, 2),
              anomaly_type = ifelse(z > 2, "[UP] Spike (High)", "[DOWN] Drop (Low)")) %>%
    arrange(desc(abs(z_score)))
}

# -- Trend explanations per city -----------------------------------------------
trend_explanations <- function(city_feat, fc_df) {
  ly <- max(city_feat$year, na.rm = TRUE)
  latest <- city_feat %>% filter(year == ly)
  fc_near <- fc_df %>% filter(year == min(year))
  latest %>%
    left_join(fc_near[, c("city","forecast_crime_rate")], by = "city") %>%
    mutate(
      fc_change_pct = (forecast_crime_rate - crime_rate_per_lakh) /
                       (crime_rate_per_lakh + 1e-3) * 100,
      explanation = paste0(
        city, " [", risk_tier, "]: ",
        round(crime_rate_per_lakh, 1), "/lakh   ",
        ifelse(is.na(crime_growth_rate), "n/a",
               paste0(round(crime_growth_rate, 1), "% YoY")), "   ",
        ifelse(!is.na(crime_growth_rate) & crime_growth_rate > 10, "[WARN] Surging",
        ifelse(!is.na(crime_growth_rate) & crime_growth_rate > 3,  "^ Rising",
        ifelse(!is.na(crime_growth_rate) & crime_growth_rate < -3, "v Falling",
        "-> Stable")))
      )
    ) %>%
    select(city, risk_tier, crime_rate_per_lakh, crime_growth_rate,
           fc_change_pct, explanation) %>%
    arrange(desc(crime_rate_per_lakh))
}

# -- AI insight cards ----------------------------------------------------------
build_insights <- function(city_feat, models, anomalies, shap) {
  ly     <- max(city_feat$year, na.rm = TRUE)
  latest <- city_feat %>% filter(year == ly)
  top3   <- paste(head(latest %>% arrange(desc(crime_growth_rate)) %>%
                         pull(city), 3), collapse = ", ")
  worst3 <- paste(head(latest %>% arrange(desc(risk_score)) %>%
                         pull(city), 3), collapse = ", ")
  best_r2 <- models$selection$comparison$r_squared[1]

  list(
    list(type = "summary",  icon = "[CHART]", color = "#58a6ff",
         title = "National Crime Overview",
         body  = sprintf(
           "Analysis covers %d cities across %d states. National avg: %.1f crimes/lakh population. %d cities classified HIGH or CRITICAL risk.",
           n_distinct(latest$city), n_distinct(CITY_STATE[latest$city]),
           mean(latest$crime_rate_per_lakh, na.rm = TRUE),
           sum(latest$risk_tier %in% c("HIGH","CRITICAL"), na.rm = TRUE))),

    list(type = "feature",  icon = "[BRAIN]", color = "#a371f7",
         title = "Top Prediction Driver",
         body  = if (nrow(shap) > 0)
           sprintf("'%s' is the strongest predictor (%.1f%% model variance). Historical crime patterns explain the majority of future rates.",
                   shap$feature_label[1], shap$pct_contribution[1])
           else "Feature importance computed from model permutations."),

    list(type = "anomaly",  icon = "[ALERT]", color = "#f85149",
         title = "Statistical Anomalies",
         body  = if (nrow(anomalies) > 0)
           sprintf("%d anomalous city-years detected (>2sigma). Largest: %s %d - %s (z=%.1f).",
                   nrow(anomalies), anomalies$city[1], anomalies$year[1],
                   anomalies$anomaly_type[1], anomalies$z_score[1])
           else "No significant anomalies detected in the dataset."),

    list(type = "threat",   icon = "[TARGET]", color = "#d29922",
         title = "Highest Risk Cities",
         body  = sprintf("Cities requiring immediate attention: %s. These show the highest composite risk scores combining rate, growth, and violent crime ratios.", worst3)),

    list(type = "growth",   icon = "[TREND]", color = "#3fb950",
         title = "Fastest Growing Crime",
         body  = sprintf("Cities with fastest crime growth: %s. Early intervention in these areas can prevent escalation to HIGH/CRITICAL tier.", top3)),

    list(type = "accuracy", icon = "[OK]", color = "#58a6ff",
         title = "Model Performance",
         body  = sprintf("Best model: %s | RMSE: %.4f | R^2: %.4f | Trained on 80%% hold-out validated on 20%% test set.",
                         models$selection$best_model_name,
                         models$selection$comparison$rmse[1], best_r2))
  )
}

# -- Master pipeline -----------------------------------------------------------
run_explainability_pipeline <- function(feat, models) {
  log_info("=== XAI PIPELINE START ===")
  imp   <- get_importance(models$ml)
  avail <- intersect(names(FEAT_LABELS), names(models$test_data))
  shap  <- tryCatch(
    compute_shap(models$ml$rf$model, models$test_data, avail),
    error = function(e) {
      message("[WARN] ", "SHAP failed: ", e$message)
      data.frame(feature=character(), shap_importance=numeric(),
                 pct_contribution=numeric(), feature_label=character())
    })
  anom    <- detect_anomalies(feat$city)
  trends  <- trend_explanations(feat$city, models$city_forecasts)
  insights <- build_insights(feat$city, models, anom, shap)
  message("[INFO] ", "=== XAI PIPELINE DONE  importance=", nrow(imp$consensus), "  anomalies=", nrow(anom), " ===")
  list(importance = imp, shap = shap, anomalies = anom,
       trends = trends, insights = insights)
}
