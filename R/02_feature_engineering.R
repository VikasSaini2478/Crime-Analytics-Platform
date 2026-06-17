# =============================================================================
# 02_feature_engineering.R
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(zoo) })
source("R/00_utils.R")

engineer_city_features <- function(city_agg) {
  log_info("Engineering city features")
  df <- city_agg %>%
    arrange(city, year) %>%
    group_by(city) %>%
    mutate(
      prev_year_crime_rate = lag(crime_rate_per_lakh, 1),
      prev_year_total      = lag(total_crimes, 1),
      rolling_avg_3yr      = rollapply(crime_rate_per_lakh, 3, mean, na.rm=TRUE, fill=NA, align="right"),
      rolling_avg_2yr      = rollapply(crime_rate_per_lakh, 2, mean, na.rm=TRUE, fill=NA, align="right"),
      crime_growth_rate    = (crime_rate_per_lakh - lag(crime_rate_per_lakh)) / (lag(crime_rate_per_lakh) + 1e-3) * 100,
      crime_growth_abs     = crime_rate_per_lakh - lag(crime_rate_per_lakh),
      pop_growth_rate      = (population - lag(population)) / (lag(population) + 1) * 100,
      crime_trend          = rollapply(crime_rate_per_lakh, 3,
                               function(x) if (sum(!is.na(x)) < 2) NA_real_ else coef(lm(x ~ seq_along(x)))[2],
                               fill=NA, align="right"),
      crime_density        = total_crimes / (population / 1e4),
      violent_ratio        = violent_crimes  / (total_crimes + 1),
      women_crime_ratio    = women_crimes    / (total_crimes + 1),
      cyber_crime_ratio    = cyber_crimes    / (total_crimes + 1),
      property_crime_ratio = property_crimes / (total_crimes + 1),
      vice_crime_ratio     = vice_crimes     / (total_crimes + 1),
      year_index           = year - min(year, na.rm=TRUE) + 1L,
      growth_acceleration  = crime_growth_rate - lag(crime_growth_rate)
    ) %>%
    ungroup() %>%
    mutate(
      n_rate    = safe_rescale(crime_rate_per_lakh),
      n_growth  = safe_rescale(pmax(crime_growth_rate, 0, na.rm=TRUE)),
      n_violent = safe_rescale(violent_ratio),
      n_women   = safe_rescale(women_crime_ratio),
      # score is already 0-100 after rescale*weights (weights sum to 100)
      risk_score = round(n_rate * 40 + n_growth * 25 + n_violent * 20 + n_women * 15, 1),
      risk_tier  = ifelse(risk_score >= 70, "CRITICAL",
                   ifelse(risk_score >= 50, "HIGH",
                   ifelse(risk_score >= 30, "MEDIUM", "LOW")))
    ) %>%
    select(-n_rate, -n_growth, -n_violent, -n_women)
  message("[INFO] ", "City features: ", ncol(df), " cols ", nrow(df), " rows")
  df
}

engineer_state_features <- function(state_agg) {
  state_agg %>%
    arrange(state, year) %>%
    group_by(state) %>%
    mutate(
      prev_year_crime_rate = lag(crime_rate_per_lakh, 1),
      rolling_avg_3yr      = rollapply(crime_rate_per_lakh, 3, mean, na.rm=TRUE, fill=NA, align="right"),
      crime_growth_rate    = (crime_rate_per_lakh - lag(crime_rate_per_lakh)) / (lag(crime_rate_per_lakh)+1e-3) * 100,
      violent_ratio        = violent_crimes / (total_crimes + 1),
      women_crime_ratio    = women_crimes   / (total_crimes + 1),
      year_index           = year - min(year, na.rm=TRUE) + 1L
    ) %>%
    ungroup() %>%
    mutate(
      n_rate = safe_rescale(crime_rate_per_lakh),
      n_grow = safe_rescale(pmax(crime_growth_rate, 0, na.rm=TRUE)),
      n_viol = safe_rescale(violent_ratio),
      state_risk_score = round(n_rate * 40 + n_grow * 30 + n_viol * 30, 1),
      risk_tier        = ifelse(state_risk_score >= 70, "CRITICAL",
                         ifelse(state_risk_score >= 50, "HIGH",
                         ifelse(state_risk_score >= 30, "MEDIUM", "LOW")))
    ) %>%
    select(-n_rate, -n_grow, -n_viol)
}

engineer_yearly_features <- function(yearly_agg) {
  yearly_agg %>% arrange(year) %>%
    mutate(
      crime_yoy_pct   = (total_crimes - lag(total_crimes)) / (lag(total_crimes) + 1) * 100,
      rolling_3yr_avg = rollapply(avg_crime_rate, 3, mean, na.rm=TRUE, fill=NA, align="right"),
      year_index      = year - min(year) + 1L,
      is_high_year    = avg_crime_rate > mean(avg_crime_rate, na.rm=TRUE)
    )
}

run_feature_pipeline <- function(master) {
  log_info("=== FEATURE PIPELINE START ===")
  city   <- engineer_city_features(master$city)
  state  <- engineer_state_features(master$state)
  yearly <- engineer_yearly_features(master$yearly)
  log_info("=== FEATURE PIPELINE DONE ===")
  list(city=city, state=state, yearly=yearly)
}
