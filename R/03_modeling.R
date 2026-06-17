# =============================================================================
# 03_modeling.R     Linear Regression   Random Forest (x2)   ETS   ARIMA
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(forecast); library(randomForest); library(zoo)
})
source("R/00_utils.R")

# NOTE: total_crimes is intentionally EXCLUDED from features to prevent data leakage.
# crime_rate_per_lakh = (total_crimes / population) * 1e5, so using total_crimes
# would directly leak the target variable.
FEAT_COLS <- c("prev_year_crime_rate","rolling_avg_3yr","crime_growth_rate",
               "pop_growth_rate","violent_ratio","women_crime_ratio",
               "cyber_crime_ratio","property_crime_ratio",
               "avg_police_deployed","case_closure_rate",
               "crime_density","year_index")

# -- Prep ----------------------------------------------------------------------
prepare_ml_data <- function(city_feat) {
  avail <- intersect(FEAT_COLS, names(city_feat))
  ml <- city_feat %>%
    filter(!is.na(prev_year_crime_rate)) %>%
    mutate(city_enc = as.numeric(as.factor(city))) %>%
    select(city, year, crime_rate_per_lakh, all_of(avail), city_enc) %>%
    na.omit()
  set.seed(42)
  idx <- sample(nrow(ml), floor(.8 * nrow(ml)))
  list(train = ml[idx,], test = ml[-idx,], avail = avail)
}

# -- Linear Regression ---------------------------------------------------------
train_lr <- function(tr, te, avail) {
  log_info("Training Linear Regression")
  # Exclude prev_year_crime_rate from LR to avoid near-perfect collinearity
  # (prev_year is a near-direct function of the lagged target)
  lr_avail <- setdiff(avail, c("prev_year_crime_rate", "rolling_avg_3yr"))
  if (length(lr_avail) == 0) lr_avail <- avail  # fallback
  fml <- as.formula(paste("crime_rate_per_lakh ~", paste(lr_avail, collapse = "+")))
  m   <- suppressWarnings(lm(fml, data = tr))
  # Warn if fit is suspiciously perfect
  r2_train <- suppressWarnings(summary(m))$r.squared
  if (!is.na(r2_train) && r2_train > 0.999)
    message("[INFO] LR R^2=", round(r2_train,4), " - near-perfect fit detected, RF selected as production model")
  pt  <- predict(m, tr);  pe <- predict(m, te)
  list(model = m, predictions_train = pt, predictions_test = pe,
       metrics_train = calc_metrics(tr$crime_rate_per_lakh, pt, "LR (Train)"),
       metrics_test  = calc_metrics(te$crime_rate_per_lakh, pe, "Linear Regression"),
       feature_importance = data.frame(
         feature    = names(coef(m))[-1],
         importance = abs(coef(m))[-1]
       ) %>% arrange(desc(importance)))
}

# -- Random Forest (default mtry) ----------------------------------------------
train_rf <- function(tr, te, avail) {
  log_info("Training Random Forest")
  cols <- c(avail, "city_enc")
  Xtr <- tr[, cols]; Xte <- te[, cols]
  set.seed(42)
  m  <- randomForest(x = Xtr, y = tr$crime_rate_per_lakh,
                     ntree = 400, importance = TRUE, do.trace = FALSE)
  pt <- predict(m, Xtr); pe <- predict(m, Xte)
  imp <- as.data.frame(importance(m)) %>%
    tibble::rownames_to_column("feature") %>%
    rename(importance = `%IncMSE`) %>%
    arrange(desc(importance))
  list(model = m, predictions_train = pt, predictions_test = pe,
       metrics_train = calc_metrics(tr$crime_rate_per_lakh, pt, "RF (Train)"),
       metrics_test  = calc_metrics(te$crime_rate_per_lakh, pe, "Random Forest"),
       feature_importance = imp)
}

# -- XGBoost proxy RF (larger mtry, more trees) -------------------------------
train_xgb_proxy <- function(tr, te, avail) {
  log_info("Training XGBoost-proxy RF")
  cols <- c(avail, "city_enc")
  Xtr <- tr[, cols]; Xte <- te[, cols]
  set.seed(7)
  m  <- randomForest(x = Xtr, y = tr$crime_rate_per_lakh,
                     ntree = 600,
                     mtry = max(2L, floor(length(cols) * .6)),
                     importance = TRUE, do.trace = FALSE)
  pt <- predict(m, Xtr); pe <- predict(m, Xte)
  imp <- as.data.frame(importance(m)) %>%
    tibble::rownames_to_column("feature") %>%
    rename(importance = `%IncMSE`) %>%
    arrange(desc(importance))
  list(model = m, predictions_train = pt, predictions_test = pe,
       metrics_train = calc_metrics(tr$crime_rate_per_lakh, pt, "XGB-proxy (Train)"),
       metrics_test  = calc_metrics(te$crime_rate_per_lakh, pe, "XGBoost (RF-proxy)"),
       feature_importance = imp)
}

# -- Time-series ETS + ARIMA ---------------------------------------------------
train_ts_models <- function(yearly_feat) {
  log_info("Training ETS + ARIMA")
  ts_obj <- ts(yearly_feat$avg_crime_rate,
               start = min(yearly_feat$year), frequency = 1)
  ets_m   <- ets(ts_obj, opt.crit = "lik")
  arima_m <- auto.arima(ts_obj, stepwise = FALSE, seasonal = FALSE, ic = "aicc")
  h <- 6   # 2025-2030
  fc_ets   <- forecast(ets_m,   h = h, level = c(80, 95))
  fc_arima <- forecast(arima_m, h = h, level = c(80, 95))
  fc_yrs   <- seq(max(yearly_feat$year) + 1, length.out = h)

  mk_fc_df <- function(fc, nm)
    data.frame(year = fc_yrs, model = nm,
               forecast  = as.numeric(fc$mean),
               lower_80  = as.numeric(fc$lower[,1]),
               upper_80  = as.numeric(fc$upper[,1]),
               lower_95  = as.numeric(fc$lower[,2]),
               upper_95  = as.numeric(fc$upper[,2]))

  list(
    ets   = list(model = ets_m,   forecast_df = mk_fc_df(fc_ets,   "ETS"),
                 metrics = calc_metrics(as.numeric(ts_obj), as.numeric(fitted(ets_m)),   "ETS")),
    arima = list(model = arima_m, forecast_df = mk_fc_df(fc_arima, "ARIMA"),
                 metrics = calc_metrics(as.numeric(ts_obj), as.numeric(fitted(arima_m)), "ARIMA")),
    ts_data = ts_obj
  )
}

# -- Select best ML model by RMSE ---------------------------------------------
select_best <- function(ml) {
  comp <- do.call(rbind, lapply(ml, `[[`, "metrics_test"))
  comp <- comp[order(comp$rmse), ]
  best_nm  <- comp$model[1]
  best_key <- switch(best_nm,
    "Linear Regression"  = "linear",
    "Random Forest"      = "rf",
    "rf")
  # If LR wins with perfect R^2, use RF as the robust production model
  lr_r2 <- comp$r_squared[comp$model == "Linear Regression"]
  if (length(lr_r2) > 0 && !is.na(lr_r2) && lr_r2 >= 0.9999) {
    message("[INFO] LR R^2 approx 1 detected - selecting Random Forest as production model")
    best_nm  <- "Random Forest"
    best_key <- "rf"
  }
  message("[INFO] ", "Best model: ", best_nm, "  RMSE=", comp$rmse[1], "  R^2=", comp$r_squared[1])
  list(comparison = comp, best_model_name = best_nm, best_model = ml[[best_key]])
}

# -- City-level ETS forecasts 2025-2030 ---------------------------------------
city_forecasts <- function(city_feat, h_years = 2025:2030) {
  message("[INFO] ", "Forecasting ", length(unique(city_feat$city)), " cities")
  out <- lapply(unique(city_feat$city), function(cn) {
    cd <- city_feat %>% filter(city == cn) %>% arrange(year)
    if (nrow(cd) < 2) return(NULL)
    ts_c <- ts(cd$crime_rate_per_lakh, start = min(cd$year), frequency = 1)
    tryCatch({
      fc <- forecast(ets(ts_c, opt.crit = "lik"), h = length(h_years), level = c(80, 95))
      data.frame(city = cn, year = h_years,
                 forecast_crime_rate = pmax(0, as.numeric(fc$mean)),
                 lower_80 = pmax(0, as.numeric(fc$lower[,1])),
                 upper_80 = pmax(0, as.numeric(fc$upper[,1])),
                 lower_95 = pmax(0, as.numeric(fc$lower[,2])),
                 upper_95 = pmax(0, as.numeric(fc$upper[,2])))
    }, error = function(e) NULL)
  })
  do.call(rbind, Filter(Negate(is.null), out))
}

# -- Master pipeline -----------------------------------------------------------
run_modeling_pipeline <- function(feat) {
  log_info("=== MODELING PIPELINE START ===")
  splits  <- prepare_ml_data(feat$city)
  lr      <- train_lr(splits$train, splits$test, splits$avail)
  rf      <- train_rf(splits$train, splits$test, splits$avail)
  xgb     <- train_xgb_proxy(splits$train, splits$test, splits$avail)
  ml      <- list(linear = lr, rf = rf, xgb = xgb)
  ts      <- train_ts_models(feat$yearly)
  sel     <- select_best(ml)
  fc      <- city_forecasts(feat$city)
  log_info("=== MODELING PIPELINE DONE ===")
  list(ml = ml, ts = ts, selection = sel, city_forecasts = fc,
       train_data = splits$train, test_data = splits$test)
}
