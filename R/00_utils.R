# =============================================================================
# 00_utils.R     Shared helpers & package-compatibility shims
# =============================================================================

# -- clean_names shim (replaces janitor) --------------------------------------
clean_names <- function(df) {
  nm <- names(df)
  nm <- tolower(nm)
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("^_|_$", "", nm)
  nm <- gsub("_{2,}", "_", nm)
  names(df) <- make.unique(nm)
  df
}

# -- Null-coalescing -----------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# -- Logging wrappers (uses logger if loaded, else message) --------------------
.log <- function(lvl, ...) {
  args <- list(...)
  msg  <- if (length(args) == 1) as.character(args[[1]])
          else paste0(sapply(args, as.character), collapse = "")
  message(sprintf("[%s %s] %s", lvl, format(Sys.time(), "%H:%M:%S"), msg))
}
log_info  <- function(...) .log("INFO",  ...)
log_warn  <- function(...) .log("WARN",  ...)
log_error <- function(...) .log("ERROR", ...)

# -- Safe rescale 0->1 ---------------------------------------------------------
safe_rescale <- function(x, to = c(0, 1)) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (is.na(rng[1]) || diff(rng) == 0) return(rep(mean(to), length(x)))
  (x - rng[1]) / (rng[2] - rng[1]) * diff(to) + to[1]
}

# -- Metric functions ----------------------------------------------------------
calc_rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
calc_mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
calc_mape <- function(a, p) mean(abs((a - p) / (abs(a) + 1e-9)), na.rm = TRUE) * 100
calc_r2   <- function(a, p) {
  ss_res <- sum((a - p)^2, na.rm = TRUE)
  ss_tot <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}
calc_metrics <- function(actual, predicted, model_name) {
  a <- as.numeric(actual); p <- as.numeric(predicted)
  data.frame(model = model_name,
             rmse  = round(calc_rmse(a, p), 4),
             mae   = round(calc_mae(a, p),  4),
             mape  = round(calc_mape(a, p), 2),
             r_squared = round(calc_r2(a, p), 4),
             n_obs = length(a),
             stringsAsFactors = FALSE)
}

# -- City -> State mapping (29 actual cities in dataset) -----------------------
CITY_STATE <- c(
  "Agra"="Uttar Pradesh", "Ahmedabad"="Gujarat",
  "Bangalore"="Karnataka", "Bhopal"="Madhya Pradesh",
  "Chennai"="Tamil Nadu", "Delhi"="Delhi",
  "Faridabad"="Haryana", "Ghaziabad"="Uttar Pradesh",
  "Hyderabad"="Telangana", "Indore"="Madhya Pradesh",
  "Jaipur"="Rajasthan", "Kalyan"="Maharashtra",
  "Kanpur"="Uttar Pradesh", "Kolkata"="West Bengal",
  "Lucknow"="Uttar Pradesh", "Ludhiana"="Punjab",
  "Meerut"="Uttar Pradesh", "Mumbai"="Maharashtra",
  "Nagpur"="Maharashtra", "Nashik"="Maharashtra",
  "Patna"="Bihar", "Pune"="Maharashtra",
  "Rajkot"="Gujarat", "Srinagar"="Jammu & Kashmir",
  "Surat"="Gujarat", "Thane"="Maharashtra",
  "Varanasi"="Uttar Pradesh", "Vasai"="Maharashtra",
  "Visakhapatnam"="Andhra Pradesh"
)

# -- City populations (2011 census + linear interpolation to 2024) -------------
CITY_POP_2011 <- c(
  "Agra"=1574542, "Ahmedabad"=5570585, "Bangalore"=8425970,
  "Bhopal"=1798218, "Chennai"=7088000, "Delhi"=16787941,
  "Faridabad"=1414050, "Ghaziabad"=2358525, "Hyderabad"=6809970,
  "Indore"=1960631, "Jaipur"=3046163, "Kalyan"=1246381,
  "Kanpur"=2920496, "Kolkata"=4496694, "Lucknow"=2817105,
  "Ludhiana"=1613878, "Meerut"=1309023, "Mumbai"=12478447,
  "Nagpur"=2405421, "Nashik"=1486053, "Patna"=1683200,
  "Pune"=3124458, "Rajkot"=1390640, "Srinagar"=1180570,
  "Surat"=4467797, "Thane"=1841488, "Varanasi"=1201696,
  "Vasai"=695445, "Visakhapatnam"=1728128
)
CITY_POP_2024 <- c(
  "Agra"=2000000, "Ahmedabad"=8450000, "Bangalore"=14227000,
  "Bhopal"=2800000, "Chennai"=11235000, "Delhi"=21359000,
  "Faridabad"=1900000, "Ghaziabad"=3200000, "Hyderabad"=10534000,
  "Indore"=3500000, "Jaipur"=4500000, "Kalyan"=1850000,
  "Kanpur"=3500000, "Kolkata"=7122000, "Lucknow"=4250000,
  "Ludhiana"=2100000, "Meerut"=1600000, "Mumbai"=21297000,
  "Nagpur"=2600000, "Nashik"=1700000, "Patna"=2240000,
  "Pune"=7764000, "Rajkot"=2100000, "Srinagar"=1500000,
  "Surat"=8618000, "Thane"=2600000, "Varanasi"=1700000,
  "Vasai"=1100000, "Visakhapatnam"=2700000
)
get_city_pop <- function(city, year) {
  p11 <- CITY_POP_2011[city] %||% 2000000
  p24 <- CITY_POP_2024[city] %||% 2500000
  round(p11 + (year - 2011) / 13 * (p24 - p11))
}
