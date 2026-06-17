# =============================================================================
# 01_data_engineering.R     Ingest   Clean   Aggregate   Master dataset
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
})
source("R/00_utils.R")

# -- 1. Load & clean raw CSV ---------------------------------------------------
load_raw <- function(data_dir = "data") {
  path <- file.path(data_dir, "crime_dataset_india.csv")
  message("[INFO] ", "Loading ", path)

  # fileEncoding="UTF-8-BOM" handles the BOM (\xef\xbb\xbf) present in this CSV
  df <- read.csv(path, stringsAsFactors = FALSE,
                 na.strings     = c("", "NA", "N/A"),
                 fileEncoding   = "UTF-8-BOM",
                 check.names    = FALSE)
  df <- clean_names(df)

  # Rename canonical columns
  # Handle both BOM-prefixed and normal column names after clean_names
  name_map <- c("x_report_number" = "report_number",
                "report_number"   = "report_number")  # already correct
  for (old_nm in names(name_map)) {
    if (old_nm %in% names(df)) names(df)[names(df) == old_nm] <- name_map[[old_nm]]
  }

  # -- Parse dates -------------------------------------------------------------
  # Format: "01-01-2020 00:00"  ->  dd-mm-yyyy HH:MM
  # CSV date format verified as mm-dd-yyyy HH:MM (e.g. "01-13-2020 00:00")
  df$date_of_occurrence <- as.POSIXct(df$date_of_occurrence,
                                       format = "%m-%d-%Y %H:%M",
                                       tz     = "Asia/Kolkata")
  # Fallback: try dd-mm-yyyy if primary parse produces >50% NA
  if (sum(is.na(df$date_of_occurrence)) > nrow(df) * 0.5) {
    warning("Primary date format failed - retrying with dd-mm-yyyy")
    df$date_of_occurrence <- as.POSIXct(df$date_of_occurrence,
                                         format = "%d-%m-%Y %H:%M",
                                         tz     = "Asia/Kolkata")
  }
  df$year    <- as.integer(format(df$date_of_occurrence, "%Y"))
  df$month   <- as.integer(format(df$date_of_occurrence, "%m"))
  df$quarter <- ceiling(df$month / 3L)
  df$hour    <- as.integer(format(df$date_of_occurrence, "%H"))

  # -- Remove duplicates --------------------------------------------------------
  n_before <- nrow(df)
  df <- df[!duplicated(df[, c("report_number","city","crime_code")]), ]
  message("[INFO] ", "Duplicates removed: ", n_before - nrow(df))

  # -- Impute missing weapon ----------------------------------------------------
  df$weapon_used[is.na(df$weapon_used)] <- "Unknown"

  # -- Standardise crime domain -------------------------------------------------
  d <- tolower(df$crime_domain)
  df$crime_domain <- ifelse(grepl("violent",  d), "Violent Crime",
                     ifelse(grepl("fire",      d), "Fire Accident",
                     ifelse(grepl("traffic",   d), "Traffic Fatality",
                     "Other Crime")))

  # -- Macro crime category from description ------------------------------------
  desc <- tolower(df$crime_description)
  df$crime_category <- ifelse(grepl("murder|homicide|assault|kidnap|robbery|dacoity|rape|sexual", desc), "Violent Crime",
                       ifelse(grepl("theft|burglary|fraud|property|extortion|snatching", desc),   "Property Crime",
                       ifelse(grepl("harassment|stalking|dowry|trafficking|molestation|domestic",  desc), "Women-Related Crime",
                       ifelse(grepl("cyber|identity|hacking|phishing|online",                       desc), "Cyber Crime",
                       ifelse(grepl("drug|narcotic|gambling|liquor|excise",                          desc), "Vice Crime",
                       ifelse(grepl("accident|fire|traffic|negligence|road",                         desc), "Accidental",
                       "Other"))))))

  # -- Boolean case_closed -------------------------------------------------------
  df$case_closed[is.na(df$case_closed)] <- "No"

  # -- State mapping -------------------------------------------------------------
  df$state <- CITY_STATE[df$city]

  message("[INFO] ", "Raw loaded: ", nrow(df), " rows   ", ncol(df), " cols   years ", min(df$year,na.rm=TRUE), "-", max(df$year,na.rm=TRUE))
  df
}

# -- 2. City-level annual aggregation -----------------------------------------
build_city_agg <- function(df) {
  agg <- df %>%
    filter(!is.na(year), !is.na(city)) %>%
    group_by(city, year) %>%
    summarise(
      total_crimes        = n(),
      violent_crimes      = sum(crime_domain == "Violent Crime",          na.rm = TRUE),
      fire_accidents      = sum(crime_domain == "Fire Accident",          na.rm = TRUE),
      traffic_fatalities  = sum(crime_domain == "Traffic Fatality",       na.rm = TRUE),
      other_crimes        = sum(crime_domain == "Other Crime",            na.rm = TRUE),
      women_crimes        = sum(crime_category == "Women-Related Crime",  na.rm = TRUE),
      cyber_crimes        = sum(crime_category == "Cyber Crime",          na.rm = TRUE),
      property_crimes     = sum(crime_category == "Property Crime",       na.rm = TRUE),
      vice_crimes         = sum(crime_category == "Vice Crime",           na.rm = TRUE),
      avg_police_deployed = mean(as.numeric(police_deployed), na.rm = TRUE),
      case_closure_rate   = mean(toupper(case_closed) == "YES",           na.rm = TRUE) * 100,
      avg_victim_age      = mean(as.numeric(victim_age),                  na.rm = TRUE),
      female_victim_pct   = mean(toupper(victim_gender) %in% c("F","FEMALE"), na.rm = TRUE) * 100,
      night_crime_pct     = mean(hour >= 20 | hour <= 5,                  na.rm = TRUE) * 100,
      weapon_used_pct     = mean(weapon_used != "Unknown",               na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      population          = mapply(get_city_pop, city, year),
      crime_rate_per_lakh = total_crimes / population * 1e5,
      violent_rate        = violent_crimes / population * 1e5,
      state               = CITY_STATE[city]
    )
  message("[INFO] ", "City agg: ", nrow(agg), " rows")
  agg
}

# -- 3. State-level annual aggregation ----------------------------------------
build_state_agg <- function(city_agg) {
  agg <- city_agg %>%
    filter(!is.na(state)) %>%
    group_by(state, year) %>%
    summarise(
      total_crimes        = sum(total_crimes,       na.rm = TRUE),
      violent_crimes      = sum(violent_crimes,     na.rm = TRUE),
      women_crimes        = sum(women_crimes,       na.rm = TRUE),
      cyber_crimes        = sum(cyber_crimes,       na.rm = TRUE),
      property_crimes     = sum(property_crimes,    na.rm = TRUE),
      population          = sum(population,         na.rm = TRUE),
      avg_case_closure    = mean(case_closure_rate, na.rm = TRUE),
      n_cities            = n_distinct(city),
      .groups = "drop"
    ) %>%
    mutate(crime_rate_per_lakh = total_crimes / population * 1e5)
  message("[INFO] ", "State agg: ", nrow(agg), " rows")
  agg
}

# -- 4. Yearly national aggregate ---------------------------------------------
build_yearly_agg <- function(city_agg) {
  city_agg %>%
    group_by(year) %>%
    summarise(
      total_crimes      = sum(total_crimes,       na.rm = TRUE),
      violent_crimes    = sum(violent_crimes,     na.rm = TRUE),
      women_crimes      = sum(women_crimes,       na.rm = TRUE),
      cyber_crimes      = sum(cyber_crimes,       na.rm = TRUE),
      property_crimes   = sum(property_crimes,    na.rm = TRUE),
      avg_crime_rate    = mean(crime_rate_per_lakh, na.rm = TRUE),
      median_crime_rate = median(crime_rate_per_lakh, na.rm = TRUE),
      n_cities          = n_distinct(city),
      .groups = "drop"
    ) %>% arrange(year)
}

# -- 5. Master pipeline --------------------------------------------------------
create_master_dataset <- function(data_dir = "data") {
  log_info("=== DATA ENGINEERING START ===")
  raw    <- load_raw(data_dir)
  city   <- build_city_agg(raw)
  state  <- build_state_agg(city)
  yearly <- build_yearly_agg(city)
  log_info("=== DATA ENGINEERING DONE ===")
  list(raw = raw, city = city, state = state, yearly = yearly)
}
