# =============================================================================
# 07_statistical_tests.R
# Statistical Hypothesis Testing: T-Test   Chi-Square   ANOVA
# =============================================================================
source("R/00_utils.R")

# -- Helper: formatted p-value -------------------------------------------------
fmt_p <- function(p) {
  if (is.na(p))       return("NA")
  if (p < 0.001)      return("< 0.001 ***")
  if (p < 0.01)       return(paste0(round(p,4), " **"))
  if (p < 0.05)       return(paste0(round(p,4), " *"))
  if (p < 0.10)       return(paste0(round(p,4), " ."))
  return(paste0(round(p,4), " ns"))
}

significance_label <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.10)  return(".")
  return("ns")
}

# =============================================================================
# 1. T-TESTS
# =============================================================================

# 1a. One-sample t-test: is mean crime rate != national average?
t_one_sample <- function(city_feat, mu_ref = NULL) {
  df     <- city_feat %>% filter(!is.na(crime_rate_per_lakh))
  mu_nat <- mu_ref %||% mean(df$crime_rate_per_lakh, na.rm = TRUE)

  tt <- t.test(df$crime_rate_per_lakh, mu = mu_nat)
  data.frame(
    test          = "One-Sample T-Test",
    description   = paste0("H0: mean crime rate = ", round(mu_nat, 2), " (national avg)"),
    t_statistic   = round(tt$statistic, 4),
    df            = round(tt$parameter, 2),
    p_value       = round(tt$p.value, 6),
    significance  = significance_label(tt$p.value),
    ci_lower      = round(tt$conf.int[1], 4),
    ci_upper      = round(tt$conf.int[2], 4),
    mean_estimate = round(tt$estimate, 4),
    conclusion    = ifelse(tt$p.value < 0.05,
                           "REJECT H0: Mean crime rate differs significantly from national average",
                           "FAIL TO REJECT H0: No significant difference from national average"),
    stringsAsFactors = FALSE
  )
}

# 1b. Independent two-sample t-test: HIGH+CRITICAL vs LOW+MEDIUM risk cities
t_risk_groups <- function(city_feat) {
  df <- city_feat %>%
    filter(!is.na(crime_rate_per_lakh)) %>%
    mutate(risk_group = ifelse(risk_tier %in% c("HIGH","CRITICAL"), "High Risk", "Low Risk"))

  hi  <- df$crime_rate_per_lakh[df$risk_group == "High Risk"]
  lo  <- df$crime_rate_per_lakh[df$risk_group == "Low Risk"]

  tt  <- t.test(hi, lo, var.equal = FALSE)   # Welch t-test
  lev <- var.test(hi, lo)                    # Levene proxy via F-test

  data.frame(
    test         = "Two-Sample Welch T-Test",
    description  = "H0: High-Risk cities have same mean crime rate as Low-Risk cities",
    group_1      = "HIGH + CRITICAL",
    group_2      = "LOW + MEDIUM",
    n_group1     = length(hi),
    n_group2     = length(lo),
    mean_group1  = round(mean(hi), 4),
    mean_group2  = round(mean(lo), 4),
    mean_diff    = round(mean(hi) - mean(lo), 4),
    t_statistic  = round(tt$statistic, 4),
    df           = round(tt$parameter, 2),
    p_value      = round(tt$p.value, 6),
    significance = significance_label(tt$p.value),
    ci_lower     = round(tt$conf.int[1], 4),
    ci_upper     = round(tt$conf.int[2], 4),
    f_var_test_p = round(lev$p.value, 4),
    conclusion   = ifelse(tt$p.value < 0.05,
                          "REJECT H0: Significant crime rate difference between risk groups",
                          "FAIL TO REJECT H0: No significant difference between risk groups"),
    stringsAsFactors = FALSE
  )
}

# 1c. Paired t-test: 2020 vs 2023 crime rates (same cities)
t_paired_years <- function(city_feat, yr1 = 2020, yr2 = 2023) {
  common <- city_feat %>%
    filter(year %in% c(yr1, yr2)) %>%
    select(city, year, crime_rate_per_lakh) %>%
    tidyr::pivot_wider(names_from = year, values_from = crime_rate_per_lakh,
                       names_prefix = "yr_") %>%
    filter(!is.na(.data[[paste0("yr_",yr1)]] ),
           !is.na(.data[[paste0("yr_",yr2)]]))

  g1 <- common[[paste0("yr_",yr1)]]
  g2 <- common[[paste0("yr_",yr2)]]
  tt <- t.test(g1, g2, paired = TRUE)

  data.frame(
    test         = "Paired T-Test",
    description  = paste0("H0: Mean crime rate in ", yr1, " = Mean crime rate in ", yr2),
    n_pairs      = nrow(common),
    mean_yr1     = round(mean(g1), 4),
    mean_yr2     = round(mean(g2), 4),
    mean_diff    = round(mean(g1 - g2), 4),
    t_statistic  = round(tt$statistic, 4),
    df           = round(tt$parameter, 2),
    p_value      = round(tt$p.value, 6),
    significance = significance_label(tt$p.value),
    ci_lower     = round(tt$conf.int[1], 4),
    ci_upper     = round(tt$conf.int[2], 4),
    conclusion   = ifelse(tt$p.value < 0.05,
                          paste0("REJECT H0: Crime rate changed significantly from ",yr1," to ",yr2),
                          paste0("FAIL TO REJECT H0: No significant change from ",yr1," to ",yr2)),
    stringsAsFactors = FALSE
  )
}

# 1d. T-test: Metro vs Non-Metro crime rates
t_metro_vs_nonmetro <- function(city_feat) {
  metros     <- c("Mumbai","Delhi","Bangalore","Kolkata","Chennai","Hyderabad","Pune","Ahmedabad")
  df <- city_feat %>%
    filter(!is.na(crime_rate_per_lakh)) %>%
    mutate(city_type = ifelse(city %in% metros, "Metro", "Non-Metro"))

  m  <- df$crime_rate_per_lakh[df$city_type == "Metro"]
  nm <- df$crime_rate_per_lakh[df$city_type == "Non-Metro"]
  tt <- t.test(m, nm, var.equal = FALSE)

  data.frame(
    test         = "Metro vs Non-Metro T-Test",
    description  = "H0: Metro and Non-Metro cities have equal mean crime rates",
    n_metro      = length(m),
    n_nonmetro   = length(nm),
    mean_metro   = round(mean(m), 4),
    mean_nonmetro= round(mean(nm), 4),
    mean_diff    = round(mean(m) - mean(nm), 4),
    t_statistic  = round(tt$statistic, 4),
    df           = round(tt$parameter, 2),
    p_value      = round(tt$p.value, 6),
    significance = significance_label(tt$p.value),
    conclusion   = ifelse(tt$p.value < 0.05,
                          "REJECT H0: Significant crime rate difference between Metro and Non-Metro",
                          "FAIL TO REJECT H0: No significant difference"),
    stringsAsFactors = FALSE
  )
}

# 1e. T-test: Violent crime rate: Case-closure HIGH vs LOW cities
t_case_closure <- function(city_feat) {
  df <- city_feat %>% filter(!is.na(violent_ratio), !is.na(case_closure_rate)) %>%
    mutate(closure_group = ifelse(case_closure_rate >= median(case_closure_rate, na.rm=TRUE),
                                  "High Closure", "Low Closure"))
  hi <- df$violent_ratio[df$closure_group == "High Closure"]
  lo <- df$violent_ratio[df$closure_group == "Low Closure"]
  tt <- t.test(hi, lo, var.equal = FALSE)
  data.frame(
    test         = "Case Closure vs Violent Crime T-Test",
    description  = "H0: High case-closure cities have same violent crime ratio as low closure",
    mean_high_closure = round(mean(hi), 4),
    mean_low_closure  = round(mean(lo), 4),
    t_statistic  = round(tt$statistic, 4),
    df           = round(tt$parameter, 2),
    p_value      = round(tt$p.value, 6),
    significance = significance_label(tt$p.value),
    conclusion   = ifelse(tt$p.value < 0.05,
                          "REJECT H0: Case closure significantly affects violent crime ratio",
                          "FAIL TO REJECT H0: No significant relationship"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 2. CHI-SQUARE TESTS
# =============================================================================

# 2a. Chi-square: Risk tier distribution differs across states?
chi_risk_vs_state <- function(city_feat) {
  ct <- table(city_feat$state, city_feat$risk_tier)
  # Remove any all-zero rows/cols
  ct <- ct[rowSums(ct) > 0, colSums(ct) > 0, drop = FALSE]
  cs <- chisq.test(ct, simulate.p.value = (min(ct) < 5))
  data.frame(
    test         = "Chi-Square: Risk Tier x State",
    description  = "H0: Risk tier distribution is independent of state",
    chi_sq       = round(cs$statistic, 4),
    df           = cs$parameter,
    p_value      = round(cs$p.value, 6),
    significance = significance_label(cs$p.value),
    cramers_v    = round(sqrt(cs$statistic / (sum(ct) * (min(dim(ct)) - 1))), 4),
    n_obs        = sum(ct),
    conclusion   = ifelse(cs$p.value < 0.05,
                          "REJECT H0: Risk tier distribution significantly varies by state",
                          "FAIL TO REJECT H0: Risk tier is independent of state"),
    stringsAsFactors = FALSE
  )
}

# 2b. Chi-square: Case closure independent of crime domain?
chi_closure_vs_domain <- function(raw_df) {
  df <- raw_df %>%
    filter(!is.na(case_closed), !is.na(crime_domain)) %>%
    mutate(closed = toupper(case_closed) == "YES")
  ct <- table(df$crime_domain, df$closed)
  ct <- ct[rowSums(ct) > 0, , drop = FALSE]
  cs <- chisq.test(ct)
  data.frame(
    test         = "Chi-Square: Case Closure x Crime Domain",
    description  = "H0: Case closure is independent of crime domain",
    chi_sq       = round(cs$statistic, 4),
    df           = cs$parameter,
    p_value      = round(cs$p.value, 6),
    significance = significance_label(cs$p.value),
    cramers_v    = round(sqrt(cs$statistic / (sum(ct) * (min(dim(ct)) - 1))), 4),
    n_obs        = sum(ct),
    conclusion   = ifelse(cs$p.value < 0.05,
                          "REJECT H0: Case closure rate significantly differs across crime domains",
                          "FAIL TO REJECT H0: Closure is independent of crime domain"),
    stringsAsFactors = FALSE
  )
}

# 2c. Chi-square: Gender distribution differs across crime categories?
chi_gender_vs_category <- function(raw_df) {
  df <- raw_df %>%
    filter(toupper(victim_gender) %in% c("M","F","MALE","FEMALE"),
           !is.na(crime_category)) %>%
    mutate(gender = ifelse(toupper(victim_gender) %in% c("F","FEMALE"), "Female", "Male"),
           cat    = ifelse(crime_category %in% c("Violent Crime","Women-Related Crime",
                                                  "Property Crime","Cyber Crime"),
                           crime_category, "Other"))
  ct <- table(df$cat, df$gender)
  ct <- ct[rowSums(ct) > 0, , drop = FALSE]
  cs <- chisq.test(ct)
  data.frame(
    test         = "Chi-Square: Victim Gender x Crime Category",
    description  = "H0: Victim gender distribution is independent of crime category",
    chi_sq       = round(cs$statistic, 4),
    df           = cs$parameter,
    p_value      = round(cs$p.value, 6),
    significance = significance_label(cs$p.value),
    cramers_v    = round(sqrt(cs$statistic / (sum(ct) * (min(dim(ct)) - 1))), 4),
    n_obs        = sum(ct),
    conclusion   = ifelse(cs$p.value < 0.05,
                          "REJECT H0: Gender distribution significantly differs across crime categories",
                          "FAIL TO REJECT H0: Gender is independent of crime category"),
    stringsAsFactors = FALSE
  )
}

# 2d. Chi-square: Weapon use rate differs across cities?
chi_weapon_vs_city <- function(raw_df) {
  df <- raw_df %>%
    filter(!is.na(weapon_used), !is.na(city)) %>%
    mutate(weapon = ifelse(weapon_used == "Unknown", "No Weapon", "Weapon Used"))
  ct <- table(df$city, df$weapon)
  ct <- ct[rowSums(ct) > 0, , drop = FALSE]
  cs <- chisq.test(ct, simulate.p.value = TRUE, B = 2000)
  data.frame(
    test         = "Chi-Square: Weapon Use x City",
    description  = "H0: Weapon usage rate is independent of city",
    chi_sq       = round(cs$statistic, 4),
    df           = ifelse(is.null(cs$parameter), NA, cs$parameter),
    p_value      = round(cs$p.value, 6),
    significance = significance_label(cs$p.value),
    n_obs        = sum(ct),
    conclusion   = ifelse(cs$p.value < 0.05,
                          "REJECT H0: Weapon usage rate significantly varies across cities",
                          "FAIL TO REJECT H0: Weapon use is city-independent"),
    stringsAsFactors = FALSE
  )
}

# 2e. Chi-square: Risk tier vs Year (is risk distribution changing over time?)
chi_risk_vs_year <- function(city_feat) {
  ct <- table(city_feat$year, city_feat$risk_tier)
  ct <- ct[rowSums(ct) > 0, colSums(ct) > 0, drop = FALSE]
  cs <- chisq.test(ct, simulate.p.value = (any(ct < 5)))
  data.frame(
    test         = "Chi-Square: Risk Tier x Year",
    description  = "H0: Risk tier distribution is stable across years",
    chi_sq       = round(cs$statistic, 4),
    df           = ifelse(is.null(cs$parameter), NA, cs$parameter),
    p_value      = round(cs$p.value, 6),
    significance = significance_label(cs$p.value),
    n_obs        = sum(ct),
    conclusion   = ifelse(cs$p.value < 0.05,
                          "REJECT H0: Risk tier distribution significantly changed over years",
                          "FAIL TO REJECT H0: Risk distribution stable across years"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 3. ONE-WAY ANOVA
# =============================================================================

# 3a. ANOVA: Crime rate differs across risk tiers?
anova_rate_by_risk <- function(city_feat) {
  df <- city_feat %>% filter(!is.na(crime_rate_per_lakh), !is.na(risk_tier))
  fit <- aov(crime_rate_per_lakh ~ risk_tier, data = df)
  s   <- summary(fit)[[1]]
  # Tukey HSD post-hoc
  tukey <- tryCatch(TukeyHSD(fit), error = function(e) NULL)
  tukey_df <- if (!is.null(tukey)) {
    as.data.frame(tukey$risk_tier) %>%
      tibble::rownames_to_column("comparison") %>%
      rename(diff=diff, lwr=lwr, upr=upr, p_adj=`p adj`) %>%
      mutate(significant = p_adj < 0.05)
  } else NULL

  list(
    result = data.frame(
      test         = "One-Way ANOVA: Crime Rate ~ Risk Tier",
      description  = "H0: Mean crime rate is equal across all risk tiers",
      groups       = paste(sort(unique(df$risk_tier)), collapse=" | "),
      group_means  = paste(round(tapply(df$crime_rate_per_lakh, df$risk_tier, mean, na.rm=TRUE),2), collapse=" | "),
      f_statistic  = round(s$`F value`[1], 4),
      df_between   = s$Df[1],
      df_within    = s$Df[2],
      p_value      = round(s$`Pr(>F)`[1], 6),
      significance = significance_label(s$`Pr(>F)`[1]),
      eta_squared  = round(s$`Sum Sq`[1] / sum(s$`Sum Sq`), 4),
      conclusion   = ifelse(s$`Pr(>F)`[1] < 0.05,
                            "REJECT H0: Crime rate significantly differs across risk tiers",
                            "FAIL TO REJECT H0: No significant difference across tiers"),
      stringsAsFactors = FALSE
    ),
    tukey = tukey_df,
    model = fit
  )
}

# 3b. ANOVA: Crime rate differs across states?
anova_rate_by_state <- function(city_feat) {
  df  <- city_feat %>% filter(!is.na(crime_rate_per_lakh), !is.na(state))
  fit <- aov(crime_rate_per_lakh ~ state, data = df)
  s   <- summary(fit)[[1]]

  state_means <- df %>% group_by(state) %>%
    summarise(mean_rate=round(mean(crime_rate_per_lakh,na.rm=TRUE),2),
              n=n(), .groups="drop") %>%
    arrange(desc(mean_rate))

  list(
    result = data.frame(
      test         = "One-Way ANOVA: Crime Rate ~ State",
      description  = "H0: Mean crime rate is equal across all states",
      n_groups     = n_distinct(df$state),
      f_statistic  = round(s$`F value`[1], 4),
      df_between   = s$Df[1],
      df_within    = s$Df[2],
      p_value      = round(s$`Pr(>F)`[1], 6),
      significance = significance_label(s$`Pr(>F)`[1]),
      eta_squared  = round(s$`Sum Sq`[1] / sum(s$`Sum Sq`), 4),
      conclusion   = ifelse(s$`Pr(>F)`[1] < 0.05,
                            "REJECT H0: Crime rate significantly differs across states",
                            "FAIL TO REJECT H0: No significant difference across states"),
      stringsAsFactors = FALSE
    ),
    state_means = state_means,
    model = fit
  )
}

# 3c. ANOVA: Violent crime ratio across crime domains?
anova_violent_by_domain <- function(raw_df) {
  df <- raw_df %>%
    filter(!is.na(crime_domain)) %>%
    group_by(city, year, crime_domain) %>%
    summarise(violent_pct = mean(crime_domain == "Violent Crime", na.rm=TRUE) * 100,
              .groups="drop")
  fit <- aov(violent_pct ~ crime_domain, data = df)
  s   <- summary(fit)[[1]]
  data.frame(
    test         = "One-Way ANOVA: Violent % ~ Crime Domain",
    description  = "H0: Violent crime percentage is equal across crime domains",
    f_statistic  = round(s$`F value`[1], 4),
    df_between   = s$Df[1],
    df_within    = s$Df[2],
    p_value      = round(s$`Pr(>F)`[1], 6),
    significance = significance_label(s$`Pr(>F)`[1]),
    eta_squared  = round(s$`Sum Sq`[1] / sum(s$`Sum Sq`), 4),
    conclusion   = ifelse(s$`Pr(>F)`[1] < 0.05,
                          "REJECT H0: Violent crime % significantly differs across domains",
                          "FAIL TO REJECT H0: No significant difference"),
    stringsAsFactors = FALSE
  )
}

# 3d. ANOVA: Victim age differs across crime categories?
anova_age_by_category <- function(raw_df) {
  df <- raw_df %>%
    filter(!is.na(victim_age), !is.na(crime_category),
           victim_age > 0, victim_age < 100,
           crime_category != "Other")
  fit <- aov(as.numeric(victim_age) ~ crime_category, data = df)
  s   <- summary(fit)[[1]]
  tukey <- tryCatch(TukeyHSD(fit), error = function(e) NULL)
  tukey_df <- if (!is.null(tukey)) {
    as.data.frame(tukey$crime_category) %>%
      tibble::rownames_to_column("comparison") %>%
      mutate(significant = `p adj` < 0.05) %>%
      arrange(`p adj`)
  } else NULL

  list(
    result = data.frame(
      test         = "One-Way ANOVA: Victim Age ~ Crime Category",
      description  = "H0: Mean victim age is equal across crime categories",
      group_means  = paste(
        round(tapply(as.numeric(df$victim_age), df$crime_category, mean, na.rm=TRUE), 1),
        collapse=" | "),
      f_statistic  = round(s$`F value`[1], 4),
      df_between   = s$Df[1],
      df_within    = s$Df[2],
      p_value      = round(s$`Pr(>F)`[1], 6),
      significance = significance_label(s$`Pr(>F)`[1]),
      eta_squared  = round(s$`Sum Sq`[1] / sum(s$`Sum Sq`), 4),
      conclusion   = ifelse(s$`Pr(>F)`[1] < 0.05,
                            "REJECT H0: Victim age significantly differs across crime categories",
                            "FAIL TO REJECT H0: Victim age is similar across crime categories"),
      stringsAsFactors = FALSE
    ),
    tukey = tukey_df,
    model = fit
  )
}

# 3e. ANOVA: Police deployment differs across risk tiers?
anova_police_by_risk <- function(city_feat) {
  df  <- city_feat %>% filter(!is.na(avg_police_deployed), !is.na(risk_tier))
  fit <- aov(avg_police_deployed ~ risk_tier, data = df)
  s   <- summary(fit)[[1]]
  data.frame(
    test         = "One-Way ANOVA: Police Deployed ~ Risk Tier",
    description  = "H0: Average police deployment is equal across risk tiers",
    group_means  = paste(round(tapply(df$avg_police_deployed, df$risk_tier, mean, na.rm=TRUE),2), collapse=" | "),
    f_statistic  = round(s$`F value`[1], 4),
    df_between   = s$Df[1],
    df_within    = s$Df[2],
    p_value      = round(s$`Pr(>F)`[1], 6),
    significance = significance_label(s$`Pr(>F)`[1]),
    eta_squared  = round(s$`Sum Sq`[1] / sum(s$`Sum Sq`), 4),
    conclusion   = ifelse(s$`Pr(>F)`[1] < 0.05,
                          "REJECT H0: Police deployment significantly differs across risk tiers",
                          "FAIL TO REJECT H0: Deployment is similar across risk tiers"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# MASTER TEST RUNNER
# =============================================================================
run_statistical_tests <- function(master, feat) {
  message("[INFO] === STATISTICAL TESTS START ===")

  # -- T-Tests ------------------------------------------------------------------
  message("[INFO] Running T-Tests...")
  t1 <- t_one_sample(feat$city)
  t2 <- t_risk_groups(feat$city)
  t3 <- t_paired_years(feat$city, 2020, 2023)
  t4 <- t_metro_vs_nonmetro(feat$city)
  t5 <- t_case_closure(feat$city)

  # -- Chi-Square Tests ---------------------------------------------------------
  message("[INFO] Running Chi-Square Tests...")
  c1 <- chi_risk_vs_state(feat$city)
  c2 <- chi_closure_vs_domain(master$raw)
  c3 <- chi_gender_vs_category(master$raw)
  c4 <- chi_weapon_vs_city(master$raw)
  c5 <- chi_risk_vs_year(feat$city)

  # -- ANOVA --------------------------------------------------------------------
  message("[INFO] Running ANOVA Tests...")
  a1 <- anova_rate_by_risk(feat$city)
  a2 <- anova_rate_by_state(feat$city)
  a3 <- anova_violent_by_domain(master$raw)
  a4 <- anova_age_by_category(master$raw)
  a5 <- anova_police_by_risk(feat$city)

  message("[INFO] === STATISTICAL TESTS DONE ===")

  list(
    ttest = list(
      one_sample      = t1,
      risk_groups     = t2,
      paired_years    = t3,
      metro_nonmetro  = t4,
      case_closure    = t5
    ),
    chisq = list(
      risk_vs_state   = c1,
      closure_vs_domain = c2,
      gender_vs_category = c3,
      weapon_vs_city  = c4,
      risk_vs_year    = c5
    ),
    anova = list(
      rate_by_risk    = a1,
      rate_by_state   = a2,
      violent_by_domain = a3,
      age_by_category = a4,
      police_by_risk  = a5
    )
  )
}
