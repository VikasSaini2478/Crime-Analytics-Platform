# =============================================================================
# 08_stats_report.R     Excel + HTML export for statistical test results
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(openxlsx) })
source("R/00_utils.R")

generate_stats_excel <- function(tests, output_dir = "reports") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  wb <- createWorkbook()

  # Styles
  hs  <- createStyle(fontName="Calibri", fontSize=11, fontColour="#FFFFFF",
                     bgFill="#1a1f36", halign="CENTER", textDecoration="Bold",
                     border="Bottom", borderColour="#58a6ff", borderStyle="medium")
  sig <- createStyle(fontColour="#3fb950", textDecoration="Bold")
  ns_ <- createStyle(fontColour="#8b949e")
  rd  <- createStyle(fontColour="#f85149", textDecoration="Bold")

  add_result_sheet <- function(wb, sheet, df, title) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, title, startRow=1)
    addStyle(wb, sheet, createStyle(fontSize=13, textDecoration="Bold",
             fontColour="#1a1f36"), rows=1, cols=1)
    writeDataTable(wb, sheet, df, startRow=3, tableStyle="TableStyleMedium15")
    setColWidths(wb, sheet, 1:ncol(df), widths=pmax(nchar(names(df))+4, 16))
    # Colour significance column
    sig_col <- which(names(df) == "significance")
    if (length(sig_col)) {
      for(r in seq_len(nrow(df))+3) {
        val <- df$significance[r-3]
        sty <- if (grepl("\\*", val)) sig else if (val=="ns") ns_ else NULL
        if (!is.null(sty)) addStyle(wb, sheet, sty, rows=r, cols=sig_col, stack=TRUE)
      }
    }
  }

  # -- T-Tests ------------------------------------------------------------------
  t_all <- dplyr::bind_rows(
    tests$ttest$one_sample,
    tests$ttest$risk_groups    [, intersect(names(tests$ttest$one_sample), names(tests$ttest$risk_groups))],
    tests$ttest$paired_years   [, intersect(names(tests$ttest$one_sample), names(tests$ttest$paired_years))],
    tests$ttest$metro_nonmetro [, intersect(names(tests$ttest$one_sample), names(tests$ttest$metro_nonmetro))],
    tests$ttest$case_closure   [, intersect(names(tests$ttest$one_sample), names(tests$ttest$case_closure))]
  )
  add_result_sheet(wb, "T-Tests", tests$ttest$risk_groups,
                   "T-TEST RESULTS - India Crime Analytics")

  addWorksheet(wb, "T-Tests Summary")
  writeData(wb, "T-Tests Summary", "T-TEST SUMMARY (All 5 Tests)", startRow=1)
  t_summary <- data.frame(
    `Test Name`   = c("One-Sample T-Test","Two-Sample Welch T-Test","Paired T-Test",
                      "Metro vs Non-Metro T-Test","Case Closure vs Violent T-Test"),
    `Hypothesis`  = c("Mean crime rate = national avg","High vs Low risk crime rates",
                      "2020 vs 2023 crime rates","Metro vs Non-Metro cities",
                      "High vs Low case closure violent ratio"),
    `T-Statistic` = c(tests$ttest$one_sample$t_statistic,
                      tests$ttest$risk_groups$t_statistic,
                      tests$ttest$paired_years$t_statistic,
                      tests$ttest$metro_nonmetro$t_statistic,
                      tests$ttest$case_closure$t_statistic),
    `P-Value`     = c(tests$ttest$one_sample$p_value,
                      tests$ttest$risk_groups$p_value,
                      tests$ttest$paired_years$p_value,
                      tests$ttest$metro_nonmetro$p_value,
                      tests$ttest$case_closure$p_value),
    `Significance`= c(tests$ttest$one_sample$significance,
                      tests$ttest$risk_groups$significance,
                      tests$ttest$paired_years$significance,
                      tests$ttest$metro_nonmetro$significance,
                      tests$ttest$case_closure$significance),
    `Decision`    = c(tests$ttest$one_sample$conclusion,
                      tests$ttest$risk_groups$conclusion,
                      tests$ttest$paired_years$conclusion,
                      tests$ttest$metro_nonmetro$conclusion,
                      tests$ttest$case_closure$conclusion),
    check.names = FALSE
  )
  writeDataTable(wb, "T-Tests Summary", t_summary, startRow=3, tableStyle="TableStyleMedium15")
  setColWidths(wb, "T-Tests Summary", 1:6, widths=c(32,45,14,12,14,55))

  # -- Chi-Square ---------------------------------------------------------------
  c_summary <- data.frame(
    `Test Name`   = c("Risk Tier x State","Case Closure x Crime Domain",
                      "Victim Gender x Crime Category","Weapon Use x City","Risk Tier x Year"),
    `Hypothesis`  = c("Risk tier independent of state","Closure independent of domain",
                      "Gender independent of crime category","Weapon use independent of city",
                      "Risk distribution stable over years"),
    `Chi-Sq`      = c(tests$chisq$risk_vs_state$chi_sq,
                      tests$chisq$closure_vs_domain$chi_sq,
                      tests$chisq$gender_vs_category$chi_sq,
                      tests$chisq$weapon_vs_city$chi_sq,
                      tests$chisq$risk_vs_year$chi_sq),
    `P-Value`     = c(tests$chisq$risk_vs_state$p_value,
                      tests$chisq$closure_vs_domain$p_value,
                      tests$chisq$gender_vs_category$p_value,
                      tests$chisq$weapon_vs_city$p_value,
                      tests$chisq$risk_vs_year$p_value),
    `Significance`= c(tests$chisq$risk_vs_state$significance,
                      tests$chisq$closure_vs_domain$significance,
                      tests$chisq$gender_vs_category$significance,
                      tests$chisq$weapon_vs_city$significance,
                      tests$chisq$risk_vs_year$significance),
    `Cramer V`    = c(tests$chisq$risk_vs_state$cramers_v,
                      tests$chisq$closure_vs_domain$cramers_v,
                      tests$chisq$gender_vs_category$cramers_v,
                      NA,NA),
    `Decision`    = c(tests$chisq$risk_vs_state$conclusion,
                      tests$chisq$closure_vs_domain$conclusion,
                      tests$chisq$gender_vs_category$conclusion,
                      tests$chisq$weapon_vs_city$conclusion,
                      tests$chisq$risk_vs_year$conclusion),
    check.names = FALSE
  )
  addWorksheet(wb, "Chi-Square Summary")
  writeData(wb, "Chi-Square Summary", "CHI-SQUARE TEST SUMMARY (All 5 Tests)", startRow=1)
  writeDataTable(wb, "Chi-Square Summary", c_summary, startRow=3, tableStyle="TableStyleMedium15")
  setColWidths(wb, "Chi-Square Summary", 1:7, widths=c(35,45,10,10,14,12,55))

  # -- ANOVA --------------------------------------------------------------------
  a_summary <- data.frame(
    `Test Name`   = c("Crime Rate ~ Risk Tier","Crime Rate ~ State",
                      "Violent % ~ Crime Domain","Victim Age ~ Crime Category",
                      "Police Deployed ~ Risk Tier"),
    `Hypothesis`  = c("Mean rate equal across risk tiers","Mean rate equal across states",
                      "Violent % equal across domains","Victim age equal across categories",
                      "Police deployed equal across risk tiers"),
    `F-Statistic` = round(c(tests$anova$rate_by_risk$result$f_statistic,
                      tests$anova$rate_by_state$result$f_statistic,
                      tests$anova$violent_by_domain$f_statistic,
                      tests$anova$age_by_category$result$f_statistic,
                      tests$anova$police_by_risk$f_statistic),4),
    `df (between)`= c(tests$anova$rate_by_risk$result$df_between,
                      tests$anova$rate_by_state$result$df_between,
                      tests$anova$violent_by_domain$df_between,
                      tests$anova$age_by_category$result$df_between,
                      tests$anova$police_by_risk$df_between),
    `P-Value`     = c(tests$anova$rate_by_risk$result$p_value,
                      tests$anova$rate_by_state$result$p_value,
                      tests$anova$violent_by_domain$p_value,
                      tests$anova$age_by_category$result$p_value,
                      tests$anova$police_by_risk$p_value),
    `Significance`= c(tests$anova$rate_by_risk$result$significance,
                      tests$anova$rate_by_state$result$significance,
                      tests$anova$violent_by_domain$significance,
                      tests$anova$age_by_category$result$significance,
                      tests$anova$police_by_risk$significance),
    `Eta^2`        = c(tests$anova$rate_by_risk$result$eta_squared,
                      tests$anova$rate_by_state$result$eta_squared,
                      tests$anova$violent_by_domain$eta_squared,
                      tests$anova$age_by_category$result$eta_squared,
                      tests$anova$police_by_risk$eta_squared),
    `Decision`    = c(tests$anova$rate_by_risk$result$conclusion,
                      tests$anova$rate_by_state$result$conclusion,
                      tests$anova$violent_by_domain$conclusion,
                      tests$anova$age_by_category$result$conclusion,
                      tests$anova$police_by_risk$conclusion),
    check.names = FALSE
  )
  addWorksheet(wb, "ANOVA Summary")
  writeData(wb, "ANOVA Summary", "ONE-WAY ANOVA SUMMARY (All 5 Tests)", startRow=1)
  writeDataTable(wb, "ANOVA Summary", a_summary, startRow=3, tableStyle="TableStyleMedium15")
  setColWidths(wb, "ANOVA Summary", 1:8, widths=c(30,42,14,14,10,14,8,55))

  # -- Tukey HSD ----------------------------------------------------------------
  if (!is.null(tests$anova$rate_by_risk$tukey)) {
    addWorksheet(wb, "Tukey HSD (Risk Tiers)")
    writeData(wb, "Tukey HSD (Risk Tiers)",
              "TUKEY HSD POST-HOC: Crime Rate ~ Risk Tier", startRow=1)
    writeDataTable(wb, "Tukey HSD (Risk Tiers)",
                   tests$anova$rate_by_risk$tukey, startRow=3, tableStyle="TableStyleMedium15")
    setColWidths(wb, "Tukey HSD (Risk Tiers)", 1:6, widths=c(22,12,12,12,14,14))
  }

  # -- State Means ---------------------------------------------------------------
  addWorksheet(wb, "State Crime Means")
  writeData(wb, "State Crime Means", "ANOVA: State-wise Mean Crime Rate", startRow=1)
  writeDataTable(wb, "State Crime Means",
                 tests$anova$rate_by_state$state_means, startRow=3, tableStyle="TableStyleMedium15")
  setColWidths(wb, "State Crime Means", 1:3, widths=c(30,16,10))

  fp <- file.path(output_dir, paste0("statistical_tests_", format(Sys.Date(),"%Y%m%d"), ".xlsx"))
  saveWorkbook(wb, fp, overwrite = TRUE)
  message("[INFO] Stats Excel saved: ", fp)
  fp
}
