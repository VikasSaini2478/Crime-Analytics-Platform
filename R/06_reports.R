# =============================================================================
# 06_reports.R - Excel & HTML report generation
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(openxlsx) })
source("R/00_utils.R")

generate_excel_report <- function(master_data, feature_data, model_results,
                                   xai_data, output_dir="reports") {
  log_info("Generating Excel report")
  dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  wb <- createWorkbook()
  hs <- createStyle(fontName="Calibri",fontSize=11,fontColour="#FFFFFF",
                    bgFill="#1a1f36",halign="CENTER",textDecoration="Bold",
                    border="Bottom",borderColour="#58a6ff",borderStyle="medium")

  ly <- max(feature_data$city$year,na.rm=TRUE)
  lc <- feature_data$city %>% filter(year==ly)

  # Sheet 1: Executive Summary
  addWorksheet(wb,"Executive Summary")
  sum_df <- data.frame(
    Metric=c("Report Generated","Data Period","Cities","Total Crimes",
             "Avg Crime Rate/Lakh","High Risk Cities","Best Model","RMSE","R^2"),
    Value=c(format(Sys.time(),"%Y-%m-%d %H:%M"),
            paste0(min(feature_data$city$year)," - ",ly),
            n_distinct(feature_data$city$city),
            format(sum(lc$total_crimes,na.rm=TRUE),big.mark=","),
            round(mean(lc$crime_rate_per_lakh,na.rm=TRUE),2),
            sum(lc$risk_tier %in% c("CRITICAL","HIGH"),na.rm=TRUE),
            model_results$selection$best_model_name,
            model_results$selection$comparison$rmse[1],
            model_results$selection$comparison$r_squared[1]),
    stringsAsFactors=FALSE
  )
  writeData(wb,"Executive Summary","INDIA CRIME ANALYTICS - EXECUTIVE SUMMARY",
            startRow=1,startCol=1)
  addStyle(wb,"Executive Summary",createStyle(fontSize=16,textDecoration="Bold",
           fontColour="#1a1f36"),rows=1,cols=1)
  writeDataTable(wb,"Executive Summary",sum_df,startRow=3,tableStyle="TableStyleMedium15")
  setColWidths(wb,"Executive Summary",1:2,widths=c(35,40))

  # Sheet 2: City Data
  addWorksheet(wb,"City Crime Data")
  city_exp <- feature_data$city %>%
    select(city,year,total_crimes,crime_rate_per_lakh,violent_crimes,
           women_crimes,cyber_crimes,property_crimes,case_closure_rate,
           crime_growth_rate,rolling_avg_3yr,risk_score,risk_tier) %>%
    mutate(across(where(is.numeric),~round(.,2)))
  writeDataTable(wb,"City Crime Data",city_exp,tableStyle="TableStyleMedium15")
  setColWidths(wb,"City Crime Data",1:ncol(city_exp),widths=rep(16,ncol(city_exp)))

  # Sheet 3: State Data
  addWorksheet(wb,"State Crime Data")
  writeDataTable(wb,"State Crime Data",
    feature_data$state %>% mutate(across(where(is.numeric),~round(.,2))),
    tableStyle="TableStyleMedium15")

  # Sheet 4: Forecasts
  addWorksheet(wb,"Forecasts 2025-2030")
  writeDataTable(wb,"Forecasts 2025-2030",
    model_results$city_forecasts %>% mutate(across(where(is.numeric),~round(.,2))),
    tableStyle="TableStyleMedium15")

  # Sheet 5: Model Metrics
  addWorksheet(wb,"Model Evaluation")
  writeDataTable(wb,"Model Evaluation",
    model_results$selection$comparison %>% mutate(across(where(is.numeric),~round(.,4))),
    tableStyle="TableStyleMedium15")

  # Sheet 6: Feature Importance
  addWorksheet(wb,"Feature Importance")
  writeDataTable(wb,"Feature Importance",
    xai_data$importance$consensus %>% mutate(across(where(is.numeric),~round(.,3))),
    tableStyle="TableStyleMedium15")

  # Sheet 7: Anomalies
  addWorksheet(wb,"Anomalies")
  if(nrow(xai_data$anomalies)>0)
    writeDataTable(wb,"Anomalies",
      xai_data$anomalies %>% mutate(across(where(is.numeric),~round(.,2))),
      tableStyle="TableStyleMedium15")

  fp <- file.path(output_dir,paste0("crime_analytics_",format(Sys.Date(),"%Y%m%d"),".xlsx"))
  saveWorkbook(wb,fp,overwrite=TRUE)
  message("[INFO] ", "Excel saved: ", fp)
  fp
}

generate_html_summary <- function(master_data, feature_data, model_results,
                                   output_dir="reports") {
  dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  ly <- max(feature_data$city$year,na.rm=TRUE)
  lc <- feature_data$city %>% filter(year==ly)
  top10 <- lc %>% arrange(desc(crime_rate_per_lakh)) %>% head(10) %>%
    mutate(across(where(is.numeric),~round(.,1)))
  top10_html <- paste(apply(top10,1,function(r)
    sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s%%</td></tr>",
            r["city"],r["crime_rate_per_lakh"],r["risk_tier"],r["crime_growth_rate"])),
    collapse="\n")
  model_html <- paste(apply(model_results$selection$comparison,1,function(r)
    sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
            r["model"],r["rmse"],r["mae"],r["r_squared"])), collapse="\n")

  html <- sprintf('<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8">
<title>India Crime Analytics Report</title>
<style>
body{font-family:"Segoe UI",sans-serif;background:#0d1117;color:#e6edf3;margin:0;padding:20px}
.hdr{background:linear-gradient(135deg,#1c2d5e,#2d1b69);padding:36px;border-radius:12px;margin-bottom:24px}
.hdr h1{margin:0;font-size:1.9em}.hdr p{color:#8b949e;margin:6px 0 0}
.kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin-bottom:24px}
.kpi{background:#161b22;border:1px solid #30363d;border-radius:10px;padding:20px}
.kpi .lbl{font-size:11px;text-transform:uppercase;letter-spacing:.05em;color:#8b949e}
.kpi .val{font-size:2em;font-weight:800;margin-top:6px}
.bl{color:#58a6ff}.pu{color:#a371f7}.rd{color:#f85149}.gr{color:#3fb950}
.sec{background:#161b22;border:1px solid #30363d;border-radius:10px;padding:24px;margin-bottom:20px}
.sec h2{margin:0 0 16px;font-size:1.05em;color:#58a6ff}
table{width:100%%;border-collapse:collapse}
th{background:#1c2128;padding:10px 14px;text-align:left;color:#58a6ff;font-size:.82em;text-transform:uppercase}
td{padding:9px 14px;border-bottom:1px solid #21262d;font-size:.88em}
tr:last-child td{border:none}
.ftr{text-align:center;color:#8b949e;font-size:.78em;margin-top:30px}
</style></head><body>
<div class="hdr"><h1>[SHIELD] India Crime Analytics Platform</h1>
<p>Comprehensive Intelligence Report | Generated: %s</p></div>
<div class="kpis">
<div class="kpi"><div class="lbl">Total Crimes</div><div class="val bl">%s</div></div>
<div class="kpi"><div class="lbl">Avg Rate/Lakh</div><div class="val pu">%s</div></div>
<div class="kpi"><div class="lbl">High Risk Cities</div><div class="val rd">%s</div></div>
<div class="kpi"><div class="lbl">Best Model R^2</div><div class="val gr">%s</div></div>
</div>
<div class="sec"><h2>[CHART] Top 10 Cities by Crime Rate</h2>
<table><thead><tr><th>City</th><th>Rate/Lakh</th><th>Risk Tier</th><th>Growth</th></tr></thead>
<tbody>%s</tbody></table></div>
<div class="sec"><h2>[AI] Model Evaluation</h2>
<table><thead><tr><th>Model</th><th>RMSE</th><th>MAE</th><th>R^2</th></tr></thead>
<tbody>%s</tbody></table></div>
<div class="ftr">Crime Analytics Platform v2.0 | Data: 2020-2023 | %s</div>
</body></html>',
    format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
    format(sum(lc$total_crimes,na.rm=TRUE),big.mark=","),
    round(mean(lc$crime_rate_per_lakh,na.rm=TRUE),1),
    sum(lc$risk_tier %in% c("CRITICAL","HIGH"),na.rm=TRUE),
    model_results$selection$comparison$r_squared[1],
    top10_html, model_html,
    format(Sys.Date(),"%B %Y")
  )
  fp <- file.path(output_dir,paste0("crime_report_",format(Sys.Date(),"%Y%m%d"),".html"))
  writeLines(html,fp)
  message("[INFO] ", "HTML saved: ", fp)
  fp
}
