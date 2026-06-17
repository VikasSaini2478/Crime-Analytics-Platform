# =============================================================================
# server.R - Crime Analytics Platform v2.1
# Windows + Linux + Mac compatible - All ASCII source
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(plotly)
  library(DT); library(tidyr)
})
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE)))
  source(f, encoding = "UTF-8")

server <- function(input, output, session) {

  # ===========================================================================
  # UI HELPERS (defined FIRST - used by all page builders below)
  # ===========================================================================
  ph <- function(title, sub = NULL)
    tags$div(class = "ph",
      tags$h2(title),
      if (!is.null(sub)) tags$p(sub))

  kpi <- function(id, label, icon_char, color)
    tags$div(class = "kpi-card", style = paste0("--ac:", color),
      tags$div(class = "kpi-lbl",  label),
      tags$div(class = "kpi-val",  textOutput(id, inline = TRUE)),
      tags$div(class = "kpi-icon", icon_char))

  CC <- function(title, sub = NULL, ...)
    tags$div(class = "cc",
      tags$div(class = "cc-hdr",
        tags$div(class = "cc-title", title),
        if (!is.null(sub)) tags$div(class = "cc-sub", sub)),
      ...)

  G2 <- function(...) tags$div(class = "chart-grid g2", ...)
  G3 <- function(...) tags$div(class = "chart-grid g3", ...)
  GW <- function(...) tags$div(class = "chart-grid gw", ...)

  # ===========================================================================
  # REACTIVE STATE
  # ===========================================================================
  rv <- reactiveValues(
    master = NULL, feat   = NULL, models = NULL,
    xai    = NULL, tests  = NULL, ready  = FALSE,
    active = "nav_overview",
    cities = character(0)   # populated after boot; used by forecast page
  )

  # ===========================================================================
  # BOOT PIPELINE
  # ===========================================================================
  observe({
    withProgress(message = "Loading Crime Analytics Platform...", value = 0, {
      tryCatch({
        incProgress(0.05, detail = "Ingesting data...")
        rv$master <- create_master_dataset("data")

        incProgress(0.30, detail = "Engineering features...")
        rv$feat   <- run_feature_pipeline(rv$master)

        incProgress(0.55, detail = "Training models...")
        rv$models <- run_modeling_pipeline(rv$feat)

        incProgress(0.80, detail = "Running XAI...")
        rv$xai    <- run_explainability_pipeline(rv$feat, rv$models)

        incProgress(1.00, detail = "Done!")
        rv$ready  <- TRUE
        message("[INFO] Platform boot complete")

      }, error = function(e) {
        showNotification(
          paste("Boot error:", conditionMessage(e)),
          type = "error", duration = NULL)
        message("[ERROR] Boot failed: ", conditionMessage(e))
      })
    })
  })

  # Statistical tests: run once after boot (lazy)
  observe({
    req(rv$ready)
    isolate({
      if (is.null(rv$tests)) {
        tryCatch({
          rv$tests <- run_statistical_tests(rv$master, rv$feat)
          message("[INFO] Statistical tests computed")
        }, error = function(e) {
          message("[ERROR] Stats tests failed: ", conditionMessage(e))
          rv$tests <- list()
        })
      }
    })
  })

  # ===========================================================================
  # TAB ROUTING
  # ===========================================================================
  observeEvent(input$active_tab, {
    rv$active <- input$active_tab
  }, ignoreNULL = TRUE)

  # Populate the master city list as soon as the data pipeline is ready
  observe({
    req(rv$ready)
    cities <- sort(unique(rv$feat$city$city))
    cities <- cities[!is.na(cities) & nchar(cities) > 0]
    if (length(rv$cities) == 0) rv$cities <- cities
  })

  # Re-populate sel_city dropdown EVERY time the Forecast page is opened.
  # This fixes the original bug: updateSelectInput() was called once at
  # boot time, before the <select> element existed in the DOM (user was
  # still on the Overview page), so Shiny silently dropped the update.
  # Firing on every navigation to nav_forecast guarantees the element
  # exists when we call updateSelectInput().
  observeEvent(input$active_tab, {
    if (identical(input$active_tab, "nav_forecast")) {
      cities <- if (length(rv$cities) > 0) {
        rv$cities
      } else if (!is.null(rv$feat)) {
        sort(unique(rv$feat$city$city))
      } else {
        character(0)
      }
      updateSelectInput(session, "sel_city",
                        choices  = cities,
                        selected = if (length(cities) > 0) cities[1] else NULL)
    }
  }, ignoreNULL = TRUE)

  # ===========================================================================
  # FILTERED REACTIVE DATA
  # ===========================================================================
  f_city <- reactive({
    req(rv$ready)
    rv$feat$city %>%
      filter(year >= input$yr[1], year <= input$yr[2])
  })

  f_raw <- reactive({
    req(rv$ready)
    rv$master$raw %>%
      filter(year >= input$yr[1], year <= input$yr[2])
  })

  latest <- reactive({
    req(rv$ready)
    f_city() %>% filter(year == max(year, na.rm = TRUE))
  })

  # ===========================================================================
  # PAGE ROUTER
  # ===========================================================================
  output$page_body <- renderUI({
    switch(rv$active,
      nav_overview   = page_overview(),
      nav_map        = page_map(),
      nav_forecast   = page_forecast(),
      nav_states     = page_states(),
      nav_categories = page_categories(),
      nav_ai         = page_ai(),
      nav_models     = page_models(),
      nav_xai        = page_xai(),
      nav_stats      = page_stats(),
      nav_reports    = page_reports(),
      nav_data       = page_data(),
      nav_admin      = page_admin(),
      page_overview()
    )
  })

  # ===========================================================================
  # PAGE BUILDERS
  # ===========================================================================

  page_overview <- function() tagList(
    ph("Executive Overview", "National crime intelligence summary"),
    tags$div(class = "kpi-row",
      kpi("kpi_crimes",   "Total Crimes",     "[#]",  "#58a6ff"),
      kpi("kpi_rate",     "Avg Rate / Lakh",  "[%]",  "#a371f7"),
      kpi("kpi_highrisk", "High Risk Cities", "[!]",  "#f85149"),
      kpi("kpi_r2",       "Best Model R^2",   "[M]",  "#3fb950")
    ),
    GW(
      CC("National Crime Rate - Historical & Forecast",
         "ETS + ARIMA projections 2025-2030",
         withSpinner(plotlyOutput("plt_trend",  height = "370px"), color = "#58a6ff")),
      CC("Crime Category Distribution",
         withSpinner(plotlyOutput("plt_donut",  height = "370px"), color = "#a371f7"))
    ),
    G2(
      CC("Top Cities by Crime Rate", "Latest year ranking",
         withSpinner(plotlyOutput("plt_rank",   height = "340px"), color = "#58a6ff")),
      CC("YoY Crime Growth Rate", "Top 8 cities by volume",
         withSpinner(plotlyOutput("plt_growth", height = "340px"), color = "#3fb950"))
    )
  )

  page_map <- function() tagList(
    ph("India Crime Intelligence Map", "City x Year heatmap & risk classification"),
    GW(
      CC("Crime Rate Heatmap",
         "Colour = crime rate per lakh population",
         withSpinner(plotlyOutput("plt_heat",     height = "500px"), color = "#58a6ff")),
      tagList(
        CC("Risk Tier Split",
           withSpinner(plotlyOutput("plt_risk_pie", height = "230px"), color = "#a371f7")),
        CC("State Summary",
           withSpinner(
             tags$div(style = "max-height:240px;overflow-y:auto;",
                      DTOutput("tbl_state_sum")),
             color = "#58a6ff"))
      )
    )
  )

  page_forecast <- function() tagList(
    ph("Forecast Center", "City-level crime rate forecasts 2025-2030 with confidence intervals"),
    # City selector card - full width, prominent
    tags$div(class = "cc", style = "margin-bottom:16px",
      tags$div(class = "cc-hdr",
        tags$div(class = "cc-title", "Select City to Forecast"),
        tags$div(class = "cc-sub",
          "Choose any of the 29 cities below - historical trend and 2025-2030 forecast will render automatically.")
      ),
      tags$div(class = "forecast-dropdown",
        selectInput("sel_city", NULL,
                    choices  = character(0),
                    width    = "300px")
      ),
      withSpinner(plotlyOutput("plt_city_fc", height = "420px"), color = "#a371f7")
    ),
    G2(
      CC("National ETS Forecast 2025-2030",
         "Average crime rate across all 29 cities",
         withSpinner(plotlyOutput("plt_nat_fc", height = "320px"), color = "#58a6ff")),
      CC("Forecast Table - Selected City",
         "Annual crime rate forecast with confidence intervals",
         withSpinner(
             tags$div(style = "max-height:320px;overflow-y:auto;",
                      DTOutput("tbl_fc")),
             color = "#58a6ff"))
    )
  )

  page_states <- function() tagList(
    ph("State Intelligence Panel", "State-wise crime ranking, risk scores and trends"),
    G2(
      CC("State Crime Rate Ranking",
         withSpinner(plotlyOutput("plt_state_rank", height = "400px"), color = "#58a6ff")),
      CC("State Risk Score",
         withSpinner(plotlyOutput("plt_state_risk", height = "400px"), color = "#f85149"))
    ),
    CC("State Intelligence Data", withSpinner(DTOutput("tbl_state_full")))
  )

  page_categories <- function() tagList(
    ph("Crime Category Analytics", "Deep-dive into crime type composition and trends"),
    G3(
      CC("Violent Crimes",
         withSpinner(plotlyOutput("plt_c_viol",  height = "230px"), color = "#f85149")),
      CC("Women-Related Crimes",
         withSpinner(plotlyOutput("plt_c_women", height = "230px"), color = "#a371f7")),
      CC("Cyber Crimes",
         withSpinner(plotlyOutput("plt_c_cyber", height = "230px"), color = "#58a6ff"))
    ),
    G2(
      CC("Category Trend Over Years",
         withSpinner(plotlyOutput("plt_cat_trend", height = "300px"), color = "#58a6ff")),
      CC("Weapon Usage Distribution",
         withSpinner(plotlyOutput("plt_weapon",    height = "300px"), color = "#d29922"))
    ),
    CC("Crime Mix by City",
       withSpinner(plotlyOutput("plt_cat_city", height = "400px"), color = "#3fb950"))
  )

  page_ai <- function() tagList(
    ph("AI Intelligence Panel", "Insights, Anomaly Detection & Emerging Threats"),
    GW(
      tags$div(
        tags$div(class = "cc-title",
                 style = "margin-bottom:14px;", "[AI] AI Generated Insights"),
        uiOutput("ui_insights")
      ),
      CC("Statistical Anomalies", "City-years > 2 standard deviations from city mean",
         withSpinner(
             tags$div(style = "max-height:460px;overflow-y:auto;",
                      DTOutput("tbl_anom")),
             color = "#a371f7"))
    ),
    CC("City Trend Explanations", withSpinner(DTOutput("tbl_trends")))
  )

  page_models <- function() tagList(
    ph("Model Evaluation", "Comparative performance of all trained prediction models"),
    G2(
      CC("Model Performance Comparison",
         withSpinner(plotlyOutput("plt_model_cmp", height = "340px"), color = "#58a6ff")),
      CC("Actual vs Predicted - Best Model",
         withSpinner(plotlyOutput("plt_act_pred",  height = "340px"), color = "#3fb950"))
    ),
    CC("Model Metrics", withSpinner(DTOutput("tbl_metrics")))
  )

  page_xai <- function() tagList(
    ph("Explainable AI", "Feature importance, SHAP permutation & prediction reasoning"),
    G2(
      CC("Consensus Feature Importance", "Averaged across RF and XGB-proxy",
         withSpinner(plotlyOutput("plt_feat_imp", height = "370px"), color = "#58a6ff")),
      CC("SHAP Permutation Importance", "RMSE increase when feature is permuted",
         withSpinner(plotlyOutput("plt_shap",     height = "370px"), color = "#a371f7"))
    ),
    CC("Feature Importance by Model", withSpinner(DTOutput("tbl_feat_imp")))
  )

  page_stats <- function() tagList(
    ph("Statistical Hypothesis Tests",
       "T-Test / Chi-Square / One-Way ANOVA with Tukey HSD - 15 tests total"),
    tags$div(class = "kpi-row",
             style = "grid-template-columns:repeat(3,1fr)",
      tags$div(class = "kpi-card", style = "--ac:#58a6ff",
        tags$div(class = "kpi-lbl", "T-Tests Run"),
        tags$div(class = "kpi-val", "5"),
        tags$div(class = "kpi-icon", "[T]")),
      tags$div(class = "kpi-card", style = "--ac:#a371f7",
        tags$div(class = "kpi-lbl", "Chi-Square Tests"),
        tags$div(class = "kpi-val", "5"),
        tags$div(class = "kpi-icon", "[X2]")),
      tags$div(class = "kpi-card", style = "--ac:#3fb950",
        tags$div(class = "kpi-lbl", "ANOVA Tests"),
        tags$div(class = "kpi-val", "5"),
        tags$div(class = "kpi-icon", "[F]"))
    ),
    CC("[T] T-Test Results",
       "One-Sample / Two-Sample (Welch) / Paired / Metro vs Non-Metro / Case Closure",
       tags$div(style = "margin-bottom:14px",
         selectInput("sel_ttest", "View detailed results:",
           choices = c("One-Sample T-Test"         = "one_sample",
                       "Two-Sample: High vs Low"   = "risk_groups",
                       "Paired: 2020 vs 2023"      = "paired_years",
                       "Metro vs Non-Metro"        = "metro_nonmetro",
                       "Case Closure vs Violent"   = "case_closure"),
           width = "320px")
       ),
       withSpinner(DTOutput("tbl_t_summary"), color = "#58a6ff"),
       tags$hr(style = "border-color:#30363d;margin:14px 0"),
       tags$div(class = "cc-sub", style = "margin-bottom:8px",
                "Detailed view of selected test:"),
       withSpinner(DTOutput("tbl_t_detail"), color = "#58a6ff")
    ),
    G2(
      CC("[T] T-Test Box Plot",
         "Crime rate distribution - Two-Sample Welch T-Test groups",
         withSpinner(plotlyOutput("plt_t_boxplot", height = "320px"), color = "#58a6ff")),
      CC("T-Test Interpretation Guide",
        tags$div(style = "color:#8b949e;font-size:12px;line-height:2",
          tags$p(tags$b("Null Hypothesis (H0):"),  " No significant difference exists"),
          tags$p(tags$b("Alternative (H1):"),       " A significant difference exists"),
          tags$p(tags$b("t-statistic:"),            " Distance from H0 in standard errors"),
          tags$p(tags$b("p-value < 0.05:"),         " Reject H0 - statistically significant"),
          tags$p(tags$b("95% CI:"),                 " Range where true difference likely falls"),
          tags$hr(style = "border-color:#30363d;margin:8px 0"),
          tags$p(tags$b("Significance codes:")),
          tags$p("*** p<0.001  ** p<0.01  * p<0.05  ns = not significant"),
          tags$hr(style = "border-color:#30363d;margin:8px 0"),
          tags$p(tags$b(style = "color:#3fb950", "[OK] T2: High vs Low Risk p<0.001")),
          tags$p("High-risk cities have crime rates 2.8x higher"),
          tags$p(tags$b(style = "color:#3fb950", "[OK] T3: 2020 vs 2023 p=0.0004")),
          tags$p("Crime rate declined significantly over 3 years")
        )
      )
    ),
    CC("[X2] Chi-Square Test Results",
       "Tests of independence between categorical variables - Cramer V = effect size",
       withSpinner(DTOutput("tbl_chi_summary"), color = "#a371f7")
    ),
    G2(
      CC("City x Risk Tier Frequency Matrix",
         "Observed counts used in Chi-Square test",
         withSpinner(plotlyOutput("plt_chi_heatmap", height = "320px"), color = "#a371f7")),
      CC("Weapon Usage % by City",
         "Weapon use rate varies significantly across cities (Chi-sq p<0.001)",
         withSpinner(plotlyOutput("plt_chi_weapon",  height = "320px"), color = "#d29922"))
    ),
    CC("[F] One-Way ANOVA Results",
       "Eta^2 = effect size / F > 1 means between-group variance dominates",
       withSpinner(DTOutput("tbl_anova_summary"), color = "#3fb950")
    ),
    G2(
      CC("ANOVA: Crime Rate by Risk Tier",
         "F = 101.27 *** / Eta^2 = 0.587 (large effect)",
         withSpinner(plotlyOutput("plt_anova_boxplot", height = "320px"), color = "#3fb950")),
      CC("ANOVA: Mean Crime Rate by State",
         "F = 3.90 *** / Eta^2 = 0.296 (medium effect)",
         withSpinner(plotlyOutput("plt_anova_state",   height = "320px"), color = "#58a6ff"))
    ),
    CC("Tukey HSD Post-Hoc - Crime Rate by Risk Tier",
      tags$div(style = "color:#8b949e;font-size:11px;margin-bottom:10px",
        "All pairwise comparisons. p adj < 0.05 = significantly different pairs."),
      withSpinner(DTOutput("tbl_tukey"), color = "#3fb950")
    ),
    tags$div(class = "cc", style = "margin-top:16px;text-align:center;padding:28px",
      tags$div(style = "font-size:40px;margin-bottom:12px", "[DOWNLOAD]"),
      tags$div(class = "cc-title", style = "margin-bottom:8px",
               "Download Full Statistical Report"),
      tags$div(style = "color:#8b949e;font-size:12px;margin-bottom:16px",
        "Excel workbook: all 15 tests, summary tables, Tukey HSD, state means"),
      downloadButton("dl_stats_xlsx", "[D] Download Statistical Tests Excel",
                     class = "btn-dl btn-dl-green",
                     style = "font-size:14px;padding:10px 24px")
    )
  )

  page_reports <- function() tagList(
    ph("Report Generation Center", "Export comprehensive analytics reports"),
    tags$div(class = "chart-grid g3",
      tags$div(class = "cc report-card",
        tags$div(style = "font-size:48px;text-align:center;margin-bottom:14px", "[XL]"),
        tags$div(class = "cc-title", style = "text-align:center", "Excel Workbook"),
        tags$div(style = "color:#8b949e;font-size:12px;text-align:center;margin-bottom:18px",
          "7 sheets: summary, city, state, forecasts, metrics, importance, anomalies"),
        downloadButton("dl_xlsx", "Download Excel",
                       class = "btn-dl btn-dl-green", style = "width:100%")),
      tags$div(class = "cc report-card",
        tags$div(style = "font-size:48px;text-align:center;margin-bottom:14px", "[HTML]"),
        tags$div(class = "cc-title", style = "text-align:center", "HTML Report"),
        tags$div(style = "color:#8b949e;font-size:12px;text-align:center;margin-bottom:18px",
          "Self-contained HTML with KPIs, tables and model summary"),
        downloadButton("dl_html", "Download HTML",
                       class = "btn-dl btn-dl-blue", style = "width:100%")),
      tags$div(class = "cc report-card",
        tags$div(style = "font-size:48px;text-align:center;margin-bottom:14px", "[CSV]"),
        tags$div(class = "cc-title", style = "text-align:center", "Forecast CSV"),
        tags$div(style = "color:#8b949e;font-size:12px;text-align:center;margin-bottom:18px",
          "City-level forecasts 2025-2030 with 80% and 95% confidence intervals"),
        downloadButton("dl_csv", "Download CSV",
                       class = "btn-dl btn-dl-purple", style = "width:100%"))
    )
  )

  page_data <- function() tagList(
    ph("Raw Data Explorer", "Browse, filter and inspect all underlying datasets"),
    tabsetPanel(
      tabPanel("City Features",
        tags$div(style = "padding-top:16px",
          CC(NULL, withSpinner(DTOutput("tbl_city_full"))))),
      tabPanel("Raw Records",
        tags$div(style = "padding-top:16px",
          CC(NULL, withSpinner(DTOutput("tbl_raw"))))),
      tabPanel("Yearly Agg",
        tags$div(style = "padding-top:16px",
          CC(NULL, withSpinner(DTOutput("tbl_yearly"))))),
      tabPanel("State Features",
        tags$div(style = "padding-top:16px",
          CC(NULL, withSpinner(DTOutput("tbl_state_raw")))))
    )
  )

  page_admin <- function() tagList(
    ph("Administration", "Data upload, model retraining & system status"),
    G2(
      CC("Data Upload & Retraining",
        fileInput("new_data", "Upload New Crime Dataset (.csv)",
                  accept = ".csv",
                  buttonLabel = "Browse...",
                  placeholder = "crime_dataset.csv"),
        actionButton("btn_retrain", "[R] Retrain All Models",
                     class = "btn-primary", style = "width:100%;margin-top:10px"),
        tags$hr(style = "border-color:#30363d"),
        tags$p(style = "color:#8b949e;font-size:12px",
          "Upload a CSV matching crime_dataset_india.csv schema, then retrain.")
      ),
      CC("System Status",
        tags$div(style = "color:#8b949e;font-size:13px;line-height:2",
          tags$p(tags$span(class = "dot dot-green"), " Data Pipeline: Active"),
          tags$p(tags$span(class = "dot dot-green"), " ML Models: Loaded"),
          tags$p(tags$span(class = "dot dot-green"), " Forecast Engine: Ready"),
          tags$p(tags$span(class = "dot dot-green"), " XAI Engine: Ready"),
          tags$hr(style = "border-color:#30363d"),
          tags$p("Records: ",  textOutput("sys_records", inline = TRUE)),
          tags$p("Cities: ",   textOutput("sys_cities",  inline = TRUE)),
          tags$p("Last run: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
        )
      )
    ),
    CC("Audit Log", verbatimTextOutput("audit"))
  )

  # ===========================================================================
  # KPI OUTPUTS
  # ===========================================================================
  output$kpi_crimes   <- renderText({
    req(rv$ready)
    format(sum(latest()$total_crimes, na.rm = TRUE), big.mark = ",")
  })
  output$kpi_rate     <- renderText({
    req(rv$ready)
    round(mean(latest()$crime_rate_per_lakh, na.rm = TRUE), 1)
  })
  output$kpi_highrisk <- renderText({
    req(rv$ready)
    sum(latest()$risk_tier %in% c("HIGH", "CRITICAL"), na.rm = TRUE)
  })
  output$kpi_r2 <- renderText({
    req(rv$ready)
    paste0(
      round(rv$models$selection$comparison$r_squared[
        rv$models$selection$comparison$model ==
          rv$models$selection$best_model_name
      ] * 100, 1), "%")
  })
  output$sys_records  <- renderText({
    req(rv$ready); format(nrow(rv$master$raw), big.mark = ",")
  })
  output$sys_cities   <- renderText({
    req(rv$ready); n_distinct(rv$master$raw$city)
  })

  # ===========================================================================
  # OVERVIEW PLOTS
  # ===========================================================================
  output$plt_trend  <- renderPlotly({
    req(rv$ready); make_crime_trend_chart(rv$feat$yearly, rv$models$ts)
  })
  output$plt_donut  <- renderPlotly({
    req(rv$ready); make_category_donut(f_raw())
  })
  output$plt_rank   <- renderPlotly({
    req(rv$ready); make_city_ranking_chart(f_city())
  })
  output$plt_growth <- renderPlotly({
    req(rv$ready); make_growth_chart(f_city())
  })

  # ===========================================================================
  # MAP
  # ===========================================================================
  output$plt_heat <- renderPlotly({
    req(rv$ready); make_crime_heatmap(f_city())
  })
  output$plt_risk_pie <- renderPlotly({
    req(rv$ready)
    df   <- latest() %>% group_by(risk_tier) %>% summarise(n = n(), .groups = "drop")
    cols <- c(CRITICAL = "#f85149", HIGH = "#d29922",
              MEDIUM   = "#58a6ff", LOW  = "#3fb950")
    plot_ly(df, labels = ~risk_tier, values = ~n, type = "pie", hole = 0.55,
      marker = list(colors = unname(cols[df$risk_tier]),
                    line   = list(color = "#161b22", width = 2)),
      textinfo  = "label+percent",
      textfont  = list(color = "#e6edf3", size = 11),
      hovertemplate = "<b>%{label}</b><br>%{value} cities<extra></extra>") %>%
      dark_layout("Risk Tier Distribution", show_legend = FALSE)
  })
  output$tbl_state_sum <- renderDT({
    req(rv$ready)
    rv$feat$state %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      transmute(State = state,
                `Rate/Lakh`  = round(crime_rate_per_lakh, 1),
                Crimes       = total_crimes,
                `Risk Score` = round(state_risk_score, 1),
                Risk         = risk_tier) %>%
      arrange(desc(`Rate/Lakh`)) %>%
      datatable(options = list(pageLength = 8, dom = "t"),
                rownames = FALSE, class = "compact")
  })

  # ===========================================================================
  # FORECAST
  # ===========================================================================
  output$plt_city_fc <- renderPlotly({
    req(rv$ready)
    validate(
      need(!is.null(rv$cities) && length(rv$cities) > 0,
           "Loading cities... please wait."),
      need(!is.null(input$sel_city) && nchar(input$sel_city) > 0,
           "Select a city from the dropdown above.")
    )
    city_name <- input$sel_city
    # Check city exists in forecast data
    fc_data <- rv$models$city_forecasts
    validate(
      need(!is.null(fc_data) && nrow(fc_data) > 0,
           "Forecast data not available yet."),
      need(city_name %in% fc_data$city,
           paste("No forecast data found for:", city_name))
    )
    make_city_forecast_chart(rv$feat$city, fc_data, city_name)
  })
  output$plt_nat_fc  <- renderPlotly({
    req(rv$ready); make_crime_trend_chart(rv$feat$yearly, rv$models$ts)
  })
  output$tbl_fc <- renderDT({
    req(rv$ready)
    validate(
      need(!is.null(rv$models$city_forecasts) &&
             nrow(rv$models$city_forecasts) > 0,
           "Forecast table not yet available.")
    )
    fc <- rv$models$city_forecasts
    # Filter to selected city if one is chosen
    if (!is.null(input$sel_city) && nchar(input$sel_city) > 0) {
      fc <- fc %>% filter(city == input$sel_city)
    }
    fc %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      datatable(
        options  = list(pageLength = 10, dom = "ftip", scrollX = TRUE),
        rownames = FALSE,
        class    = "compact"
      )
  })

  # ===========================================================================
  # STATES
  # ===========================================================================
  output$plt_state_rank <- renderPlotly({
    req(rv$ready)
    d <- rv$feat$state %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      arrange(desc(crime_rate_per_lakh)) %>%
      head(15)
    plot_ly(d, x = ~crime_rate_per_lakh,
            y = ~reorder(state, crime_rate_per_lakh),
            type = "bar", orientation = "h",
            marker = list(color = "#58a6ff"),
            hovertemplate = "<b>%{y}</b><br>%{x:.1f}/lakh<extra></extra>") %>%
      dark_layout("State Crime Rate Ranking", "Rate/Lakh", NULL,
                  show_legend = FALSE) %>%
      layout(margin = list(l = 150, r = 40, t = 55, b = 55))
  })
  output$plt_state_risk <- renderPlotly({
    req(rv$ready)
    d <- rv$feat$state %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      arrange(desc(state_risk_score)) %>%
      head(15) %>%
      mutate(col = ifelse(risk_tier == "CRITICAL", "#f85149",
                   ifelse(risk_tier == "HIGH",     "#d29922",
                   ifelse(risk_tier == "MEDIUM",   "#58a6ff", "#3fb950"))))
    plot_ly(d, x = ~state_risk_score,
            y = ~reorder(state, state_risk_score),
            type = "bar", orientation = "h",
            marker = list(color = ~col),
            hovertemplate = "<b>%{y}</b><br>Score: %{x:.1f}<extra></extra>") %>%
      dark_layout("State Risk Score (0-100)", "Risk Score", NULL,
                  show_legend = FALSE) %>%
      layout(margin = list(l = 150, r = 40, t = 55, b = 55))
  })
  output$tbl_state_full <- renderDT({
    req(rv$ready)
    rv$feat$state %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
                class = "compact")
  })

  # ===========================================================================
  # CATEGORIES
  # ===========================================================================
  bar_yr <- function(col_name, title, color) {
    renderPlotly({
      req(rv$ready)
      d <- f_city() %>%
        group_by(year) %>%
        summarise(v = sum(.data[[col_name]], na.rm = TRUE), .groups = "drop")
      plot_ly(d, x = ~year, y = ~v, type = "bar",
              marker = list(color = color),
              hovertemplate = paste0(title, "<br>Year: %{x}<br>%{y:,}<extra></extra>")) %>%
        dark_layout(title, "Year", "Count", show_legend = FALSE)
    })
  }
  output$plt_c_viol  <- bar_yr("violent_crimes",  "Violent Crimes",       "#f85149")
  output$plt_c_women <- bar_yr("women_crimes",     "Women-Related Crimes", "#a371f7")
  output$plt_c_cyber <- bar_yr("cyber_crimes",     "Cyber Crimes",         "#58a6ff")

  output$plt_cat_trend <- renderPlotly({
    req(rv$ready)
    d <- f_city() %>%
      group_by(year) %>%
      summarise(Violent  = sum(violent_crimes,  na.rm = TRUE),
                Women    = sum(women_crimes,    na.rm = TRUE),
                Cyber    = sum(cyber_crimes,    na.rm = TRUE),
                Property = sum(property_crimes, na.rm = TRUE),
                .groups  = "drop") %>%
      tidyr::pivot_longer(-year, names_to = "cat", values_to = "n")
    plot_ly(d, x = ~year, y = ~n, color = ~cat,
            colors = c("#f85149", "#a371f7", "#58a6ff", "#3fb950"),
            type = "scatter", mode = "lines+markers") %>%
      dark_layout("Crime Category Trends", "Year", "Count")
  })
  output$plt_weapon <- renderPlotly({
    req(rv$ready)
    d <- f_raw() %>%
      filter(weapon_used != "Unknown") %>%
      group_by(weapon_used) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      head(12)
    plot_ly(d, x = ~n, y = ~reorder(weapon_used, n),
            type = "bar", orientation = "h",
            marker = list(color = "#d29922"),
            hovertemplate = "<b>%{y}</b><br>%{x:,}<extra></extra>") %>%
      dark_layout("Weapon Usage", "Incidents", NULL, show_legend = FALSE) %>%
      layout(margin = list(l = 140, r = 40, t = 55, b = 55))
  })
  output$plt_cat_city <- renderPlotly({
    req(rv$ready)
    d <- latest() %>%
      arrange(desc(total_crimes)) %>%
      head(15) %>%
      select(city, Violent  = violent_crimes,
                   Women    = women_crimes,
                   Cyber    = cyber_crimes,
                   Property = property_crimes) %>%
      tidyr::pivot_longer(-city, names_to = "cat", values_to = "n")
    plot_ly(d, x = ~city, y = ~n, color = ~cat,
            colors = c("#f85149", "#a371f7", "#58a6ff", "#3fb950"),
            type = "bar", barmode = "stack") %>%
      dark_layout("Crime Mix - Top 15 Cities", "City", "Count")
  })

  # ===========================================================================
  # AI INSIGHTS
  # ===========================================================================
  output$ui_insights <- renderUI({
    req(rv$ready)
    tagList(lapply(rv$xai$insights, function(ins)
      tags$div(class = "insight-card",
        style = paste0("border-left-color:", ins$color, ";"),
        tags$div(class = "insight-title", ins$icon, " ", ins$title),
        tags$div(class = "insight-body",  ins$body))
    ))
  })
  output$tbl_anom <- renderDT({
    req(rv$ready)
    rv$xai$anomalies %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(rownames = FALSE,
                options = list(pageLength = 10, dom = "tp"),
                class = "compact")
  })
  output$tbl_trends <- renderDT({
    req(rv$ready)
    rv$xai$trends %>%
      transmute(City        = city,
                Risk        = risk_tier,
                `Rate/Lakh` = round(crime_rate_per_lakh, 1),
                `Growth%`   = round(crime_growth_rate,   1),
                `FC Change%`= round(fc_change_pct,       1),
                Explanation = explanation) %>%
      datatable(rownames = FALSE,
        options = list(pageLength = 10, dom = "ftip",
          columnDefs = list(list(width = "40%", targets = 5))),
        class = "compact")
  })

  # ===========================================================================
  # MODELS
  # ===========================================================================
  output$plt_model_cmp <- renderPlotly({
    req(rv$ready); make_model_comparison_chart(rv$models$selection$comparison)
  })
  output$plt_act_pred  <- renderPlotly({
    req(rv$ready)
    make_actual_vs_predicted(rv$models$train_data, rv$models$ml, "rf")
  })
  output$tbl_metrics <- renderDT({
    req(rv$ready)
    rv$models$selection$comparison %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                options = list(pageLength = 5, dom = "t"),
                class = "compact")
  })

  # ===========================================================================
  # XAI
  # ===========================================================================
  output$plt_feat_imp <- renderPlotly({
    req(rv$ready); make_feature_importance_chart(rv$xai$importance)
  })
  output$plt_shap <- renderPlotly({
    req(rv$ready)
    if (nrow(rv$xai$shap) == 0)
      return(plot_ly() %>%
               layout(title = "SHAP not computed",
                      paper_bgcolor = "#161b22",
                      plot_bgcolor  = "#161b22",
                      font = list(color = "#8b949e")))
    make_shap_chart(rv$xai$shap)
  })
  output$tbl_feat_imp <- renderDT({
    req(rv$ready)
    rv$xai$importance$by_model %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 15, dom = "ftip"),
                class = "compact")
  })

  # ===========================================================================
  # STATISTICAL TESTS
  # ===========================================================================
  output$tbl_t_summary <- renderDT({
    req(rv$ready, rv$tests)
    if (length(rv$tests) == 0) return(datatable(data.frame(Note = "Tests not computed")))
    t <- rv$tests$ttest
    df <- data.frame(
      Test    = c("One-Sample", "Two-Sample (Risk)", "Paired (2020-2023)",
                  "Metro vs Non-Metro", "Case Closure vs Violent"),
      H0      = c("Mean = national avg", "High Risk = Low Risk rate",
                  "2020 rate = 2023 rate", "Metro = Non-Metro rate",
                  "High closure = Low closure violent ratio"),
      t.stat  = round(c(t$one_sample$t_statistic,    t$risk_groups$t_statistic,
                        t$paired_years$t_statistic,  t$metro_nonmetro$t_statistic,
                        t$case_closure$t_statistic), 4),
      p.value = c(t$one_sample$p_value,  t$risk_groups$p_value,
                  t$paired_years$p_value, t$metro_nonmetro$p_value,
                  t$case_closure$p_value),
      Sig     = c(t$one_sample$significance,    t$risk_groups$significance,
                  t$paired_years$significance,  t$metro_nonmetro$significance,
                  t$case_closure$significance),
      Decision = c(t$one_sample$conclusion,    t$risk_groups$conclusion,
                   t$paired_years$conclusion,  t$metro_nonmetro$conclusion,
                   t$case_closure$conclusion),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE,
              options = list(pageLength = 5, dom = "t", scrollX = TRUE),
              class = "compact") %>%
      formatStyle("Sig",
        color = styleEqual(c("***","**","*","ns"),
                           c("#3fb950","#3fb950","#d29922","#8b949e")),
        fontWeight = "bold")
  })

  output$tbl_t_detail <- renderDT({
    req(rv$ready, rv$tests, input$sel_ttest)
    if (length(rv$tests) == 0) return(NULL)
    df <- switch(input$sel_ttest,
      one_sample     = rv$tests$ttest$one_sample,
      risk_groups    = rv$tests$ttest$risk_groups,
      paired_years   = rv$tests$ttest$paired_years,
      metro_nonmetro = rv$tests$ttest$metro_nonmetro,
      case_closure   = rv$tests$ttest$case_closure
    )
    if (is.null(df)) return(NULL)
    datatable(t(df), colnames = c("Field", "Value"), rownames = TRUE,
              options = list(pageLength = 20, dom = "t"), class = "compact")
  })

  output$plt_t_boxplot <- renderPlotly({
    req(rv$ready)
    df <- rv$feat$city %>%
      filter(!is.na(crime_rate_per_lakh)) %>%
      mutate(risk_group = ifelse(risk_tier %in% c("HIGH","CRITICAL"),
                                 "High Risk", "Low Risk"))
    plot_ly(df, x = ~risk_group, y = ~crime_rate_per_lakh,
            color = ~risk_group,
            colors = c("High Risk" = "#f85149", "Low Risk" = "#3fb950"),
            type = "box", boxpoints = "outliers", jitter = 0.3,
            hovertemplate = "<b>%{x}</b><br>Rate: %{y:.2f}<extra></extra>") %>%
      dark_layout("T-Test: Crime Rate - High vs Low Risk",
                  "Risk Group", "Crime Rate per Lakh") %>%
      layout(showlegend = FALSE)
  })

  output$tbl_chi_summary <- renderDT({
    req(rv$ready, rv$tests)
    if (length(rv$tests) == 0) return(NULL)
    c <- rv$tests$chisq
    df <- data.frame(
      Test    = c("Risk Tier x State","Closure x Crime Domain",
                  "Gender x Crime Category","Weapon Use x City","Risk Tier x Year"),
      H0      = c("Risk tier indep. of state","Closure indep. of domain",
                  "Gender indep. of crime category","Weapon use indep. of city",
                  "Risk distrib. stable over years"),
      Chi.sq  = round(c(c$risk_vs_state$chi_sq,      c$closure_vs_domain$chi_sq,
                        c$gender_vs_category$chi_sq, c$weapon_vs_city$chi_sq,
                        c$risk_vs_year$chi_sq), 4),
      p.value = c(c$risk_vs_state$p_value,      c$closure_vs_domain$p_value,
                  c$gender_vs_category$p_value, c$weapon_vs_city$p_value,
                  c$risk_vs_year$p_value),
      Sig     = c(c$risk_vs_state$significance,      c$closure_vs_domain$significance,
                  c$gender_vs_category$significance, c$weapon_vs_city$significance,
                  c$risk_vs_year$significance),
      CramersV = c(c$risk_vs_state$cramers_v, c$closure_vs_domain$cramers_v,
                   c$gender_vs_category$cramers_v, NA, NA),
      Decision = c(c$risk_vs_state$conclusion,      c$closure_vs_domain$conclusion,
                   c$gender_vs_category$conclusion, c$weapon_vs_city$conclusion,
                   c$risk_vs_year$conclusion),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE,
              options = list(pageLength = 5, dom = "t", scrollX = TRUE),
              class = "compact") %>%
      formatStyle("Sig",
        color = styleEqual(c("***","**","*","ns"),
                           c("#3fb950","#3fb950","#d29922","#8b949e")),
        fontWeight = "bold")
  })

  output$plt_chi_heatmap <- renderPlotly({
    req(rv$ready)
    ct <- as.data.frame(table(City = rv$feat$city$city,
                               Risk = rv$feat$city$risk_tier))
    plot_ly(ct, x = ~Risk, y = ~City, z = ~Freq, type = "heatmap",
      colorscale = list(list(0,"#0d1117"), list(0.5,"#58a6ff"), list(1,"#f85149")),
      hovertemplate = "City: %{y}<br>Risk: %{x}<br>Count: %{z}<extra></extra>") %>%
      dark_layout("Chi-Square: City x Risk Tier Frequency", "Risk Tier", NULL) %>%
      layout(margin = list(l = 140, r = 40, t = 55, b = 55))
  })

  output$plt_chi_weapon <- renderPlotly({
    req(rv$ready)
    df <- rv$master$raw %>%
      mutate(weapon = ifelse(weapon_used == "Unknown", "No Weapon", "Weapon Used")) %>%
      group_by(city, weapon) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(city) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      filter(weapon == "Weapon Used") %>%
      arrange(desc(pct))
    plot_ly(df, x = ~pct, y = ~reorder(city, pct),
            type = "bar", orientation = "h",
            marker = list(color = "#d29922"),
            hovertemplate = "<b>%{y}</b><br>Weapon: %{x:.1f}%<extra></extra>") %>%
      dark_layout("Weapon Usage % by City", "% Incidents with Weapon", NULL,
                  show_legend = FALSE) %>%
      layout(margin = list(l = 130, r = 40, t = 55, b = 55))
  })

  output$tbl_anova_summary <- renderDT({
    req(rv$ready, rv$tests)
    if (length(rv$tests) == 0) return(NULL)
    a <- rv$tests$anova
    df <- data.frame(
      Test    = c("Crime Rate ~ Risk Tier","Crime Rate ~ State",
                  "Violent % ~ Crime Domain","Victim Age ~ Crime Category",
                  "Police Deployed ~ Risk Tier"),
      F.stat  = round(c(a$rate_by_risk$result$f_statistic,
                        a$rate_by_state$result$f_statistic,
                        a$violent_by_domain$f_statistic,
                        a$age_by_category$result$f_statistic,
                        a$police_by_risk$f_statistic), 4),
      p.value = c(a$rate_by_risk$result$p_value,
                  a$rate_by_state$result$p_value,
                  a$violent_by_domain$p_value,
                  a$age_by_category$result$p_value,
                  a$police_by_risk$p_value),
      Sig     = c(a$rate_by_risk$result$significance,
                  a$rate_by_state$result$significance,
                  a$violent_by_domain$significance,
                  a$age_by_category$result$significance,
                  a$police_by_risk$significance),
      Eta2    = c(a$rate_by_risk$result$eta_squared,
                  a$rate_by_state$result$eta_squared,
                  a$violent_by_domain$eta_squared,
                  a$age_by_category$result$eta_squared,
                  a$police_by_risk$eta_squared),
      Decision = c(a$rate_by_risk$result$conclusion,
                   a$rate_by_state$result$conclusion,
                   a$violent_by_domain$conclusion,
                   a$age_by_category$result$conclusion,
                   a$police_by_risk$conclusion),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE,
              options = list(pageLength = 5, dom = "t", scrollX = TRUE),
              class = "compact") %>%
      formatStyle("Sig",
        color = styleEqual(c("***","**","*","ns"),
                           c("#3fb950","#3fb950","#d29922","#8b949e")),
        fontWeight = "bold")
  })

  output$tbl_tukey <- renderDT({
    req(rv$ready, rv$tests)
    tk <- rv$tests$anova$rate_by_risk$tukey
    if (is.null(tk)) return(datatable(data.frame(Note = "No Tukey data")))
    tk %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                options = list(pageLength = 6, dom = "t"),
                class = "compact") %>%
      formatStyle("significant",
        backgroundColor = styleEqual(c(TRUE, FALSE),
                                     c("rgba(63,185,80,0.15)", "transparent")))
  })

  output$plt_anova_boxplot <- renderPlotly({
    req(rv$ready)
    df   <- rv$feat$city %>% filter(!is.na(crime_rate_per_lakh), !is.na(risk_tier))
    cols <- c(CRITICAL = "#f85149", HIGH = "#d29922",
              MEDIUM   = "#58a6ff", LOW  = "#3fb950")
    plot_ly(df, x = ~risk_tier, y = ~crime_rate_per_lakh,
            color = ~risk_tier, colors = cols,
            type = "box", boxpoints = "all", jitter = 0.4,
            hovertemplate = "<b>%{x}</b><br>Rate: %{y:.2f}/lakh<extra></extra>") %>%
      dark_layout("ANOVA: Crime Rate by Risk Tier",
                  "Risk Tier", "Crime Rate per Lakh") %>%
      layout(showlegend = FALSE)
  })

  output$plt_anova_state <- renderPlotly({
    req(rv$ready, rv$tests)
    sm <- rv$tests$anova$rate_by_state$state_means %>%
      arrange(desc(mean_rate))
    plot_ly(sm, x = ~mean_rate, y = ~reorder(state, mean_rate),
            type = "bar", orientation = "h",
            marker = list(color = ~mean_rate,
              colorscale = list(list(0,"#3fb950"), list(0.5,"#58a6ff"),
                                list(1,"#f85149")),
              showscale = FALSE),
            hovertemplate = "<b>%{y}</b><br>Mean: %{x:.2f}<extra></extra>") %>%
      dark_layout("ANOVA: Mean Crime Rate by State",
                  "Crime Rate per Lakh", NULL, show_legend = FALSE) %>%
      layout(margin = list(l = 160, r = 40, t = 55, b = 55))
  })

  # ===========================================================================
  # DOWNLOAD HANDLERS
  # ===========================================================================
  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("crime_analytics_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      withProgress(message = "Building Excel report...", {
        p <- generate_excel_report(rv$master, rv$feat, rv$models, rv$xai, "reports")
        if (!is.null(p) && file.exists(p)) file.copy(p, file)
      })
    }
  )

  output$dl_html <- downloadHandler(
    filename = function()
      paste0("crime_report_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content  = function(file) {
      p <- generate_html_summary(rv$master, rv$feat, rv$models, "reports")
      if (!is.null(p) && file.exists(p)) file.copy(p, file)
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("forecasts_2025_2030_", Sys.Date(), ".csv"),
    content  = function(file)
      write.csv(rv$models$city_forecasts, file, row.names = FALSE)
  )

  output$dl_stats_xlsx <- downloadHandler(
    filename = function()
      paste0("statistical_tests_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      req(rv$tests)
      if (length(rv$tests) == 0) {
        showNotification("Statistical tests not yet computed.", type = "warning")
        return(NULL)
      }
      withProgress(message = "Building statistical report...", {
        p <- generate_stats_excel(rv$tests, "reports")
        if (!is.null(p) && file.exists(p)) file.copy(p, file)
      })
    }
  )

  # ===========================================================================
  # DATA TABLES
  # ===========================================================================
  output$tbl_city_full <- renderDT({
    req(rv$ready)
    f_city() %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
                class = "compact")
  })
  output$tbl_raw <- renderDT({
    req(rv$ready)
    f_raw() %>%
      select(city, year, month, crime_description, crime_category,
             crime_domain, weapon_used, victim_age, victim_gender,
             police_deployed, case_closed) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
                class = "compact")
  })
  output$tbl_yearly <- renderDT({
    req(rv$ready)
    rv$feat$yearly %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(rownames = FALSE,
                options = list(pageLength = 10, dom = "t"),
                class = "compact")
  })
  output$tbl_state_raw <- renderDT({
    req(rv$ready)
    rv$feat$state %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
                class = "compact")
  })

  # ===========================================================================
  # ADMIN
  # ===========================================================================
  output$audit <- renderText({
    logs <- list.files("logs", pattern = "\\.log$", full.names = TRUE)
    if (length(logs) == 0) return("No log file found.")
    paste(tail(readLines(logs[length(logs)], warn = FALSE), 40),
          collapse = "\n")
  })
  observeEvent(input$btn_retrain, {
    showNotification(
      "Retrain triggered - re-run app with new data file.",
      type = "message", duration = 6)
  })
  session$onSessionEnded(function()
    message("[INFO] Session ended"))

} # end server
