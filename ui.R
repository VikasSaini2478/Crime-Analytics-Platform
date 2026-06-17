# =============================================================================
# ui.R - Crime Analytics Platform v2.1
# Dark theme enterprise dashboard - Windows/Linux/Mac compatible
# All ASCII source code, UTF-8 safe
# =============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(DT)
  library(shinycssloaders)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# =============================================================================
# INLINE CSS - all ASCII safe
# =============================================================================
css <- tags$style(HTML('
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap");
*{box-sizing:border-box;margin:0;padding:0}
body,html{background:#0d1117;color:#e6edf3;font-family:Inter,system-ui,sans-serif;font-size:14px;margin:0;padding:0}
.wrap{display:flex;min-height:100vh}
.sidebar{width:228px;flex-shrink:0;background:#161b22;border-right:1px solid #30363d;
         padding:20px 14px;height:100vh;overflow-y:auto;position:sticky;top:0;align-self:flex-start}
.main{flex:1;padding:24px;background:#0d1117;overflow-x:hidden}
.logo{display:flex;align-items:center;gap:10px;padding-bottom:20px;
      border-bottom:1px solid #30363d;margin-bottom:20px}
.logo-icon{font-size:26px}.logo-text{font-size:14px;font-weight:700;line-height:1.2}
.logo-sub{font-size:10px;color:#8b949e}
.sec-label{font-size:10px;text-transform:uppercase;letter-spacing:.08em;
           color:#8b949e;padding:14px 0 6px;font-weight:600}
.nav-btn{display:flex;align-items:center;gap:10px;width:100%;text-align:left;
         padding:9px 10px;background:transparent;border:none;border-radius:7px;
         color:#8b949e;cursor:pointer;font-size:13px;margin-bottom:2px;
         transition:all .15s ease;font-family:inherit}
.nav-btn:hover{background:#1c2128;color:#e6edf3}
.nav-btn.active{background:rgba(88,166,255,.13);color:#58a6ff;font-weight:600}
.dot{display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:6px}
.dot-green{background:#3fb950;box-shadow:0 0 6px #3fb950;animation:pulse 2s infinite}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.4}}
.status-bar{font-size:11px;color:#8b949e;margin-bottom:18px;display:flex;align-items:center}
.ph{margin-bottom:22px;padding-bottom:14px;border-bottom:1px solid #30363d}
.ph h2{font-size:1.4em;font-weight:700;margin-bottom:4px}
.ph p{color:#8b949e;font-size:12px}
.kpi-row{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:20px}
.kpi-card{background:#161b22;border:1px solid #30363d;border-radius:10px;
          padding:18px;position:relative;overflow:hidden;
          transition:transform .2s,box-shadow .2s}
.kpi-card:hover{transform:translateY(-2px);box-shadow:0 8px 24px rgba(0,0,0,.4)}
.kpi-card::before{content:"";position:absolute;top:0;left:0;right:0;height:3px;
                  background:var(--ac,#58a6ff)}
.kpi-lbl{font-size:10px;text-transform:uppercase;letter-spacing:.06em;color:#8b949e;margin-bottom:6px}
.kpi-val{font-size:1.9em;font-weight:800;color:var(--ac,#58a6ff);line-height:1}
.kpi-icon{position:absolute;right:14px;top:50%;transform:translateY(-50%);
          font-size:32px;opacity:.1}
.chart-grid{display:grid;gap:16px;margin-bottom:16px}
.g2{grid-template-columns:1fr 1fr}
.g3{grid-template-columns:1fr 1fr 1fr}
.gw{grid-template-columns:2fr 1fr}
.cc{background:#161b22;border:1px solid #30363d;border-radius:10px;padding:18px;margin-bottom:0}
.cc-hdr{margin-bottom:14px}
.cc-title{font-size:13px;font-weight:600;color:#e6edf3}
.cc-sub{font-size:11px;color:#8b949e;margin-top:3px}
.report-card{text-align:center;padding:32px 20px}
.btn-dl{display:inline-block;padding:9px 18px;border-radius:8px;
        font-size:13px;font-weight:600;cursor:pointer;border:none;
        font-family:inherit;transition:all .2s}
.btn-dl-green{background:rgba(63,185,80,.15);border:1px solid #3fb950;color:#3fb950}
.btn-dl-blue{background:rgba(88,166,255,.15);border:1px solid #58a6ff;color:#58a6ff}
.btn-dl-purple{background:rgba(163,113,247,.15);border:1px solid #a371f7;color:#a371f7}
.btn-dl:hover{transform:translateY(-1px);box-shadow:0 4px 12px rgba(0,0,0,.3)}
.insight-card{background:#1c2128;border:1px solid #30363d;border-left:3px solid #58a6ff;
              border-radius:7px;padding:14px;margin-bottom:10px}
.insight-title{font-size:12px;font-weight:600;color:#e6edf3;margin-bottom:5px}
.insight-body{font-size:11px;color:#8b949e;line-height:1.6}
.selectize-control .selectize-input,.selectize-dropdown{
  background:#1c2128!important;color:#e6edf3!important;border-color:#30363d!important}
.irs--shiny .irs-bar{background:#58a6ff;border-top:1px solid #58a6ff;border-bottom:1px solid #58a6ff}
.irs--shiny .irs-handle{background:#58a6ff;border:2px solid #161b22}
.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{background:#58a6ff}
.irs-line{background:#30363d}.irs-grid-text,.irs-min,.irs-max{color:#8b949e!important}
.form-control[type=text]{background:#1c2128!important;border:1px solid #30363d!important;
                          color:#e6edf3!important;border-radius:6px!important}
input[type=file]{color:#8b949e}
.btn-primary{background:rgba(88,166,255,.15)!important;border:1px solid #58a6ff!important;
             color:#58a6ff!important;border-radius:7px!important;
             font-size:13px!important;font-family:inherit!important}
.dataTables_wrapper{color:#e6edf3!important}
table.dataTable thead th{background:#1c2128!important;color:#58a6ff!important;
  border-bottom:1px solid #30363d!important;font-size:11px;text-transform:uppercase}
table.dataTable tbody tr{background:#161b22!important}
table.dataTable tbody tr:hover{background:#1c2128!important}
table.dataTable tbody td{color:#e6edf3!important;border-top:1px solid #21262d!important;font-size:12px}
.dataTables_paginate .paginate_button{color:#8b949e!important}
.dataTables_paginate .paginate_button.current{background:rgba(88,166,255,.2)!important;
  color:#58a6ff!important;border-radius:4px}
.dataTables_info,.dataTables_length label,.dataTables_filter label{color:#8b949e!important;font-size:11px}
.dataTables_filter input{background:#1c2128!important;color:#e6edf3!important;
  border:1px solid #30363d!important;border-radius:5px;padding:3px 8px}
select{background:#1c2128!important;color:#e6edf3!important;
       border:1px solid #30363d!important;border-radius:5px}
.shiny-progress-container{background:#161b22!important;border:1px solid #30363d;border-radius:8px}
.shiny-progress .bar{background:linear-gradient(90deg,#58a6ff,#a371f7)!important}
.shiny-progress .shiny-progress-message{color:#e6edf3!important;font-family:Inter,sans-serif;font-size:13px}
.shiny-notification{background:#161b22!important;border:1px solid #30363d!important;
  color:#e6edf3!important;border-radius:8px!important;font-size:13px!important}
.shiny-notification-error{border-left:4px solid #f85149!important}
.shiny-notification-message{border-left:4px solid #58a6ff!important}
.modebar{background:rgba(22,27,34,.9)!important;border-radius:5px!important}
.modebar-btn path{fill:#8b949e!important}
.modebar-btn:hover path{fill:#58a6ff!important}
pre,.shiny-text-output{background:#0d1117!important;color:#3fb950!important;
  border:1px solid #30363d!important;border-radius:7px!important;
  font-family:"Courier New",monospace!important;font-size:11px!important;padding:14px!important}
.nav-tabs{border-bottom:1px solid #30363d!important}
.nav-tabs .nav-link{color:#8b949e!important;background:transparent!important;
  border:none!important;padding:8px 16px!important;font-size:13px}
.nav-tabs .nav-link.active{color:#58a6ff!important;background:rgba(88,166,255,.1)!important;
  border-bottom:2px solid #58a6ff!important}
.tab-content>.tab-pane{padding-top:14px}
::-webkit-scrollbar{width:5px;height:5px}
::-webkit-scrollbar-track{background:#161b22}
::-webkit-scrollbar-thumb{background:#30363d;border-radius:3px}
@media(max-width:1100px){.kpi-row{grid-template-columns:1fr 1fr}.g3,.g2,.gw{grid-template-columns:1fr}}
@media(max-width:700px){.kpi-row{grid-template-columns:1fr}.sidebar{display:none}.main{padding:10px}}
'))

# =============================================================================
# NAV BUTTON HELPER
# =============================================================================
nb <- function(id, icon_char, label)
  tags$button(
    # data-nav-id used by JS setActiveNav() to avoid conflicting
    # with Shiny input IDs. The button itself has NO id= attribute.
    "data-nav-id" = id,
    class         = "nav-btn",
    icon_char, " ", label,
    onclick = sprintf(
      "Shiny.setInputValue('active_tab','%s',{priority:'event'})", id)
  )

# =============================================================================
# UI DEFINITION
# =============================================================================
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width,initial-scale=1"),
    tags$link(rel  = "stylesheet", href = "css/theme.css"),
    css,
    tags$script(src = "js/dashboard.js")
  ),

  tags$div(class = "wrap",

    # ---- SIDEBAR ------------------------------------------------------------
    tags$div(class = "sidebar",

      # Logo
      tags$div(class = "logo",
        tags$div(class = "logo-icon", "[S]"),
        tags$div(
          tags$div(class = "logo-text", "CrimeAnalytics"),
          tags$div(class = "logo-sub",  "India Intelligence Platform")
        )
      ),

      # Status indicator
      tags$div(class = "status-bar",
        tags$span(class = "dot dot-green"), "Live Dashboard"
      ),

      # Navigation
      tags$div(class = "sec-label", "Analytics"),
      nb("nav_overview",   "[O]", "Executive Overview"),
      nb("nav_map",        "[M]", "India Crime Map"),
      nb("nav_forecast",   "[F]", "Forecast Center"),

      tags$div(class = "sec-label", "Intelligence"),
      nb("nav_states",     "[S]", "State Intelligence"),
      nb("nav_categories", "[C]", "Crime Categories"),
      nb("nav_ai",         "[AI]","AI Insights"),

      tags$div(class = "sec-label", "Models"),
      nb("nav_models",     "[ML]","Model Evaluation"),
      nb("nav_xai",        "[X]", "Explainable AI"),
      nb("nav_stats",      "[T]", "Statistical Tests"),

      tags$div(class = "sec-label", "Reports & Data"),
      nb("nav_reports",    "[R]", "Report Center"),
      nb("nav_data",       "[D]", "Raw Data"),
      nb("nav_admin",      "[A]", "Administration"),

      tags$hr(style = "border-color:#30363d;margin:16px 0"),

      # Global filters
      tags$div(class = "sec-label", "Global Filters"),
      sliderInput("yr", "Year Range",
                  min = 2020, max = 2024,
                  value = c(2020, 2024),
                  sep = "", step = 1),

      tags$div(
        style = paste0("margin-top:16px;padding:10px;",
                       "background:rgba(88,166,255,.06);",
                       "border:1px solid rgba(88,166,255,.15);",
                       "border-radius:7px;font-size:10px;color:#8b949e;line-height:1.7"),
        "v2.1 Production  |  2020-2024", tags$br(),
        "LR / RF / XGB-proxy / ETS / ARIMA"
      )
    ),

    # ---- MAIN CONTENT -------------------------------------------------------
    tags$div(class = "main", uiOutput("page_body"))
  )
)
