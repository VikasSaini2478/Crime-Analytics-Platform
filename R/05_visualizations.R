# =============================================================================
# 05_visualizations.R - Plotly chart builders
# =============================================================================
suppressPackageStartupMessages({ library(plotly); library(dplyr) })
source("R/00_utils.R")

TH <- list(
  bg="#0d1117",card="#161b22",panel="#1c2128",border="#30363d",
  blue="#58a6ff",purple="#a371f7",green="#3fb950",red="#f85149",
  amber="#d29922",text="#e6edf3",sub="#8b949e"
)

dark_layout <- function(p, title=NULL, xlab=NULL, ylab=NULL,
                         show_legend=TRUE, margin=NULL) {
  m <- margin %||% list(l=60,r=40,t=55,b=55)
  p %>% layout(
    title=list(text=title, font=list(color=TH$text,size=14,
               family="Inter,sans-serif"), x=0.02),
    paper_bgcolor=TH$card, plot_bgcolor=TH$card,
    font=list(color=TH$sub, family="Inter,sans-serif", size=11),
    xaxis=list(title=xlab, gridcolor="#21262d", zerolinecolor="#30363d",
               color=TH$sub, tickfont=list(color=TH$sub)),
    yaxis=list(title=ylab, gridcolor="#21262d", zerolinecolor="#30363d",
               color=TH$sub, tickfont=list(color=TH$sub)),
    legend=list(bgcolor="rgba(0,0,0,0)",font=list(color=TH$text)),
    showlegend=show_legend, margin=m,
    hoverlabel=list(bgcolor=TH$panel,bordercolor=TH$border,
                    font=list(color=TH$text,family="Inter,sans-serif"))
  )
}

# -- National forecast chart ---------------------------------------------------
make_crime_trend_chart <- function(yearly, ts_results) {
  hist <- data.frame(year=yearly$year, value=yearly$avg_crime_rate)
  ets_fc   <- ts_results$ets$forecast_df
  arima_fc <- ts_results$arima$forecast_df
  plot_ly() %>%
    add_trace(data=hist, x=~year, y=~value, type="scatter", mode="lines+markers",
      name="Historical", line=list(color=TH$blue,width=3),
      marker=list(color=TH$blue,size=9)) %>%
    add_trace(data=ets_fc, x=~year, y=~forecast, type="scatter", mode="lines+markers",
      name="ETS Forecast", line=list(color=TH$purple,width=2,dash="dash"),
      marker=list(color=TH$purple,size=8,symbol="diamond")) %>%
    add_ribbons(data=ets_fc, x=~year, ymin=~lower_95, ymax=~upper_95,
      name="95% CI (ETS)", fillcolor="rgba(163,113,247,0.15)",
      line=list(color="transparent")) %>%
    add_trace(data=arima_fc, x=~year, y=~forecast, type="scatter", mode="lines+markers",
      name="ARIMA Forecast", line=list(color=TH$green,width=2,dash="dot"),
      marker=list(color=TH$green,size=8,symbol="square")) %>%
    dark_layout("National Crime Rate Forecast 2024-2030",
                "Year","Crime Rate per Lakh Population")
}

# -- City ranking bar chart ----------------------------------------------------
make_city_ranking_chart <- function(city_features, top_n=20) {
  latest <- city_features %>% filter(year==max(year,na.rm=TRUE)) %>%
    arrange(desc(crime_rate_per_lakh)) %>% head(top_n) %>%
    mutate(bar_color=ifelse(risk_tier=="CRITICAL",TH$red,
                     ifelse(risk_tier=="HIGH",TH$amber,
                     ifelse(risk_tier=="MEDIUM",TH$blue,TH$green))))
  plot_ly(latest, x=~crime_rate_per_lakh, y=~reorder(city,crime_rate_per_lakh),
          type="bar", orientation="h", marker=list(color=~bar_color),
          hovertemplate="<b>%{y}</b><br>Crime Rate: %{x:.1f}<extra></extra>") %>%
    dark_layout(paste0("Top ",top_n," Cities by Crime Rate"),
                "Crime Rate (per lakh)",NULL,show_legend=FALSE) %>%
    layout(margin=list(l=140,r=40,t=55,b=55))
}

# -- Donut chart ---------------------------------------------------------------
make_category_donut <- function(raw_df) {
  df <- raw_df %>% group_by(crime_category) %>% summarise(count=n(),.groups="drop") %>%
    arrange(desc(count))
  cols <- c(TH$red,TH$amber,TH$purple,TH$blue,TH$green,"#79c0ff","#f0883e")
  plot_ly(df, labels=~crime_category, values=~count, type="pie", hole=0.55,
    marker=list(colors=cols[seq_len(nrow(df))],line=list(color=TH$card,width=2)),
    textinfo="label+percent", textfont=list(color=TH$text,size=11),
    hovertemplate="<b>%{label}</b><br>Count: %{value:,}<br>%{percent}<extra></extra>") %>%
    dark_layout("Crime Category Distribution")
}

# -- Heatmap cityxyear ---------------------------------------------------------
make_crime_heatmap <- function(city_features) {
  wide <- city_features %>%
    select(city,year,crime_rate_per_lakh) %>%
    tidyr::pivot_wider(names_from=year, values_from=crime_rate_per_lakh)
  yrs  <- as.character(sort(unique(city_features$year)))
  z    <- as.matrix(wide[, yrs, drop=FALSE])
  plot_ly(x=yrs, y=wide$city, z=z, type="heatmap",
    colorscale=list(list(0,"#0d1117"),list(0.25,"#1c2d4a"),list(0.5,TH$blue),
                    list(0.75,TH$amber),list(1,TH$red)),
    hovertemplate="<b>%{y}</b><br>Year: %{x}<br>Rate: %{z:.1f}<extra></extra>",
    colorbar=list(title="Crime Rate",tickfont=list(color=TH$sub),
                  titlefont=list(color=TH$sub))) %>%
    dark_layout("City Crime Rate Heatmap","Year",NULL) %>%
    layout(margin=list(l=160,r=100,t=55,b=55))
}

# -- Feature importance chart --------------------------------------------------
make_feature_importance_chart <- function(imp_data, top_n=12) {
  df <- head(imp_data$consensus, top_n)
  plot_ly(df, x=~avg_importance, y=~reorder(feature_label,avg_importance),
    type="bar", orientation="h",
    marker=list(color=~avg_importance,
      colorscale=list(list(0,TH$blue),list(0.5,TH$purple),list(1,TH$red)),
      showscale=FALSE),
    hovertemplate="<b>%{y}</b><br>Importance: %{x:.1f}<extra></extra>") %>%
    dark_layout("Consensus Feature Importance","Importance Score",NULL,show_legend=FALSE) %>%
    layout(margin=list(l=200,r=40,t=55,b=55))
}

# -- Model comparison ----------------------------------------------------------
make_model_comparison_chart <- function(comp_df) {
  long <- comp_df %>%
    select(model,rmse,mae,r_squared) %>%
    tidyr::pivot_longer(-model,names_to="metric",values_to="value") %>%
    mutate(metric_label=ifelse(metric=="rmse","RMSE (v better)",
                        ifelse(metric=="mae","MAE (v better)","R^2 (^ better)")))
  plot_ly(long, x=~metric_label, y=~value, color=~model,
    colors=c(TH$blue,TH$purple,TH$green), type="bar",
    hovertemplate="<b>%{x}</b><br>%{data.name}: %{y:.4f}<extra></extra>") %>%
    dark_layout("Model Performance Comparison",NULL,"Metric Value")
}

# -- Actual vs Predicted -------------------------------------------------------
make_actual_vs_predicted <- function(train_data, model_results, key="rf") {
  p <- model_results[[key]]$predictions_train
  a <- train_data$crime_rate_per_lakh
  n <- min(length(a),length(p))
  df <- data.frame(actual=a[1:n], predicted=p[1:n])
  pfl <- data.frame(x=c(min(df$actual),max(df$actual)),
                    y=c(min(df$actual),max(df$actual)))
  plot_ly() %>%
    add_trace(data=df, x=~actual, y=~predicted, type="scatter", mode="markers",
      marker=list(color=TH$blue,opacity=0.55,size=6), name="Predictions",
      hovertemplate="Actual: %{x:.1f}<br>Pred: %{y:.1f}<extra></extra>") %>%
    add_trace(data=pfl, x=~x, y=~y, type="scatter", mode="lines",
      line=list(color=TH$red,width=2,dash="dash"), name="Perfect Fit") %>%
    dark_layout(paste("Actual vs Predicted -",toupper(key)),
                "Actual Crime Rate","Predicted Crime Rate")
}

# -- City forecast chart -------------------------------------------------------
make_city_forecast_chart <- function(city_features, city_forecasts, city_name) {
  hist <- city_features %>% filter(city==city_name) %>%
    select(year, value=crime_rate_per_lakh) %>% arrange(year)
  fc   <- city_forecasts %>% filter(city==city_name) %>%
    select(year, value=forecast_crime_rate, lower_95, upper_95)
  if(nrow(hist)==0||nrow(fc)==0)
    return(plot_ly() %>% layout(title=paste("No data for",city_name),
                                 paper_bgcolor=TH$card,plot_bgcolor=TH$card,
                                 font=list(color=TH$sub)))
  plot_ly() %>%
    add_trace(data=hist, x=~year, y=~value, type="scatter", mode="lines+markers",
      name="Historical", line=list(color=TH$blue,width=3),
      marker=list(color=TH$blue,size=9)) %>%
    add_trace(data=fc, x=~year, y=~value, type="scatter", mode="lines+markers",
      name="Forecast", line=list(color=TH$purple,width=2,dash="dash"),
      marker=list(color=TH$purple,size=8,symbol="diamond")) %>%
    add_ribbons(data=fc, x=~year, ymin=~lower_95, ymax=~upper_95,
      name="95% CI", fillcolor="rgba(163,113,247,0.2)",
      line=list(color="transparent")) %>%
    dark_layout(paste0(city_name," - Crime Rate Forecast (2025-2030)"),
                "Year","Crime Rate (per lakh)")
}

# -- SHAP chart ----------------------------------------------------------------
make_shap_chart <- function(shap_vals, top_n=10) {
  df <- head(shap_vals, top_n) %>% arrange(shap_importance) %>%
    mutate(bar_color=ifelse(shap_importance>0,TH$red,TH$green))
  plot_ly(df, x=~shap_importance, y=~reorder(feature_label,shap_importance),
    type="bar", orientation="h", marker=list(color=~bar_color),
    hovertemplate="<b>%{y}</b><br>SHAP: %{x:.4f}<extra></extra>") %>%
    dark_layout("SHAP Permutation Importance","RMSE Increase when Permuted",NULL,show_legend=FALSE) %>%
    layout(margin=list(l=200,r=40,t=55,b=55))
}

# -- Growth rate chart ---------------------------------------------------------
make_growth_chart <- function(city_features, selected_cities=NULL) {
  if(is.null(selected_cities)) {
    top8 <- city_features %>% group_by(city) %>%
      summarise(tot=sum(total_crimes,na.rm=TRUE),.groups="drop") %>%
      top_n(8,tot) %>% pull(city)
    selected_cities <- top8
  }
  df <- city_features %>% filter(city %in% selected_cities, !is.na(crime_growth_rate))
  plot_ly(df, x=~year, y=~crime_growth_rate, color=~city, type="scatter", mode="lines+markers",
    hovertemplate="<b>%{data.name}</b><br>Year: %{x}<br>Growth: %{y:.1f}%<extra></extra>") %>%
    dark_layout("YoY Crime Growth Rate (%)","Year","Growth Rate (%)")
}
