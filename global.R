# =============================================================================
# global.R - Loaded once per R worker process
# Windows + Linux + Mac compatible (all ASCII, UTF-8 safe)
# =============================================================================

# -- Options: improve Windows compatibility -----------------------------------
options(
  encoding         = "UTF-8",
  scipen           = 999,        # avoid scientific notation in outputs
  warn             = 0,          # suppress non-critical warnings
  stringsAsFactors = FALSE        # default for R < 4.0 compatibility
)

# -- Logger (optional, graceful fallback) -------------------------------------
tryCatch(
  suppressPackageStartupMessages(library(logger)),
  error = function(e) message("[INFO] logger not available - using message()")
)

# -- Required packages ---------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(DT)
  library(shinycssloaders)
  library(dplyr)
  library(tidyr)
  library(zoo)
  library(randomForest)
  library(forecast)
  library(openxlsx)
})

# -- Null-coalescing operator --------------------------------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

# -- Source all R modules (encoding specified for Windows) --------------------
module_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
for (f in module_files) {
  source(f, encoding = "UTF-8", local = FALSE)
}

# -- Ensure runtime directories exist -----------------------------------------
for (d in c("reports", "logs", "models")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

message("[INFO] global.R loaded - Crime Analytics Platform v2.1")
