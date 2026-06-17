# =============================================================================
# app.R - Crime Analytics Platform v2.1
# Windows + Linux + Mac + Docker compatible
# Run: shiny::runApp(".")  OR  Rscript app.R
# =============================================================================

# -- Safe working directory (works on Windows, Linux, Mac, Docker, RStudio) ---
.set_wd <- function() {
  # Priority 1: explicit env variable set by deployment
  env_dir <- Sys.getenv("CRIME_APP_DIR", unset = "")
  if (nchar(env_dir) > 0 && dir.exists(env_dir)) {
    setwd(env_dir); return(invisible(NULL))
  }
  # Priority 2: Rscript app.R (Windows and Linux)
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("--file=", "", file_arg[1]),
                                  winslash = "/", mustWork = FALSE)
    if (file.exists(script_path)) {
      setwd(dirname(script_path)); return(invisible(NULL))
    }
  }
  # Priority 3: RStudio source() sets wd to file location automatically
  # Priority 4: shiny::runApp() sets wd automatically - no action needed
  invisible(NULL)
}

tryCatch(.set_wd(), error = function(e)
  message("Note: Could not set working directory automatically. ",
          "Run shiny::runApp('path/to/crime_analytics') explicitly."))

# -- Load application ----------------------------------------------------------
source("global.R", encoding = "UTF-8")
source("ui.R",     encoding = "UTF-8")
source("server.R", encoding = "UTF-8")

# -- Launch --------------------------------------------------------------------
shiny::shinyApp(
  ui      = ui,
  server  = server,
  options = list(
    host           = "0.0.0.0",
    port           = 3838,
    launch.browser = interactive()
  )
)
