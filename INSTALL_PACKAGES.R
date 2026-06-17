# =============================================================
# INSTALL_PACKAGES.R
# Run this ONCE before launching the app for the first time.
# Works on Windows, Linux, and Mac.
# =============================================================

required <- c(
  "shiny",
  "shinycssloaders",
  "plotly",
  "DT",
  "dplyr",
  "tidyr",
  "zoo",
  "randomForest",
  "forecast",
  "openxlsx",
  "logger"
)

# Install any missing packages
missing <- required[!required %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  cat("Installing", length(missing), "missing packages:\n")
  cat(paste(" -", missing, collapse = "\n"), "\n\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages are already installed.\n")
}

# Verify all installed
ok  <- required[required %in% installed.packages()[, "Package"]]
bad <- setdiff(required, ok)

cat("\n=== Package Check ===\n")
for (p in required) {
  status <- if (p %in% ok) "OK" else "MISSING"
  cat(sprintf("  %-20s %s\n", p, status))
}

if (length(bad) > 0) {
  stop("Could not install: ", paste(bad, collapse = ", "),
       "\nTry installing manually: install.packages(c(",
       paste0('"', bad, '"', collapse = ", "), "))")
} else {
  cat("\nAll packages ready. Run the app with:\n")
  cat("  shiny::runApp('.')\n")
}
