# Installs everything AssuRance needs. Run once in a fresh R session:
#   source("setup.R")

cran_pkgs <- c("shiny", "bslib", "plotly", "ggplot2", "DT", "rsconnect")
missing <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

# bayesassurance: CRAN first, GitHub as a fallback (the code is identical,
# version 0.1.0).
if (!requireNamespace("bayesassurance", quietly = TRUE)) {
  ok <- tryCatch({
    install.packages("bayesassurance")
    requireNamespace("bayesassurance", quietly = TRUE)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github("jpan928/bayesassurance_rpackage")
  }
}

stopifnot(requireNamespace("bayesassurance", quietly = TRUE))
message("All packages installed. Open app.R and click 'Run App'.")
