# 01_load_packages.R

required_pkgs <- c(
  
  dplyr     = "1.2.0",
  tidyr     = "1.3.2",
  ggplot2   = "4.0.2",
  ggrepel   = "0.9.6",
  readxl    = "1.4.5",
  openxlsx  = "4.2.8.1",
  psych     = "2.6.1",
  mediation = "4.5.1" ,
  car       = "3.1-5",
  lmtest    = "0.9-40",
  here      = "1.0.2",
  haven     = "2.5.5",
  purrr     = "1.2.1",
  reshape2  = "1.4.5",
  broom     = "1.0.12",
  writexl   = "1.5.4",
  rmarkdown = "2.30"
  
)


ensure_version <- function(pkg, ver) {
  is_installed <- requireNamespace(pkg, quietly = TRUE)
  current_ver  <- if (is_installed) as.character(utils::packageVersion(pkg)) else NA_character_
  
  # Install exactly the requested version if not present
  if (!is_installed || current_ver != ver) {
    message(sprintf("Installing %s (%s) ...", pkg, ver))
    # remotes is great for installing specific versions
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_version(pkg, version = ver, upgrade = "never", repos = getOption("repos"))
  }
  
  # Load it (fail early if something is wrong)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Install/load all
invisible(mapply(ensure_version, names(required_pkgs), unname(required_pkgs)))

# Log the environment for traceability
if (!dir.exists(here::here("outputs"))) dir.create(here::here("outputs"))
sink(here::here("outputs", "SESSION_INFO.txt"))
cat("R version:", R.version.string, "\n")
print(sessionInfo())
sink()

