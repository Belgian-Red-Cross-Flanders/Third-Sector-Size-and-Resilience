# 08_hc3_regressions.R
# -----------------------------------------------------------
# Regressions 
# - Example: coping capacity ~ Third_sector_size
# -----------------------------------------------------------

# source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R")) 

# Create output folders 
dir.create(here::here("outputs", "regressions", "HC3 SE"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures", "HC3 SE"),      recursive = TRUE, showWarnings = FALSE)


# Load master
master <- readRDS(here::here("data_clean", "master_dataset.rds"))


# Outcomes: ALL the variables you want regressions for
outcomes <- tibble::tibble(
  yvar  = c(
    "readiness_ndgain",
    "adaptive_capacity_ndgain",
    "nonprofit_sp547_eurobar",
    "trust_es_authorities_sp547_eurobar"
  ),
  ylab  = c(
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)",
    "Reliance on non-profits in first days of crisis (%)",
    "Trust in emergency services and authorities (%)"
  ),
  short = c(
    "ndgain_readiness",
    "ndgain_adaptive",
    "rel_nonprofit",
    "trust_es_auth"
  )
)

inputs <- tibble::tibble(
  xvar  = c(
    "volunteer_sp547_eurobar"
  ),
  xlab  = c(
    "EU-VOL (%)"
  ),
  short = c(
    "rc_eurovol"
  )
)


# -----------------------
# Run regressions for all outcomes
# -----------------------

fig <- make_4y_sharedx_figure(
  master  = master,
  outcomes = outcomes,
  input   = inputs
)

outfile <- here::here(
  "outputs", "figures", "HC3 SE",
  "figure_EUVOL_4outcomes.png"
)

ggsave(
  outfile,
  fig,
  dpi = 300,
  width = 9,
  height = 7.2
)

message("EU‑VOL shared‑x figure saved.")
