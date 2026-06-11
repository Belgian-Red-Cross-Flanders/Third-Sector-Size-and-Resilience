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
    "coping_capacity_inform",
    "log1p_affected_per_100k_15_24_mean_owid"
  ),
  ylab  = c(
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)",
    "INFORM Coping Capacity (%)",
    "Realized Disaster Impacts (log1p, per 100k)"
  ),
  short = c(
    "ndgain_readiness",
    "ndgain_adaptive",
    "inf_coping",
    "owid_affected_mean"
  )
)

inputs <- tibble::tibble(
  xvar  = c(
    "Total_tpt",
    "Paid.staff_tpt",
    "Volunteers_tpt"
    
  ),
  xlab  = c(
    "TSS (%)",
    "TSS-P (%)",
    "TSS-V (%)"
  ),
  short = c(
    "tss",
    "pstaff",
    "volunteers"
  )
)


# -----------------------
# Run regressions for all predictors
# -----------------------

# Container to collect regression summaries for this variable
reg_rows <- list()


# 2. Loop over each input

for (i in seq_len(nrow(inputs))) {
  
  fig <- make_2x2_predictor_figure(
    master   = master,
    inputs_row = inputs[i, ],
    outcomes = outcomes
  )
  
  outfile <- here::here(
    "outputs", "figures", "HC3 SE",
    paste0("figure_", inputs$short[i], "_2x2_nocountries.png")
  )
  
  ggsave(
    outfile,
    fig,
    dpi = 300,
    width = 9,
    height = 6.8
  )
}


message(" regressions complete. See  outputs/figures.")
