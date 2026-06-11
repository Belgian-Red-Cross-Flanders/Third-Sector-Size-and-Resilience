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
    "coping_capacity_inform",
    "readiness_ndgain",
    "adaptive_capacity_ndgain",
    "log1p_affected_per_100k_15_24_mean_owid"
  ),
  ylab  = c(
    "INFORM Coping Capacity (%)",
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)",
    "Realized Disaster Impacts (log1p, per 100k)"
  ),
  short = c(
    "inf_coping",
    "ndgain_readiness",
    "ndgain_adaptive",
    "owid_affected_mean"
  )
)

inputs <- tibble::tibble(
  xvar  = c(
    "Total_tpt",
    "log1p_volunteers_staff_per_100k_ifrc_2024",
    "log1p_income_per_100k_ifrc_2024",
    "log1p_people_first_aid_per_100k_ifrc_2024"
    
  ),
  xlab  = c(
    "TSS (%)",
    "RCRC-VS (log1p, per 100k)",
    "RCRC-E (log1p, per 100k, CHF)",
    "RCRC-FA (log1p, per 100k)"
  ),
  short = c(
    "tss",
    "rcrc_vs",
    "rcrc_e",
    "rcrc_fa"
  )
)


# -----------------------
# Run regressions for all outcomes
# -----------------------

# Container to collect regression summaries for this x variable
reg_rows <- list()


# 2. Loop over each outcome
for (i in seq_len(nrow(outcomes))) {
  
  fig <- make_2x2_outcome_figure(
    master = master,
    outcome_row = outcomes[i, ],
    inputs = inputs
  )
  
  outfile <- here::here(
    "outputs", "figures", "HC3 SE",
    paste0("figure_", outcomes$short[i], "_2x2_nocountries.png")
  )
  
  ggsave(
    outfile,
    fig,
    dpi = 300,
    width = 9,   # 7.2 is journal column friendly
    height = 6.8
  )
}


message(" regressions complete. See  outputs/figures.")
