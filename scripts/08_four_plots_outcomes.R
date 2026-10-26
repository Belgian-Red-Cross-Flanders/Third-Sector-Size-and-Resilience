# 08_hc3_regressions.R
# -----------------------------------------------------------
# Regressions 
# - Example: coping capacity ~ Third_sector_size
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R")) 

# Create output folders 
dir.create(here::here("outputs", "regressions", "V2"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures", "V2"),      recursive = TRUE, showWarnings = FALSE)


# Load master
master <- readRDS(here::here("data_clean", "v2_master_dataset.rds"))


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
    "EM-DAT Disaster-Affected Population (log1p, per 100k)"
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
    # "volunteer_sp547_eurobar"
  ),
  xlab  = c(
    "TSS (%)",
    "TSS-P (%)",
    "TSS-V (%)"
    # "EU-VOL (%)"

  ),
  short = c(
    "tss",
    "tss_p",
    "tss_v"
    # "eu_vol"
  )
)

# keep only the countries which have data across all outcomes and inputs above and across the controls which are:
# hdi_undp
# funding_usd_unocha
# natural_hazard_inform
#they should be 106.
# keep only countries with complete data for:
# - all outcomes
# - all inputs
# - all control variables

required_vars <- c(
  outcomes$yvar,
  inputs$xvar,
  "hdi_undp",
  "log1p_funding_per_100k_unocha",
  "natural_hazard_inform"
)

master_common <- master %>%
  dplyr::filter(
    complete.cases(
      dplyr::select(., dplyr::all_of(required_vars))
    )
  )

saveRDS(master_common, here::here("data_clean", "v2_master_dataset_common.rds"))



# -----------------------
# Run regressions for all predictors
# -----------------------

# Container to collect regression summaries for this variable
reg_rows <- list()


# 2. Loop over each input

for (i in seq_len(nrow(inputs))) {
  
  fig <- make_2x2_predictor_figure(
    master   = master_common,
    inputs_row = inputs[i, ],
    outcomes = outcomes
  )
  
  outfile <- here::here(
    "outputs", "figures", "V2",
    paste0("figure_", inputs$short[i], "_2x2_common.png")
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


