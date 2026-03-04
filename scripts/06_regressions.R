# 06_regressions_inform.R
# -----------------------------------------------------------
# Regressions for variables vs Third sector size
# - Example: coping capacity ~ Third_sector_size
# - By income groups (all / upper+high / low+lower / individual groups)
# - Outputs: tidy tables, diagnostics, plots to outputs/
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R")) 

# Create output folders 
dir.create(here::here("outputs", "regressions"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures"),      recursive = TRUE, showWarnings = FALSE)


# Load master
master <- readRDS(here::here("data_clean", "master_dataset.rds"))

income_var <- "World.Bank.Income.Group_tpt"

# Outcomes: ALL the variables you want regressions for
outcomes <- tibble::tibble(
  yvar  = c(
    "coping_capacity_inform",
    "readiness_ndgain",
    "adaptive_capacity_ndgain"  
  ),
  ylab  = c(
    "INFORM Coping Capacity (%)",
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)"
  ),
  short = c(
    "coping",
    "readiness",
    "adaptive"
  )
)

income_sets <- load_income_list()


# -----------------------
# Run regressions for all outcomes × income sets
# -----------------------

for (set_name in names(income_sets)) {
  info <- income_sets[[set_name]]
  
  # 1. Subset master by income group(s)
  if (is.null(info$levels)) {
    df_subset <- master
  } else {
    df_subset <- master %>%
      dplyr::filter(.data[[income_var]] %in% info$levels)
  }
  
  # 2. Loop over each outcome
  for (i in seq_len(nrow(outcomes))) {
    yvar  <- outcomes$yvar[i]
    ylab  <- outcomes$ylab[i]
    short <- outcomes$short[i]
    
    # Build analysis df for this outcome + income set
    df_model <- df_subset %>%
      dplyr::select(
        Country,
        Third_sector_size = Total_tpt,
        World_bank_income_group = .data[[income_var]],
        dplyr::all_of(yvar)
      ) %>%
      stats::na.omit()
    
    # Skip small samples
    if (nrow(df_model) < 5) {
      message("Skipping ", set_name, " / ", yvar, " (n = ", nrow(df_model), ").")
      next
    }
    
    # Fit model
    fmla  <- stats::as.formula(paste0(yvar, " ~ Third_sector_size"))
    model <- stats::lm(fmla, data = df_model)
    
    # Name prefix for files (tables, diagnostics)
    name_prefix <- paste0("inform_", short, "_", info$suffix)
    
    # a. Save tables + diagnostics 
    save_model_outputs(model, name_prefix, df_model)
    
    # b. Regression plot with annotation box
    plot_title <- paste(
      "Third Sector Size vs", ylab, "–", info$label
    )
    
    outfile <- here::here(
      "outputs", "figures",
      paste0("regplot_thirdsector_", short, "_", info$suffix, ".png")
    )
    
    plot_regression_third_sector(
      df      = df_model,
      yvar    = yvar,
      ylab    = ylab,
      title   = plot_title,
      outfile = outfile,
      model   = model  
    )
  }
}

# -----------------------
# 4) Session info snapshot
# -----------------------
sink(here::here("outputs","regressions","99_sessionInfo_inform.txt"))
print(sessionInfo())
sink()

message("INFORM regressions complete. See outputs/regressions and outputs/figures.")
