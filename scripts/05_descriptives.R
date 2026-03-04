# 05_descriptives.R
# -----------------------------------------------------------
# Descriptives for master dataset (tpt anchor)
# Ensure you’ve run 04_clean_merge.R so data_clean/master_dataset.rds exists.
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))


# 0) Load data ---------------------------------------------------------------
master <- readRDS(here::here("data_clean", "master_dataset.rds"))
dir.create(here::here("outputs", "descriptives"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures"),      recursive = TRUE, showWarnings = FALSE)

# 0.1) Helper functions ------------------------------------------------------
only_numeric <- function(df) dplyr::select(df, where(is.numeric))
nz_na <- function(x) sum(is.na(x))
z_score <- function(x) if (all(is.na(x))) x else (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)


# 2) Missingness overview ----------------------------------------------------
missing_tbl <- tibble::tibble(
  variable    = names(master),
  n_missing   = vapply(master, nz_na, numeric(1)),
  pct_missing = round(100 * n_missing / nrow(master), 1),
  class       = vapply(master, function(x) paste(class(x), collapse = ","), character(1))
) %>% dplyr::arrange(dplyr::desc(pct_missing), variable)

writexl::write_xlsx(missing_tbl, here::here("outputs", "descriptives", "01_missingness_by_variable.xlsx"))

# 3) Global descriptives (numeric only) --------------------------------------
num_df <- only_numeric(master)
desc_global <- psych::describe(num_df)
desc_global$variable <- rownames(desc_global); rownames(desc_global) <- NULL
writexl::write_xlsx(desc_global, here::here("outputs", "descriptives", "02_describe_global_numeric.xlsx"))

# 4) Descriptives by income group -------------------------------------------
# Your income group lives in Third Pillar: World.Bank.Income.Group_tpt
income_var <- "World.Bank.Income.Group_tpt"
if (!income_var %in% names(master)) {
  warning("Income group variable (World.Bank.Income.Group_tpt) not found. Skipping grouped descriptives.")
} else {
  groups <- sort(unique(master[[income_var]]))
  for (g in groups) {
    chunk <- master %>% dplyr::filter(.data[[income_var]] == g) %>% only_numeric()
    if (nrow(chunk) > 0 && ncol(chunk) > 0) {
      d <- psych::describe(chunk)
      d$variable <- rownames(d); rownames(d) <- NULL
      out <- here::here("outputs", "descriptives", paste0("03_describe_numeric_", g, ".xlsx"))
      writexl::write_xlsx(d, out)
    }
  }
}


# 5) Outcomes to plot against 3rd sector size

# Each row = one outcome you care about
# yvar  = column name in `master`
# ylab  = pretty label for y-axis and titles
# short = short id to use in filenames

outcomes <- tibble::tibble(
  yvar  = c(
    # "risk_inform",
    # "coping_capacity_inform",
    # "readiness_ndgain",
    # "adaptive_capacity_ndgain",
    # "deaths_per_100k_Flood_emdat",
    # "deaths_per_100k_Wildfire_emdat",
    # "affected_per_100k_Flood_emdat",
    # "affected_per_100k_Wildfire_emdat",
    # "deaths_per_100k_all_emdat",
    # "affected_per_100k_all_emdat"
    "deaths_per_100k_emdat_nt",
    "affected_per_100k_emdat_nt"
  ),
  ylab  = c(
    # "INFORM Risk (%)",
    # "INFORM Coping Capacity (%)",
    # "ND-GAIN Readiness (%)",
    # "ND-GAIN Adaptive Capacity (%)",
    # "Deaths in Floods (2023-2026, per 100k inhabitants)",
    # "Deaths in Wildfires (2023-2026, per 100k inhabitants)",
    # "Affected by Floods (2023-2026, per 100k inhabitants)",
    # "Affected by Wildfires (2023-2026, per 100k inhabitants)",
    # "Deaths in Wildfires/Floods (2023-2026, per 100k inhabitants)",
    # "Affected by Wildfires/Floods (2023-2026, per 100k inhabitants)"
    "Deaths in Disasters (2023-2026, per 100k inhabitants)",
    "Affected by Disasters (2023-2026, per 100k inhabitants)"
  ),
  short = c(
    # "risk_inform",
    # "coping_inform",
    # "readiness_ndgain",
    # "adaptive_ndgain",
    # "deaths_floods",
    # "deaths_fires",
    # "aff_floods",
    # "aff_fires",
    "deaths_disast",
    "aff_disast"
  )
)

# 5.1) Define income sets for plotting --------------------------------------

income_sets <- load_income_list()


# 6) Scatters for all incomes, 4 income groups and 2 combined sets ---------
  
for (set_name in names(income_sets)) {
  info <- income_sets[[set_name]]
  
  # Subset by income group(s)
  if (is.null(info$levels)) {
    df_subset <- master
  } else {
    df_subset <- master %>%
      dplyr::filter(.data[[income_var]] %in% info$levels)
  }
  
  # Skip if too small
  if (nrow(df_subset) < 3) {
    message("Skipping ", set_name, " (", nrow(df_subset), " rows).")
    next
  }
  
  # Loop over outcomes
  for (i in seq_len(nrow(outcomes))) {
    yvar  <- outcomes$yvar[i]
    ylab  <- outcomes$ylab[i]
    short <- outcomes$short[i]
    
    plot_title <- paste(
      "Third Sector Size vs", ylab, "–", info$label
    )
    
    outfile <- here::here(
      "outputs", "figures",
      paste0("scatter_thirdsector_", short, "_", info$suffix, ".png")
    )
    
    plot_outcome_vs_third_sector(
      df      = df_subset,
      yvar    = yvar,
      ylab    = ylab,
      title   = plot_title,
      outfile = outfile
    )
    
  }
  
  
  outfile <- here::here(
    "outputs", "descriptives",
    paste0("corr_", info$suffix, ".xlsx")
  )
  
  save_corr_matrix(df_subset, outfile)
}




# 12) Session info snapshot --------------------------------------------------
sink(here::here("outputs", "descriptives", "99_sessionInfo.txt"))
print(sessionInfo())
sink()

message("Descriptives complete. See outputs/descriptives and outputs/figures.")