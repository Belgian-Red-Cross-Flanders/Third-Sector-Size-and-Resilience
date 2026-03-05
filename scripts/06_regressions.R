# 06_regressions_inform.R
# -----------------------------------------------------------
# Regressions 
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
    "adaptive_capacity_ndgain",
    "deaths_per_100k_all_emdat_fw_15_24",
    "affected_per_100k_all_emdat_fw_15_24",
    "volunteers_staff_total_ifrc_2024",
    "Total_tpt"
  ),
  ylab  = c(
    "INFORM Coping Capacity (%)",
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)",
    "Deaths in Wildfires/Floods (2015-2024, per 100k)",
    "Affected by Wildfires/Floods (2015-2024, per 100k)",
    "Red Cross Personnel (per 100k)",
    "Third Sector Size (%)"
  ),
  short = c(
    "inf_coping",
    "ndgain_readiness",
    "ndgain_adaptive",
    "deaths_fw_15_24",
    "aff_fw_15_24",
    "rc_pers_2024",
    "third_sector_size"
  )
)

income_sets <- load_income_list()

xvar <- 'people_first_aid_per_100k_ifrc_2024'
xlab <- 'People trained in First Aid (per 100k)'


# -----------------------
# Run regressions for all outcomes × income sets
# -----------------------

# Container to collect regression summaries for this x variable
reg_rows <- list()

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
    
    folder_name <- paste(xvar, "_", short)
    
    # Create output folders 
    dir.create(here::here("outputs", "regressions", folder_name), recursive = TRUE, showWarnings = FALSE)
    dir.create(here::here("outputs", "figures", folder_name),      recursive = TRUE, showWarnings = FALSE)
    
    
    # Build analysis df for this outcome + income set
    df_model <- df_subset %>%
      dplyr::select(
        Country,
        xvar = xvar,
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
    fmla  <- stats::as.formula(paste0(yvar, " ~ xvar"))
    model <- stats::lm(fmla, data = df_model)
    
    # Name prefix for files (tables, diagnostics)
    name_prefix <- paste0(xvar, short, "_", info$suffix)
    
    # a. Save tables + diagnostics 
    outputs <- get_model_outputs(model)
    
    # ---------------------------------------------------
    # Collect regression summary for CSV
    # ---------------------------------------------------
    # Use tidy/glance already computed in outputs
    tdy <- outputs$tidy
    gln <- outputs$glance
    
    # Row for main predictor (xvar)
    main_row <- tdy[tdy$term == "xvar", ]
    
    # Append one row to reg_rows list
    reg_rows[[length(reg_rows) + 1]] <- tibble::tibble(
      xlab          = xlab,
      income_label  = info$label,
      outcome_label = ylab,
      n             = nrow(df_model),
      estimate      = main_row$estimate,
      std_error     = main_row$std.error,
      p_value       = main_row$p.value,
      conf_low      = main_row$conf.low,
      conf_high     = main_row$conf.high,
      r_squared     = gln$r.squared,
      adj_r_squared = gln$adj.r.squared,
      shapiro_p     = outputs$shapiro_p,
      dw_stat       = outputs$dw_stat,
      dw_p          = outputs$dw_p,
      bp_stat       = outputs$bp_stat,
      bp_p          = outputs$bp_p
    )
    
    
    # 1) Save tables to Excel
    out_xlsx <- here::here("outputs","regressions", folder_name, paste0(name_prefix, "_tables.xlsx"))
    writexl::write_xlsx(
      list(
        tidy      = outputs$tidy,
        glance    = outputs$glance,
        augment   = outputs$augment,
        infer     = outputs$diagnostics
      ),
      path = out_xlsx
    )
    
    # 2) Write summary + interpretation to TXT
    out_txt <- here::here("outputs","regressions", folder_name, paste0(name_prefix, "_summary.txt"))
    capture.output({
      cat("\n=== MODEL SUMMARY ===\n")
      print(summary(model))
      
      cat("\n=== DIAGNOSTICS ===\n")
      print(outputs$diagnostics)
      
      cat(outputs$interpretation_text)
    }, file = out_txt)
    
    # 3) Save diagnostic plots (if you want)
    ggplot2::ggsave(
      here::here("outputs","figures", folder_name, paste0(name_prefix, "_residuals_vs_fitted.png")),
      outputs$plot_resid, width = 7.2, height = 4.8, dpi = 300
    )
    
    ggplot2::ggsave(
      here::here("outputs","figures", folder_name, paste0(name_prefix, "_qqplot.png")),
      outputs$plot_qq, width = 7.2, height = 4.8, dpi = 300
    )
    
    
    # b. Regression plot with annotation box
    plot_title <- paste(
      xlab, " vs", ylab, "–", info$label
    )
    
    outfile <- here::here(
      "outputs", "figures", folder_name, 
      paste0("regplot_", short, "_", info$suffix, ".png")
    )
    
    plot_regression_third_sector(
      df      = df_model,
      xvar = 'xvar',
      xlab = xlab,
      yvar    = yvar,
      ylab    = ylab,
      title   = plot_title,
      outfile = outfile,
      model   = model  
    )
  }
}

# -----------------------
# Export combined regression summary for this x variable
# -----------------------
if (length(reg_rows) > 0) {
  reg_summary <- dplyr::bind_rows(reg_rows)
  
  # Safe filename based on xvar
  safe_xname <- gsub("[^A-Za-z0-9]+", "_", xvar)
  
  out_csv <- here::here(
    "outputs", "regressions",
    paste0("summary_regressions_", safe_xname, ".csv")
  )
  
  readr::write_csv(reg_summary, out_csv)
  message("Saved combined regression summary to: ", out_csv)
} else {
  warning("No regression rows collected; summary CSV not written.")
}

# -----------------------
# 4) Session info snapshot
# -----------------------
sink(here::here("outputs","regressions","99_sessionInfo_inform.txt"))
print(sessionInfo())
sink()

message(" regressions complete. See outputs/regressions and outputs/figures.")
