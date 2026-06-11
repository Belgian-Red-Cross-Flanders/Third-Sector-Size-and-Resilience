# source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

master <- readRDS(here::here("data_clean", "master_dataset.rds"))
# master <- readRDS(here::here("data_clean", "master_climate_change.rds"))

# Outcomes where you want to adjust for aid
y_vars <- tibble::tibble(
  yvar  = c(
    "coping_capacity_inform",
    "readiness_ndgain",
    "adaptive_capacity_ndgain",
    "log1p_affected_per_100k_15_24_mean_owid",
    "nonprofit_sp547_eurobar",
    "trust_es_authorities_sp547_eurobar"
  ),
  ylab  = c(
    "INFORM Coping Capacity (%)",
    "ND-GAIN Readiness (%)",
    "ND-GAIN Adaptive Capacity (%)",
    "EM-DAT realized disaster impacts (log1p, 2015-2024 average, per 100k)",
    "Reliance on non-profits in first days of crisis (%)",
    "Trust in emergency services and authorities (%)"
  )
)


x_vars <- tibble::tibble(
  xvar  = c(
    "Total_tpt",
    "Paid.staff_tpt",
    "Volunteers_tpt"
    
  ),
  xlab  = c(
    "TSS (%)",
    "Paid Staff (%)",
    "Volunteers (%)"
  ),
)


control_vars <- tibble::tibble(
  control = c(
    "log1p_funding_per_100k_unocha",
    "natural_hazard_inform",
    "hdi_undp"
  )
)


reg_rows <- list()

for (i in seq_len(nrow(x_vars))) {
  for (j in seq_len(nrow(y_vars))) {
    
    this_xvar <- x_vars$xvar[i]
    this_xlab <- x_vars$xlab[i]
    this_yvar <- y_vars$yvar[j]
    this_ylab <- y_vars$ylab[j]
    
    # =========================
    # 1) BIVARIATE REGRESSION
    # =========================
    df_biv <- master %>%
      dplyr::select(
        Country,
        xvar = .data[[this_xvar]],
        y    = .data[[this_yvar]]
      ) %>%
      stats::na.omit()
    
    if (nrow(df_biv) >= 15) {
      
      model_biv <- stats::lm(y ~ xvar, data = df_biv)
      outputs   <- get_model_outputs_hc3_multivar(model_biv)
      tdy       <- outputs$tidy_hc3
      gln       <- outputs$glance
      
      x_row <- tdy[tdy$term == "xvar", ]
      
      if (nrow(x_row) == 1) {
        reg_rows[[length(reg_rows) + 1]] <- tibble::tibble(
          ylab               = this_ylab,
          xlab               = this_xlab,
          control            = "None (bivariate)",
          beta               = x_row$estimate,
          se_hc3             = x_row$std.error,
          p_value_hc3        = x_row$p.value,
          r2_adjusted        = gln$adj.r.squared,
          n                  = nrow(df_biv),
          shapiro_p          = outputs$shapiro_p,
          durbin_watson_stat = outputs$dw_stat,
          breusch_pagan_p    = outputs$bp_p
        )
      }
    }
    
    # ===================================
    # 2) ONE-CONTROL-AT-A-TIME REGRESSIONS
    # ===================================
    for (k in seq_len(nrow(control_vars))) {
      
      this_ctrl <- control_vars$control[k]
      
      df_model <- master %>%
        dplyr::select(
          Country,
          xvar = .data[[this_xvar]],
          ctrl = .data[[this_ctrl]],
          y    = .data[[this_yvar]]
        ) %>%
        stats::na.omit()
      
      if (nrow(df_model) < 15) next
      
      model <- stats::lm(y ~ xvar + ctrl, data = df_model)
      outputs <- get_model_outputs_hc3_multivar(model)
      tdy     <- outputs$tidy_hc3
      gln     <- outputs$glance
      
      x_row <- tdy[tdy$term == "xvar", ]
      if (nrow(x_row) != 1) next
      
      reg_rows[[length(reg_rows) + 1]] <- tibble::tibble(
        ylab               = this_ylab,
        xlab               = this_xlab,
        control            = this_ctrl,
        beta               = x_row$estimate,
        se_hc3             = x_row$std.error,
        p_value_hc3        = x_row$p.value,
        r2_adjusted        = gln$adj.r.squared,
        n                  = nrow(df_model),
        shapiro_p          = outputs$shapiro_p,
        durbin_watson_stat = outputs$dw_stat,
        breusch_pagan_p    = outputs$bp_p
      )
    }
  }
}


if (length(reg_rows) == 0) {
  stop("No valid regressions were estimated.")
}

reg_summary <- dplyr::bind_rows(reg_rows)

out_csv <- here::here(
  "outputs", "regressions",
  "summary_all_x_y_controls_hc3_tss_only.csv"
)

readr::write_csv(reg_summary, out_csv)

message("Saved regression summary to: ", out_csv)

# -----------------------
# 4) Session info snapshot
# -----------------------
sink(here::here("outputs","regressions","99_sessionInfo_inform.txt"))
print(sessionInfo())
sink()

message(" regressions complete. See outputs/regressions")