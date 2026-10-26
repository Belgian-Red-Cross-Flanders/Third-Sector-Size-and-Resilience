source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

master <- readRDS(here::here("data_clean", "v2_master_dataset_common.rds"))
TSS_replicated <- read.csv(here::here("data_clean", "thirdpillar_replication.csv"))
master_observed <- master %>%
  left_join(TSS_replicated, by = "Country")
master <- master_observed

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
    "EM-DAT Disaster-Affected Population (log1p, 2015-2024 average, per 100k)",
    "Reliance on non-profits in first days of crisis (%)",
    "Trust in emergency services and authorities (%)"
  )
)


x_vars <- tibble::tibble(
  xvar  = c(
    # "Total_tpt",
    # "Paid.staff_tpt",
    # "Volunteers_tpt",
    # "volunteer_sp547_eurobar"
    "TSS.observed",
    "TSS.P.observed",
    "TSS.V.observed"
    
  ),
  xlab  = c(
    # "TSS",
    # "TSS-P",
    # "TSS-V",
    # "EU-VOL"
    "TSS observed",
    "TSS-P observed",
    "TSS-V observed"
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

all_controls <- control_vars$control

# Generate all possible control combinations
ctrl_combinations <- list(character(0)) # empty = bivariate

sample_types <- c("available", "common")

for (m in seq_along(all_controls)) {
  ctrl_combinations <- c(
    ctrl_combinations,
    combn(all_controls, m, simplify = FALSE)
  )
}

for (i in seq_len(nrow(x_vars))) {
  for (j in seq_len(nrow(y_vars))) {
    
    this_xvar <- x_vars$xvar[i]
    this_xlab <- x_vars$xlab[i]
    
    this_yvar <- y_vars$yvar[j]
    this_ylab <- y_vars$ylab[j]
    
    # Loop over every control specification
    for (sample_type in sample_types) {
      
      if (sample_type == "common") {
        
        common_df <- master %>%
          dplyr::select(
            Country,
            dplyr::all_of(this_xvar),
            dplyr::all_of(this_yvar),
            dplyr::all_of(all_controls)
          ) %>%
          stats::na.omit()
        
        if (nrow(common_df) < 15) next
      }
      
      for (ctrl_set in ctrl_combinations) {
        if (sample_type == "available") {
      
          vars_to_select <- c(
            "Country",
            this_xvar,
            ctrl_set,
            this_yvar
          )
          
          df_model <- master %>%
            dplyr::select(dplyr::all_of(vars_to_select)) %>%
            stats::na.omit()
        } else {
          # common sample
          df_model <- common_df
        }
        
        if (nrow(df_model) < 15) next
        
        names(df_model)[names(df_model) == this_xvar] <- "xvar"
        names(df_model)[names(df_model) == this_yvar] <- "y"
        
        rhs <- c("xvar", ctrl_set)
        
        formula <- stats::as.formula(
          paste("y ~", paste(rhs, collapse = " + "))
        )
        
        model <- stats::lm(formula, data = df_model)
        
        outputs <- get_model_outputs_hc3_multivar(model)
        tdy     <- outputs$tidy_hc3
        gln     <- outputs$glance
        pr2 <- rsq::rsq.partial(model)
        
        partial_r2_x <- pr2$partial.rsq[
          pr2$variable == "xvar"
        ]
        
        x_row <- tdy[tdy$term == "xvar", ]
        
        if (nrow(x_row) != 1) next
        
        reg_rows[[length(reg_rows) + 1]] <- tibble::tibble(
          sample_type        = sample_type,
          ylab               = this_ylab,
          xlab               = this_xlab,
          n_controls         = length(ctrl_set),
          control            = ifelse(
            length(ctrl_set) == 0,
            "None (bivariate)",
            paste(ctrl_set, collapse = " + ")
          ),
          beta               = x_row$estimate,
          r2_adjusted        = gln$adj.r.squared,
          partial_r2         = partial_r2_x,
          se_hc3             = x_row$std.error,
          p_value_hc3        = x_row$p.value,
          n                  = nrow(df_model),
          shapiro_p          = outputs$shapiro_p,
          durbin_watson_stat = outputs$dw_stat,
          breusch_pagan_p    = outputs$bp_p
        )
      }
    }
  }
}


if (length(reg_rows) == 0) {
  stop("No valid regressions were estimated.")
}

reg_summary <- dplyr::bind_rows(reg_rows)

out_csv <- here::here(
  "outputs", "regressions",
  "v2_summary_all_x_y_controls_hc3_observed_only.csv"
)

readr::write_csv(reg_summary, out_csv)

message("Saved regression summary to: ", out_csv)

# -----------------------
# 4) Session info snapshot
# -----------------------
sink(here::here("outputs","regressions","99_sessionInfo.txt"))
print(sessionInfo())
sink()

message(" regressions complete. See outputs/regressions")