# source(here::here("scripts", "01_load_packages.R"))
# source(here::here("scripts", "02_functions.R"))

master <- readRDS(
  here::here("data_clean", "v2_master_dataset_common.rds")
)

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
    "Total_tpt",
    "Paid.staff_tpt",
    "Volunteers_tpt",
    "volunteer_sp547_eurobar"
    # "TSS.observed",
    # "TSS.P.observed",
    # "TSS.V.observed"
    
  ),
  xlab  = c(
    "TSS",
    "TSS-P",
    "TSS-V",
    "EU-VOL"
    # "TSS observed",
    # "TSS-P observed",
    # "TSS-V observed"
  ),
)

loo_rows <- list()

for (i in seq_len(nrow(x_vars))) {
  
  this_xvar <- x_vars$xvar[i]
  this_xlab <- x_vars$xlab[i]
  
  for (j in seq_len(nrow(y_vars))) {
    
    this_yvar <- y_vars$yvar[j]
    this_ylab <- y_vars$ylab[j]
    
    # ----------------------------
    # Full-sample model
    # ----------------------------
    
    df_model <- master %>%
      dplyr::select(
        Country,
        xvar = dplyr::all_of(this_xvar),
        y = dplyr::all_of(this_yvar)
      ) %>%
      stats::na.omit()
    
    if (nrow(df_model) < 15) next
    
    full_model <- lm(y ~ xvar, data = df_model)
    
    full_outputs <- get_model_outputs_hc3_multivar(
      full_model
    )
    
    full_xrow <- full_outputs$tidy_hc3 %>%
      dplyr::filter(term == "xvar")
    
    # Store full model
    
    loo_rows[[length(loo_rows) + 1]] <-
      tibble::tibble(
        xlab = this_xlab,
        ylab = this_ylab,
        omitted_country = "FULL SAMPLE",
        beta = full_xrow$estimate,
        se_hc3 = full_xrow$std.error,
        p_value = full_xrow$p.value,
        n = nrow(df_model)
      )
    
    # ----------------------------
    # Leave-one-out
    # ----------------------------
    
    countries <- unique(df_model$Country)
    
    for (ctry in countries) {
      
      df_loo <- df_model %>%
        dplyr::filter(Country != ctry)
      
      if (nrow(df_loo) < 15) next
      
      model_loo <- lm(
        y ~ xvar,
        data = df_loo
      )
      
      outputs_loo <-
        get_model_outputs_hc3_multivar(
          model_loo
        )
      
      xrow_loo <- outputs_loo$tidy_hc3 %>%
        dplyr::filter(term == "xvar")
      
      if (nrow(xrow_loo) != 1) next
      
      loo_rows[[length(loo_rows) + 1]] <-
        tibble::tibble(
          xlab = this_xlab,
          ylab = this_ylab,
          omitted_country = ctry,
          beta = xrow_loo$estimate,
          se_hc3 = xrow_loo$std.error,
          p_value = xrow_loo$p.value,
          n = nrow(df_loo)
        )
    }
  }
}

loo_summary <- dplyr::bind_rows(loo_rows)

# =====================================================
# Workbook with one tab per outcome
# =====================================================

wb <- createWorkbook()

for (outcome in unique(loo_summary$ylab)) {
  
  tab <- loo_summary %>%
    dplyr::filter(ylab == outcome) %>%
    dplyr::mutate(
      xlab = factor(
        xlab,
        levels = c(
          "TSS",
          "TSS-P",
          "TSS-V",
          "EU-VOL"
        )
      )
    ) %>%
    dplyr::arrange(
      xlab,
      omitted_country
    ) %>%
    dplyr::mutate(
      beta = round(beta, 3),
      se_hc3 = round(se_hc3, 3),
      p_value = dplyr::if_else(
        p_value < 0.001,
        "<0.001",
        sprintf("%.3f", p_value)
      )
    ) %>%
    dplyr::select(
      Predictor = xlab,
      `Omitted Country` = omitted_country,
      Beta = beta,
      SE = se_hc3,
      `p-value` = p_value,
      N = n
    )
  
  sheet_name <- substr(outcome, 1, 31)
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = tab
  )
}

saveWorkbook(
  wb,
  here::here(
    "outputs",
    "regressions",
    "v2_leave_one_out_bivariate.xlsx"
  ),
  overwrite = TRUE
)

# =====================================================
# Stability summary
# =====================================================

loo_stability <- loo_summary %>%
  dplyr::filter(
    omitted_country != "FULL SAMPLE"
  ) %>%
  dplyr::group_by(
    xlab,
    ylab
  ) %>%
  dplyr::summarise(
    beta_min = min(beta, na.rm = TRUE),
    beta_min_country = omitted_country[which.min(beta)],
    
    beta_max = max(beta, na.rm = TRUE),
    beta_max_country = omitted_country[which.max(beta)],
    
    beta_sd  = sd(beta, na.rm = TRUE),
    beta_range = beta_max - beta_min,
    
    p_min = min(p_value, na.rm = TRUE),
    p_max = max(p_value, na.rm = TRUE),
    
    .groups = "drop"
  )


full_models <- loo_summary %>%
  dplyr::filter(
    omitted_country == "FULL SAMPLE"
  ) %>%
  dplyr::select(
    xlab,
    ylab,
    full_beta = beta
  )

loo_stability <- loo_stability %>%
  dplyr::left_join(
    full_models,
    by = c("xlab", "ylab")
  )

readr::write_csv(
  loo_stability,
  here::here(
    "outputs",
    "regressions",
    "v2_leave_one_out_stability_summary.csv"
  )
)

# Plotting
# ---------------------------
# Prepare plotting data
# ---------------------------
loo_plot <- loo_summary %>%
  group_by(xlab, ylab) %>%
  mutate(
    full_beta = beta[omitted_country == "FULL SAMPLE"]
  ) %>%
  ungroup() %>%
  filter(
    omitted_country != "FULL SAMPLE"
  ) %>%
  mutate(
    beta_change = beta - full_beta,
    ylab_wrap = stringr::str_wrap(ylab, width = 35)
  )

# ---------------------------
# Label only influential points
# ---------------------------

label_df <- loo_plot %>%
  group_by(xlab, ylab) %>%
  mutate(
    abs_change = abs(beta_change)
  ) %>%
  filter(
    abs_change >= quantile(abs_change, 0.99, na.rm = TRUE)
  ) %>%
  ungroup()

# Split into two datasets
main_outcomes <- c(
  "INFORM Coping Capacity (%)",
  "ND-GAIN Readiness (%)",
  "ND-GAIN Adaptive Capacity (%)",
  "EM-DAT Disaster-Affected Population (log1p, 2015-2024 average, per 100k)"
)

survey_outcomes <- c(
  "Reliance on non-profits in first days of crisis (%)",
  "Trust in emergency services and authorities (%)"
)

loo_plot_main <- loo_plot %>%
  filter(ylab %in% main_outcomes)

loo_plot_survey <- loo_plot %>%
  filter(ylab %in% survey_outcomes)

label_df_main <- label_df %>%
  filter(ylab %in% main_outcomes)

label_df_survey <- label_df %>%
  filter(ylab %in% survey_outcomes)

# ---------------------------
# N used in each model
# ---------------------------

n_df <- loo_summary %>%
  filter(
    omitted_country == "FULL SAMPLE"
  ) %>%
  transmute(
    xlab,
    ylab_wrap = str_wrap(ylab, width = 35),
    N = n
  )


# location slightly below the data
annotation_df <- loo_plot %>%
  group_by(ylab_wrap) %>%
  summarise(
    ymin = min(beta_change, na.rm = TRUE),
    ymax = max(beta_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(
    n_df,
    by = "ylab_wrap"
  ) %>%
  mutate(
    y_n = ymin - 0.15 * (ymax - ymin)
  )

annotation_df_main <- annotation_df %>%
  filter(ylab_wrap %in% str_wrap(main_outcomes, width = 35))

annotation_df_survey <- annotation_df %>%
  filter(ylab_wrap %in% str_wrap(survey_outcomes, width = 35))





# ---------------------------
# Plot
# ---------------------------

p_main <- ggplot(
  loo_plot_main,
  aes(
    x = factor(
      xlab,
      levels = c(
        "TSS",
        "TSS-P",
        "TSS-V",
        "EU-VOL"
      )
    ),
    y = beta_change
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.6
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.45,
    size = 1.8
  ) +
  geom_text_repel(
    data = label_df_main,
    aes(label = omitted_country),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.3
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_text(
    data = annotation_df_main,
    aes(
      x = xlab,
      y = y_n,
      label = paste0("N=", N)
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  facet_wrap(
    ~ ylab_wrap,
    scales = "free_y"
  ) +
  labs(
    x = "Predictor",
    y = "Difference from full-sample coefficient",
    title = "Leave-one-out sensitivity analysis"
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 20,
      l = 10
    )
  )

ggsave(
  here::here(
    "outputs",
    "regressions",
    "v2_leave_one_out_stability_main.png"
  ),
  p_main,
  width = 12,
  height = 9,
  dpi = 300
)

p_survey <- ggplot(
  loo_plot_survey,
  aes(
    x = factor(
      xlab,
      levels = c(
        "TSS",
        "TSS-P",
        "TSS-V",
        "EU-VOL"
      )
    ),
    y = beta_change
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.6
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.45,
    size = 1.8
  ) +
  geom_text_repel(
    data = label_df_survey,
    aes(label = omitted_country),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.3
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_text(
    data = annotation_df_survey,
    aes(
      x = xlab,
      y = y_n,
      label = paste0("N=", N)
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  facet_wrap(
    ~ ylab_wrap,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    x = "Predictor",
    y = "Difference from full-sample coefficient",
    title = "Leave-one-out sensitivity analysis"
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(10, 20, 20, 10)
  )

ggsave(
  here::here(
    "outputs",
    "regressions",
    "v2_leave_one_out_stability_survey.png"
  ),
  p_survey,
  width = 12,
  height = 5,
  dpi = 300
)
