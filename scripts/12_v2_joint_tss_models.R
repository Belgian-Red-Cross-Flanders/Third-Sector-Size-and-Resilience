# "When paid staff and volunteer capacity compete in the same model, do they both matter?"
# Outcome ~ TSS-P + TSS-V
# Outcome ~ TSS-P + TSS-V + controls

# Variance Inflation Factor (VIF) measures how strongly a predictor can be explained by the other predictors in the model.
# so the VIF for TSS-P asks how redundant is TSS-P given TSS-V and the controls?
# VIF < 2     : essentially no concern
# VIF 2-5     : mild correlation
# VIF 5-10    : moderate concern
# VIF > 10    : serious multicollinearity

# source(here::here("scripts", "01_load_packages.R"))
# source(here::here("scripts", "02_functions.R"))

master <- readRDS(
  here::here("data_clean", "v2_master_dataset.rds")
)

all_controls <- c(
  "log1p_funding_per_100k_unocha",
  "natural_hazard_inform",
  "hdi_undp"
)

joint_rows <- list()

for (j in seq_len(nrow(y_vars))) {
  
  this_yvar <- y_vars$yvar[j]
  this_ylab <- y_vars$ylab[j]
  
  # ==========================================
  # COMMON SAMPLE
  # ==========================================
  
  df_common <- master %>%
    dplyr::select(
      Country,
      Paid.staff_tpt,
      Volunteers_tpt,
      dplyr::all_of(this_yvar),
      dplyr::all_of(all_controls)
    ) %>%
    stats::na.omit()
  
  if (nrow(df_common) < 15) next
  
  names(df_common)[names(df_common) == this_yvar] <- "y"
  
  # ==========================================
  # MODEL 1
  # Outcome ~ TSS-P + TSS-V
  # ==========================================
  
  model_simple <- lm(
    y ~ Paid.staff_tpt + Volunteers_tpt,
    data = df_common
  )
  
  outputs_simple <- get_model_outputs_hc3_multivar(
    model_simple
  )
  
  tdy_simple <- outputs_simple$tidy_hc3
  
  gln_simple <- outputs_simple$glance
  
  pr2_simple <- rsq::rsq.partial(model_simple)
  
  vif_simple <- car::vif(model_simple)
  
  # TSS-P row
  
  row_paid <- tdy_simple %>%
    dplyr::filter(term == "Paid.staff_tpt")
  
  pr2_paid <- pr2_simple$partial.rsq[
    pr2_simple$variable == "Paid.staff_tpt"
  ]
  
  joint_rows[[length(joint_rows) + 1]] <-
    tibble::tibble(
      Outcome = this_ylab,
      Model = "TSS-P + TSS-V",
      Predictor = "TSS-P",
      Beta = row_paid$estimate,
      SE = row_paid$std.error,
      p_value = row_paid$p.value,
      Partial_R2 = pr2_paid,
      Adj_R2 = gln_simple$adj.r.squared,
      VIF = vif_simple["Paid.staff_tpt"],
      N = nrow(df_common)
    )
  
  # TSS-V row
  
  row_vol <- tdy_simple %>%
    dplyr::filter(term == "Volunteers_tpt")
  
  pr2_vol <- pr2_simple$partial.rsq[
    pr2_simple$variable == "Volunteers_tpt"
  ]
  
  joint_rows[[length(joint_rows) + 1]] <-
    tibble::tibble(
      Outcome = this_ylab,
      Model = "TSS-P + TSS-V",
      Predictor = "TSS-V",
      Beta = row_vol$estimate,
      SE = row_vol$std.error,
      p_value = row_vol$p.value,
      Partial_R2 = pr2_vol,
      Adj_R2 = gln_simple$adj.r.squared,
      VIF = vif_simple["Volunteers_tpt"],
      N = nrow(df_common)
    )
  
  # ==========================================
  # MODEL 2
  # Outcome ~ TSS-P + TSS-V + controls
  # ==========================================
  
  model_full <- lm(
    y ~
      Paid.staff_tpt +
      Volunteers_tpt +
      log1p_funding_per_100k_unocha +
      natural_hazard_inform +
      hdi_undp,
    data = df_common
  )
  
  outputs_full <- get_model_outputs_hc3_multivar(
    model_full
  )
  
  tdy_full <- outputs_full$tidy_hc3
  
  gln_full <- outputs_full$glance
  
  pr2_full <- rsq::rsq.partial(model_full)
  
  vif_full <- car::vif(model_full)
  
  # TSS-P row
  
  row_paid <- tdy_full %>%
    dplyr::filter(term == "Paid.staff_tpt")
  
  pr2_paid <- pr2_full$partial.rsq[
    pr2_full$variable == "Paid.staff_tpt"
  ]
  
  joint_rows[[length(joint_rows) + 1]] <-
    tibble::tibble(
      Outcome = this_ylab,
      Model = "TSS-P + TSS-V + controls",
      Predictor = "TSS-P",
      Beta = row_paid$estimate,
      SE = row_paid$std.error,
      p_value = row_paid$p.value,
      Partial_R2 = pr2_paid,
      Adj_R2 = gln_full$adj.r.squared,
      VIF = vif_full["Paid.staff_tpt"],
      N = nrow(df_common)
    )
  
  # TSS-V row
  
  row_vol <- tdy_full %>%
    dplyr::filter(term == "Volunteers_tpt")
  
  pr2_vol <- pr2_full$partial.rsq[
    pr2_full$variable == "Volunteers_tpt"
  ]
  
  joint_rows[[length(joint_rows) + 1]] <-
    tibble::tibble(
      Outcome = this_ylab,
      Model = "TSS-P + TSS-V + controls",
      Predictor = "TSS-V",
      Beta = row_vol$estimate,
      SE = row_vol$std.error,
      p_value = row_vol$p.value,
      Partial_R2 = pr2_vol,
      Adj_R2 = gln_full$adj.r.squared,
      VIF = vif_full["Volunteers_tpt"],
      N = nrow(df_common)
    )
}

joint_summary <- dplyr::bind_rows(joint_rows)

# Save csv
readr::write_csv(
  joint_summary,
  here::here(
    "outputs",
    "regressions",
    "v2_joint_tssp_tssv_models.csv"
  )
)

# Save excel, one worksheet per outcome
wb <- openxlsx::createWorkbook()

for(outcome in unique(joint_summary$Outcome)) {
  
  tab <- joint_summary %>%
    dplyr::filter(
      Outcome == outcome
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 3)
      ),
      p_value = dplyr::if_else(
        p_value < 0.001,
        "<0.001",
        sprintf("%.3f", p_value)
      )
    )
  
  openxlsx::addWorksheet(
    wb,
    sheetName = substr(outcome, 1, 31)
  )
  
  openxlsx::writeData(
    wb,
    sheet = substr(outcome, 1, 31),
    x = tab
  )
}

openxlsx::saveWorkbook(
  wb,
  here::here(
    "outputs",
    "regressions",
    "v2_joint_tssp_tssv_models.xlsx"
  ),
  overwrite = TRUE
)
