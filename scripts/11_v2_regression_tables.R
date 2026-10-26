reg_summary <- readr::read_csv(
  here::here(
    "outputs",
    "regressions",
    "v2_summary_all_x_y_controls_hc3_observed_only.csv"
  )
)



for (this_sample in unique(reg_summary$sample_type)){
  wb <- createWorkbook()
  for (outcome in unique(reg_summary$ylab)) {
    
    tab <- reg_summary %>%
      dplyr::filter(
        ylab == outcome,
        sample_type == this_sample
      ) %>%
      mutate(
        xlab = factor(
          xlab,
          levels = c(
            "TSS observed",
            "TSS-P observed",
            "TSS-V observed"
            # "EU-VOL"
          )
        ),
        specification = case_when(
          control == "None (bivariate)" ~ "Bivariate",
          TRUE ~ paste("+", control)
        )
      ) %>%
      mutate(
        specification = stringr::str_replace_all(
          specification,
            c(
              "log1p_funding_per_100k_unocha" = "H. Aid",
              "natural_hazard_inform" = "N. Haz.",
              "hdi_undp" = "HDI"
            )
          )
        ) %>%
      dplyr::arrange(xlab, n_controls, control) %>%
      dplyr::select(
        Predictor = xlab,
        Regression = specification,
        Beta = beta,
        SE = se_hc3,
        `p-value` = p_value_hc3,
        `Partial R²` = partial_r2,
        `Adjusted R²` = r2_adjusted,
        N = n,
        `Shapiro-Wilk (p)` = shapiro_p,
        `Durbin-Watson` = durbin_watson_stat,
        `Breusch-Pagan (p)` = breusch_pagan_p
      )%>%
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 3)
        ),
        `p-value` = dplyr::if_else(
          `p-value` < 0.001,
          "<0.001",
          sprintf("%.3f", `p-value`)
        ),
        `Shapiro-Wilk (p)` = dplyr::if_else(
          `Shapiro-Wilk (p)` < 0.001,
          "<0.001",
          sprintf("%.3f", `Shapiro-Wilk (p)`)
        ),
        `Breusch-Pagan (p)` = dplyr::if_else(
          `Breusch-Pagan (p)` < 0.001,
          "<0.001",
          sprintf("%.3f", `Breusch-Pagan (p)`)
        )
      )
    
    addWorksheet(wb, substr(outcome, 1, 31))
    
    writeData(
      wb,
      sheet = substr(outcome, 1, 31),
      x = tab
    )
  }
  
  saveWorkbook(
    wb,
    here::here(
      "outputs",
      "regressions",
      paste0("v2_regression_tables_", this_sample,"_observed.xlsx")
    ),
    overwrite = TRUE
  )
}

