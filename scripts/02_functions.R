# 02_functions.R

# Functions

only_numeric <- function(df) {
  df[, sapply(df, is.numeric), drop = FALSE]
}

save_corr_matrix <- function(df, outfile) {
  # 1) Keep only numeric variables
  num_df <- only_numeric(df)   # your helper: select(where(is.numeric))
  
  # 2) Drop numeric columns that are all NA
  num_df <- num_df[, colSums(!is.na(num_df)) > 0, drop = FALSE]
  
  # 3) Need at least 2 numeric vars with some data
  if (ncol(num_df) >= 2) {
    
    # psych::corr.test will handle NAs pairwise
    cor_m <- psych::corr.test(
      num_df,
      use    = "pairwise",
      method = "pearson"
    )
    
    writexl::write_xlsx(
      list(
        "cor_matrix" = as.data.frame(cor_m$r),
        "p_values"   = as.data.frame(cor_m$p),
        "n_pairs"    = as.data.frame(cor_m$n)  # optional: Ns per pair
      ),
      outfile
    )
    
    message("✓ Saved correlation matrix to: ", outfile)
    
  } else {
    warning(
      "Not enough numeric columns with data to compute correlations for: ",
      outfile
    )
  }
}

#' Reads the excel file with manual country name corrections. This file should be located in the data_raw 
#' folder of the project and must contain at least 2 cols:
#' original (incorrect name)
#' corrected (desired name)
#' @return A data frame with country name corrections.
load_country_corrections <- function() {
  # Read correction table from Excel file using a path built with here::here()
  corr <- read.xlsx(here::here("data_raw", "country_corrections.xlsx"))
  corr
}


#' Replaces country names in a vector using a lookup table with
#' `original` and `corrected` values.
#'
#' @param country_vec Character vector of country names.
#' @param corrections Data frame with columns `original` and `corrected`.
#'
#' @return A character vector with standardized country names.
#'
standardize_country <- function(country_vec, corrections) {
  out <- country_vec
  for (i in seq_len(nrow(corrections))) {
    out[out == corrections$original[i]] <- corrections$corrected[i]
  }
  trimws(out)
}

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


#' Converts WVS ISO3 country codes into full country names using the
#' predefined `wvs_country_lookup`. Unknown codes are returned unchanged.
#'
#' @param code_vec Character vector of ISO3 country codes.
#'
#' @return A character vector of country names.
#'
map_wvs_country <- function(code_vec) {
  out <- wvs_country_lookup[code_vec]
  out[is.na(out)] <- code_vec[is.na(out)]
  return(out)
}


#' Converts ESS ISO2 country codes into full country names using the
#' predefined `ess_country_lookup`. Unknown codes are returned unchanged.
#'
#' @param code_vec Character vector of ISO2 country codes.
#'
#' @return A character vector of country names.
map_ess_country <- function(code_vec) {
  out <- ess_country_lookup[code_vec]
  out[is.na(out)] <- code_vec[is.na(out)]
  return(out)
}



#' Add a suffix to variable names
#'
#' Appends a suffix to all columns except `Country`, useful when merging
#' datasets with overlapping variable names.
#'
#' @param df A data frame.
#' @param suffix A character string to append to variable names.
#'
#' @return The data frame with suffixed variable names.
suffix_vars <- function(df, suffix) {
  df %>%
    dplyr::rename_with(
      ~ ifelse(.x == "Country", .x, paste0(.x, "_", suffix))
    )
}



#' Plot an outcome variable against Third Sector Size
#'
#' Creates a scatter plot with linear fit and labels, annotating the Pearson
#' correlation between Third Sector Size (%) and the chosen outcome variable.
#'
#' @param df A data frame containing:
#'   - Third_sector_size_pct (fixed x-axis variable),
#'   - Country (for labels),
#'   - the column named in `yvar`.
#' @param yvar String; column name in `df` to plot on the y-axis.
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @param outfile File path for the saved PNG.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits
#'   (on Third Sector Size (%)).
#'
#' @return (Invisibly) the ggplot object.
plot_outcome_vs_third_sector <- function(df, yvar, ylab, title, outfile,
                                         x_limits = NULL) {
  
  # ---- Fixed X variable: Third Sector Size (%) ----
  xvar_name <- "Total_tpt"
  
  # Extract vectors
  x <- df[[xvar_name]]
  y <- df[[yvar]]
  
  # Compute correlation
  r <- cor(x, y, use = "complete.obs")
  
  # Base plot
  p <- ggplot2::ggplot(
    df,
    aes(x = .data[[xvar_name]], y = .data[[yvar]])
  ) +
    geom_point(shape = 20) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
    
    # Country labels
    ggrepel::geom_text_repel(
      aes(label = Country),
      size = 2.5,
      max.overlaps = 6,
      family = "Times New Roman"
    ) +
    
    # Correlation annotation
    annotate(
      "text",
      x = if (!is.null(x_limits)) x_limits[2] * 0.9 else max(x, na.rm = TRUE) * 0.9,
      y = max(y, na.rm = TRUE), # you can adjust this if you want it lower
      label = paste0("Correlation: ", sprintf("%.2f", r)),
      family = "Times New Roman",
      size = 4,
      hjust = 1
    ) +
    
    # X axis limits (optional)
    {
      if (!is.null(x_limits)) {
        ggplot2::scale_x_continuous(limits = x_limits)
      } else {
        ggplot2::scale_x_continuous()
      }
    } +
    
    # Titles & labels
    ggplot2::labs(
      title = title,
      x = "Third Sector Size (%)",
      y = ylab
    ) +
    
    # Clean style
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, family = "Times New Roman"),
      text = ggplot2::element_text(family = "Times New Roman")
    )
  
  ggplot2::ggsave(outfile, plot = p, device = "png",
                  width = 7.5, height = 5.2, dpi = 300)
  
  return(p)
}


#' Regression plot: outcome vs x (usually 3rd sector related) with key stats
#'
#' Creates a scatter plot with linear fit, country labels, and an annotation
#' box reporting the main regression results (slope, 95% CI, p-value, R-squared, N).
#' This is designed to match common scientific standards for presenting
#' bivariate OLS associations.
#'
#' @param df A data frame containing:
#'   - xvar (x-axis variable),
#'   - Country (for labels),
#'   - the column named in `yvar`.
#' @param xvar String; column name in `df` to plot on the x-axis.
#' @param xlab X-axis label.
#' @param yvar String; column name in `df` to plot on the y-axis.
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @param outfile File path for the saved PNG.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param model Optional `lm` object. If NULL, the function fits
#'   lm(as.formula(paste(yvar, "~ Third_sector_size")), data = df).
#'
#' @return (Invisibly) the ggplot object.
plot_regression_third_sector <- function(df, xvar, xlab, yvar, ylab, title, outfile,
                                         x_limits = NULL, model = NULL) {
  
  # Drop rows with missing x or y
  df_clean <- df %>%
    dplyr::select(Country, all_of(xvar), all_of(yvar)) %>%
    stats::na.omit()
  
  # Fit model if not supplied
  if (is.null(model)) {
    f <- as.formula(paste(yvar, "~", xvar))
    model <- lm(f, data = df_clean)
  }
  
  # Tidy model for annotations
  tdy <- broom::tidy(model, conf.int = TRUE)
  gln <- broom::glance(model)
  
  # Extract main term (slope for Third_sector_size)
  main_row <- tdy[tdy$term == xvar, ]
  beta     <- main_row$estimate
  se       <- main_row$std.error
  pval     <- main_row$p.value
  ci_l     <- main_row$conf.low
  ci_u     <- main_row$conf.high
  
  r2       <- gln$r.squared
  n_obs    <- stats::nobs(model)
  
  # Correlation (for reference, not central)
  r <- cor(df_clean[[xvar]], df_clean[[yvar]], use = "complete.obs")
  
  # Nice labels
  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", sprintf("%.3f", pval))
  }
  
  annot_text <- paste0(
    "β: ", sprintf("%.3f", beta),
    " [", sprintf("%.3f", ci_l), ", ", sprintf("%.3f", ci_u), "]\n",
    p_label, "\n",
    "R² = ", sprintf("%.2f", r2),
    "; N = ", n_obs, "\n",
    "Pearson r = ", sprintf("%.2f", r)
  )
  
  # For positioning annotation
  x_vals <- df_clean[[xvar]]
  y_vals <- df_clean[[yvar]]
  
  
  x_range <- range(x_vals, na.rm = TRUE)
  y_range <- range(y_vals, na.rm = TRUE)
  
  # Margin fractions for the label box
  x_margin <- 0.02
  y_margin <- 0.05
  
  # Place label depending on the sign of the slope:
  # - negative beta -> upper right
  # - non-negative beta -> lower right
  
  # Always on the right horizontally
  x_annot <- x_range[2] - x_margin * diff(x_range)
  
  if (beta < 0) {
    y_annot <- y_range[2] - y_margin * diff(y_range)  # near top
    vjust   <- 1   # anchor top edge
  } else {
    y_annot <- y_range[1] + y_margin * diff(y_range)
    vjust   <- 0   # anchor bottom edge 
  }
  
  # Wrap title over multiple lines if it's long
  title_wrapped <- stringr::str_wrap(title, width = 60)

  # Base plot
  p <- ggplot2::ggplot(
    df_clean,
    aes(x = .data[[xvar]], y = .data[[yvar]])
  ) +
    ggplot2::geom_point(shape = 20) +
    
    # Linear fit with 95% CI ribbon
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         color = "red", fill = "red", alpha = 0.15,
                         linewidth = 0.6) +
    
    # Country labels
    ggrepel::geom_text_repel(
      aes(label = Country),
      size = 2.5,
      max.overlaps = 6,
      family = "Times New Roman"
    ) +
    
    # Annotation box with regression stats
    ggplot2::annotate(
      "label",
      x = x_annot,
      y = y_annot,
      label = annot_text,
      hjust = 1,
      vjust = vjust,
      size = 3.2,
      family = "Times New Roman",
      label.size = 0.25,
      label.r = grid::unit(0.15, "lines"),
      label.padding = grid::unit(0.25, "lines")
    ) +
    
    # Optional x-axis limits
    {
      if (!is.null(x_limits)) {
        ggplot2::scale_x_continuous(limits = x_limits)
      } else {
        ggplot2::scale_x_continuous()
      }
    } +
    
    # Titles & labels
    ggplot2::labs(
      title = title_wrapped,
      x = xlab,
      y = ylab
    ) +
    
    # Clean, article-like theme
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(hjust = 0.5, family = "Times New Roman"),
      text         = ggplot2::element_text(family = "Times New Roman")
    )
  
  ggplot2::ggsave(outfile, plot = p, device = "png",
                  width = 7.5, height = 5.2, dpi = 300)
  
  invisible(p)
}


load_income_list <- function() {
  income_sets <- list(
    all = list(
      levels = NULL,   # all income groups
      label  = "All income groups",
      suffix = "all"
    ),
    high = list(
      levels = "High income",
      label  = "High-income countries",
      suffix = "high"
    ),
    upper_middle = list(
      levels = "Upper-middle income",
      label  = "Upper-middle-income countries",
      suffix = "upper_middle"
    ),
    lower_middle = list(
      levels = "Lower-middle income",
      label  = "Lower-middle-income countries",
      suffix = "lower_middle"
    ),
    low = list(
      levels = "Low income",
      label  = "Low-income countries",
      suffix = "low"
    ),
    upper_high = list(
      levels = c("Upper-middle income", "High income"),
      label  = "High- & Upper-Middle-Income Countries",
      suffix = "upper_high"
    ),
    lower_low = list(
      levels = c("Low income", "Lower-middle income"),
      label  = "Low- & Lower-Middle-Income Countries",
      suffix = "low_lower"
    )
  )
  
  return(income_sets)
}


# -----------------------
# Helper: compute model outputs (no saving)
# -----------------------

get_model_outputs <- function(model) {
  
  # 0. Model components
  tdy <- broom::tidy(model, conf.int = TRUE)
  gln <- broom::glance(model)
  aug <- broom::augment(model)
  
  # Extract main effect (first non-intercept)
  main_term <- tdy$term[tdy$term != "(Intercept)"][1]
  main_row  <- tdy[tdy$term == main_term, ]
  
  main_est  <- main_row$estimate
  main_p    <- main_row$p.value
  main_ci_l <- main_row$conf.low
  main_ci_u <- main_row$conf.high
  r2        <- gln$r.squared
  
  # 1. Regression assumption tests
  shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value,
                        error = function(e) NA_real_)
  dw        <- tryCatch(car::durbinWatsonTest(model), error = function(e) NA)
  bp        <- tryCatch(lmtest::bptest(model),        error = function(e) NA)
  
  dw_stat <- ifelse(is.list(dw), dw$dw, NA_real_)
  dw_p    <- ifelse(is.list(dw), dw$p,  NA_real_)
  bp_stat <- ifelse(is.list(bp), as.numeric(bp$statistic), NA_real_)
  bp_p    <- ifelse(is.list(bp), bp$p.value, NA_real_)
  
  # 2. Assumption Interpretation Logic
  interp_normality <- if (is.na(shapiro_p)) {
    "Normality test unavailable."
  } else if (shapiro_p > 0.05) {
    "Residuals appear normally distributed (Shapiro p > 0.05)."
  } else {
    "Residuals deviate from normality (Shapiro p < 0.05)."
  }
  
  interp_dw <- if (is.na(dw_stat)) {
    "Durbin–Watson test unavailable."
  } else if (dw_stat > 1.5 & dw_stat < 2.5) {
    paste0("Residuals show no evidence of autocorrelation (DW = ",
           round(dw_stat, 3), ").")
  } else {
    paste0("Possible autocorrelation detected (DW = ",
           round(dw_stat, 3), ").")
  }
  
  interp_bp <- if (is.na(bp_p)) {
    "Breusch–Pagan test unavailable."
  } else if (bp_p > 0.05) {
    "No evidence of heteroscedasticity (BP p > 0.05)."
  } else {
    "Heteroscedasticity detected (BP p < 0.05)."
  }
  
  # 3. Automatic Plain‑Language Interpretation
  effect_direction <- ifelse(main_est > 0, "increases", "decreases")
  
  effect_strength <- if (abs(main_est) < 0.5) {
    "a very small effect"
  } else if (abs(main_est) < 1.5) {
    "a modest effect"
  } else if (abs(main_est) < 3) {
    "a strong effect"
  } else {
    "a very strong effect"
  }
  
  auto_interpretation <- paste0(
    "\n\n=== AUTOMATIC INTERPRETATION ===\n",
    "Main predictor: ", main_term, "\n",
    "Effect size: ", round(main_est, 3), " (",
    effect_strength, ")\n",
    "Interpretation: A one-unit increase in ", main_term, 
    " is associated with a ",
    abs(round(main_est, 3)), "-point change in the outcome.\n",
    "Direction: The predictor ", effect_direction, " the outcome.\n",
    "Significance: The effect is ",
    if (main_p < 0.001) "highly statistically significant (p < 0.001)"
    else if (main_p < 0.05) "statistically significant (p < 0.05)"
    else "not statistically significant (p ≥ 0.05)", ".\n",
    "R-squared: The model explains ", round(r2 * 100, 1),
    "% of variance.\n\n",
    "=== ASSUMPTION CHECKS ===\n",
    "- Normality: ", interp_normality, "\n",
    "- Independence: ", interp_dw, "\n",
    "- Homoscedasticity: ", interp_bp, "\n",
    "\n--------------------------------------------\n",
    "If all assumptions are met: coefficients, SEs, ",
    "and p-values are trustworthy.\n",
    "If assumptions are violated: consider robust SEs, ",
    "transformations, or alternative models.\n"
  )
  
  # 4. Diagnostic Plots (not saved, just ggplot objects)
  p1 <- ggplot2::ggplot(aug, ggplot2::aes(.fitted, .resid)) +
    ggplot2::geom_point(shape = 20) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(hjust = 0.5)
    )
  
  qq_df <- data.frame(sample = residuals(model))
  
  p2 <- ggplot2::ggplot(qq_df, ggplot2::aes(sample = sample)) +
    ggplot2::stat_qq(shape = 20) +
    ggplot2::stat_qq_line(color = "red", linewidth = 0.5) +
    ggplot2::labs(
      title = "QQ plot of residuals"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(hjust = 0.5)
    )
  
  # 5. Collect diagnostics as a small data.frame
  diag_df <- data.frame(
    test      = c("Shapiro-Wilk (normality)",
                  "Durbin-Watson (autocorrelation)",
                  "Breusch-Pagan (heteroscedasticity)"),
    statistic = c(NA,         dw_stat,  bp_stat),
    p_value   = c(shapiro_p,  dw_p,     bp_p),
    reference = c(
      "> 0.05 desirable",
      "≈ 2.0 indicates independence",
      "> 0.05 desirable"
    )
  )
  
  # 6. Return everything as a list
  list(
    tidy        = tdy,
    glance      = gln,
    augment     = aug,
    diagnostics = diag_df,
    shapiro_p   = shapiro_p,
    dw_stat     = dw_stat,
    dw_p        = dw_p,
    bp_stat     = bp_stat,
    bp_p        = bp_p,
    main_term   = main_term,
    main_row    = main_row,
    interpretation_text = auto_interpretation,
    plot_resid  = p1,
    plot_qq     = p2
  )
}
