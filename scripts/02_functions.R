# 02_functions.R
library(patchwork)


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




# Pretty axis for variables that are already on the log1p scale.
# - Pick ticks in RAW units, then convert to breaks = log1p(raw_ticks)
# - tick_mode = "125"  -> 1–2–5 per decade (default, standard scientific ticks)
# - tick_mode = "by5"  -> start_raw * 5^k  (e.g., 200, 1k, 5k, 25k, 125k …)
# - axis = "x" or "y"
# - limits_trans: limits on the transformed (log1p) scale; pass NULL to auto
make_log1p_pretty_scale <- function(
    trans_values,
    axis         = c("x", "y"),
    tick_mode    = c("125", "by5"),
    n            = 8,
    limits_trans = NULL,
    start_raw    = NULL,   # optional for "by5"
    include_zero = TRUE     # add 0 tick explicitly if raw min = 0
) {
  
  axis      <- match.arg(axis)
  tick_mode <- match.arg(tick_mode)
  
  # --- RAW VALUES FROM log1p VALUES ---
  trans_vals <- trans_values[is.finite(trans_values)]
  raw_vals   <- expm1(trans_vals)
  
  raw_min <- min(raw_vals, na.rm = TRUE)
  raw_max <- max(raw_vals, na.rm = TRUE)
  
  # If nothing to plot or degenerate:
  if (!is.finite(raw_min) || !is.finite(raw_max) || raw_min == raw_max) {
    breaks_trans <- unique(trans_vals)
    lab_fun <- function(t) scales::label_number(scale_cut = scales::cut_short_scale())(expm1(t))
    scl <- if (axis == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
    return(scl(breaks = breaks_trans, labels = lab_fun, limits = limits_trans))
  }
  
  # Smallest strictly positive value (needed when raw_min = 0)
  raw_min_pos <- suppressWarnings(min(raw_vals[raw_vals > 0], na.rm = TRUE))
  if (!is.finite(raw_min_pos)) raw_min_pos <- raw_max  # extreme fallback
  
  # -------------------------------------------
  # TICK GENERATION: 1–2–5 OR ×5 SCHEME
  # -------------------------------------------
  raw_ticks <- NULL
  
  if (tick_mode == "125") {
    # ----------- 1–2–5 ticks -------------
    seeds <- c(1, 2, 5)
    
    d_min <- floor(log10(raw_min_pos))
    d_max <- ceiling(log10(raw_max))
    mags  <- 10^seq(d_min - 1, d_max + 1)
    
    cands <- as.numeric(outer(seeds, mags, `*`))
    raw_ticks <- cands[cands >= raw_min_pos & cands <= raw_max]
    
    if (include_zero && raw_min <= 0) raw_ticks <- c(0, raw_ticks)
    if (length(raw_ticks) > n) {
      raw_ticks <- raw_ticks[seq(1, length(raw_ticks), length.out = n)]
    }
    
  } else {
    # ----------- ×5 ticks -------------
    # Determine robust starting base
    if (is.null(start_raw) || !is.finite(start_raw) || start_raw <= 0) {
      start_raw <- 10^floor(log10(raw_min_pos))
    }
    
    # Find first ×5 tick <= raw_min_pos
    k_first <- floor(log(raw_min_pos / start_raw, base = 5))
    first_tick <- start_raw * 5^k_first
    
    # Build sequence upward
    seq_raw <- first_tick * 5^(0:(n * 2))
    raw_ticks <- seq_raw[seq_raw >= raw_min_pos & seq_raw <= raw_max]
    
    # Optionally include zero
    if (include_zero && raw_min <= 0) raw_ticks <- c(0, raw_ticks)
    
    # Thin to n ticks
    if (length(raw_ticks) > n) {
      raw_ticks <- raw_ticks[seq(1, length(raw_ticks), length.out = n)]
    }
  }
  
  # Convert raw ticks to log1p breaks
  breaks_trans <- log1p(raw_ticks)
  
  # Label in raw units
  lab_fun <- function(t) {
    scales::label_number(scale_cut = scales::cut_short_scale())(expm1(t))
  }
  
  # Return appropriate scale
  if (axis == "x") {
    ggplot2::scale_x_continuous(
      breaks = breaks_trans,
      labels = lab_fun,
      limits = limits_trans,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    )
  } else {
    ggplot2::scale_y_continuous(
      breaks = breaks_trans,
      labels = lab_fun,
      limits = limits_trans,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    )
  }
}



# -------------------------------------------------------------------------------------------------------------------------------------
plot_regression_third_sector <- function(df, xvar, xlab, yvar, ylab,
                                         title, outfile, outputs,
                                         x_limits = NULL, y_limits = NULL,
                                         show_y_axis = TRUE,
                                         x_pad_mult = 0.10) {
  
  # --------------------------------------------------
  # 1. Use the model + HC3 data you ALREADY computed
  # --------------------------------------------------
  model    <- outputs$model
  vcov_hc3 <- outputs$vcov_hc3
  
  main_row <- outputs$tidy_hc3 |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::slice(1)
  
  beta  <- main_row$estimate
  p_hc3 <- main_row$p.value
  ci_l  <- main_row$conf.low
  ci_u  <- main_row$conf.high
  
  r2     <- outputs$glance$adj.r.squared
  
  n_obs  <- nrow(df)
  
  # --------------------------------------------------
  # 2. Annotation text (kept lean)
  # --------------------------------------------------
  
  
  
  p_text <- if (p_hc3 < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", formatC(p_hc3, format = "f", digits = 3))
  }
  
  annot_text <- paste0(
    "95% CI (HC3): [",
    sprintf("%.3f", ci_l), ", ", sprintf("%.3f", ci_u), "]\n",
    p_text, "\n",
    "Adj. R² = ", sprintf("%.3f", r2),
    "; N = ", n_obs
  )
  
  
  
  # --------------------------------------------------
  # 3. Auto-detect log1p
  # --------------------------------------------------
  is_x_log1p <- grepl("log1p", xlab, ignore.case = TRUE)
  is_y_log1p <- grepl("log1p", ylab, ignore.case = TRUE)
  
  # --------------------------------------------------
  # 4. Define displayed x/y ranges ONCE (used everywhere)
  #    -> fixes "CI ends early" and helps labels stay inside panel
  # --------------------------------------------------
  x_vals <- df[[xvar]]
  x_range <- range(x_vals, na.rm = TRUE)
  
  # internal padding so labels have room INSIDE panel
  x_pad <- x_pad_mult * diff(x_range)
  x_plot_limits <- c(x_range[1] - x_pad, x_range[2] + x_pad)
  
  # log1p-transformed axes cannot show < 0 in transformed space
  if (is_x_log1p) x_plot_limits[1] <- max(0, x_plot_limits[1])
  
  # y limits for coord + for label constraints
  if (is.null(y_limits)) {
    y_plot_limits <- range(df[[yvar]], na.rm = TRUE)
  } else {
    y_plot_limits <- y_limits
  }
  
  # --------------------------------------------------
  # 5. Scales
  # --------------------------------------------------
  if (is_x_log1p) {
    x_scale <- make_log1p_pretty_scale(
      df[[xvar]], axis = "x",
      tick_mode = "by5", start_raw = 50, n = 8
    )
  } else {
    # Let coord_cartesian control the visible window;
    # set expand = 0 so the axis doesn't add extra space beyond x_plot_limits.
    x_scale <- ggplot2::scale_x_continuous(
      limits = NULL,
      expand = ggplot2::expansion(mult = c(0, 0))
    )
  }
  
  if (is_y_log1p) {
    y_scale <- make_log1p_pretty_scale(
      df[[yvar]], axis = "y",
      tick_mode = "by5", start_raw = 1, n = 6,
      include_zero = TRUE
    )
  } else {
    y_scale <- ggplot2::scale_y_continuous(
      limits = NULL,
      expand = ggplot2::expansion(mult = c(0, 0))
    )
  }
  
  # --------------------------------------------------
  # 6. HC3 ribbon prediction (no refit)
  #    IMPORTANT: predict over x_plot_limits so line/CI reach the shown x-range
  # --------------------------------------------------
  newx <- seq(x_plot_limits[1], x_plot_limits[2], length.out = 200)
  
  term_names <- all.vars(stats::terms(model))
  base_row <- lapply(df[term_names], function(col) {
    if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else if (is.factor(col)) {
      levels(col)[1]
    } else {
      if (is.logical(col)) FALSE else {
        lv <- unique(col[!is.na(col)])
        if (length(lv) == 0) NA else lv[1]
      }
    }
  })
  base_row <- as.data.frame(base_row, stringsAsFactors = FALSE)
  
  newdat <- base_row[rep(1, length(newx)), , drop = FALSE]
  newdat[[xvar]] <- newx
  
  Xg       <- stats::model.matrix(stats::terms(model), data = newdat)
  beta_hat <- stats::coef(model)
  fit      <- as.numeric(Xg %*% beta_hat)
  vcov_hc3 <- (vcov_hc3 + t(vcov_hc3)) / 2  # helps numerical symmetry
  
  quad <- rowSums((Xg %*% vcov_hc3) * Xg)
  se_fit <- sqrt(pmax(0, quad))
  tcrit    <- stats::qt(0.975, df = outputs$glance$df.residual)
  
  pred_df <- tibble::tibble(
    !!xvar := newx,
    fit = fit,
    lwr = fit - tcrit * se_fit,
    upr = fit + tcrit * se_fit
  )
  
  # --------------------------------------------------
  # 7. Labels: balanced selection (half/half)
  #    IMPORTANT: use df[[xvar]] consistently (no df$xvar assumptions)
  # --------------------------------------------------
  df_labs <- df %>%
    dplyr::filter(nchar(Country) <= 10) %>%
    dplyr::mutate(dist_mean = abs(.data[[xvar]] - mean(.data[[xvar]], na.rm = TRUE))) %>%
    dplyr::arrange(desc(dist_mean)) %>%
    dplyr::mutate(half = dplyr::ntile(dist_mean, 2)) %>%
    dplyr::group_by(half) %>%
    dplyr::slice_head(n = 7) %>%
    dplyr::ungroup()
  
  # small right nudge (in x units) to avoid left-boundary collisions
  nudge_right <- 0.04 * diff(x_plot_limits)
  
  # --------------------------------------------------
  # 8. Plot
  # --------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])) +
    
    ggplot2::geom_point(shape = 20, size = 1.6) +
    
    ggplot2::geom_ribbon(
      data = pred_df,
      ggplot2::aes(x = .data[[xvar]], ymin = lwr, ymax = upr),
      inherit.aes = FALSE,
      fill = "red", alpha = 0.15
    ) +
    ggplot2::geom_line(
      data = pred_df,
      ggplot2::aes(x = .data[[xvar]], y = fit),
      inherit.aes = FALSE,
      color = "red", linewidth = 0.6
    ) +
    
    # # LABELS: constrain inside panel + improve legibility
    # ggrepel::geom_label_repel(
    #   data = df_labs,
    #   ggplot2::aes(label = Country),
    #   size = 2.2,
    #   family = "Times New Roman",
    #   
    #   # readable but subtle
    #   fill = scales::alpha("white", 0.80),
    #   label.size = 0,
    #   
    #   # leader lines
    #   min.segment.length = 0,
    #   segment.size = 0.25,
    #   segment.alpha = 0.6,
    #   segment.color = "grey30",
    #   
    #   # stronger separation from points + between labels
    #   box.padding = 0.40,
    #   point.padding = 0.35,
    #   force = 2.2,
    #   force_pull = 0.4,
    #   max.overlaps = Inf,
    #   max.iter = 8000,
    #   
    #   # keep labels inside the panel
    #   xlim = x_plot_limits,
    #   ylim = y_plot_limits,
    #   
    #   # keep dense left clusters readable: stack vertically + nudge right
    #   direction = "y",
    #   nudge_x = nudge_right,
    #   
    #   seed = 123
    # ) +
    
    # annotation: bottom-right corner INSIDE panel
    
    
    ggplot2::annotate(
      "label",
      x = x_plot_limits[2],
      y = y_plot_limits[1],
      label = annot_text,
      hjust = 1,
      vjust = 0,
      size = 3,
      linewidth = 0,
      fill = scales::alpha("white", 0.85),
      label.r = grid::unit(0.15, "lines"),
      label.padding = grid::unit(0.35, "lines")
    )+
    
    x_scale +
    y_scale +
    
    # IMPORTANT: control shown x and y ranges (keeps everything inside)
    ggplot2::coord_cartesian(xlim = x_plot_limits, ylim = y_plot_limits) +
    
    ggplot2::labs(
      title = NULL,
      x = xlab,
      y = if (show_y_axis) ylab else NULL
    ) +
    
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = if (show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.text.y  = if (show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.ticks.y = if (show_y_axis) ggplot2::element_line() else ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Times New Roman")
    )
  
  if (!is.null(outfile)) {
    ggplot2::ggsave(outfile, p, dpi = 300, width = 3.6, height = 3.0)
  }
  
  return(p)
}


# Pretty, evenly spaced log1p axis (Y)
scale_y_log1p_nice <- function(y_log_values, n = 7, limits = NULL) {
  y_log_min <- min(y_log_values, na.rm = TRUE)
  y_log_max <- max(y_log_values, na.rm = TRUE)

  break_vals <- seq(y_log_min, y_log_max, length.out = n)
  label_vals <- expm1(break_vals)
  label_fmt  <- scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1)(label_vals)

  ggplot2::scale_y_continuous(
    limits = limits,
    breaks = break_vals,
    labels = label_fmt,
    guide  = ggplot2::guide_axis(check.overlap = TRUE)
  )
}

make_2x2_outcome_figure <- function(
    master, outcome_row, inputs
) {
  
  yvar  <- outcome_row$yvar
  ylab  <- outcome_row$ylab
  short <- outcome_row$short
  
  # --- global y limits (shared across panels) ---
  y_vals <- master[[yvar]]
  y_limits <- range(y_vals, na.rm = TRUE)
  
  plots <- vector("list", 4)
  
  for (j in seq_len(nrow(inputs))) {
    
    this_xvar <- inputs$xvar[j]
    xlab      <- inputs$xlab[j]
    
    df_model <- master %>%
      dplyr::select(
        Country,
        xvar = .data[[this_xvar]],
        dplyr::all_of(yvar)
      ) %>%
      dplyr::filter(!is.na(xvar), !is.na(.data[[yvar]]))
    
    model   <- lm(as.formula(paste0(yvar, " ~ xvar")), data = df_model)
    outputs <- get_model_outputs_hc3_multivar(model)
    
    plots[[j]] <- plot_regression_third_sector(
      df          = df_model,
      xvar        = "xvar",
      xlab        = xlab,
      yvar        = yvar,
      ylab        = ylab,
      title       = NULL,
      outfile     = NULL,          # IMPORTANT: do not save individual plots
      outputs     = outputs,
      y_limits    = y_limits,
      show_y_axis = j %in% c(1, 3)  # left column only
    )
  }
  
  # --- assemble 2x2 ---
  fig <- (plots[[1]] | plots[[2]]) /
    (plots[[3]] | plots[[4]])
}

##
get_model_outputs_hc3 <- function(model) {
  # 1) HC3 robust covariance matrix
  vcov_hc3 <- sandwich::vcovHC(model, type = "HC3")
  
  # 2) OLS tidy
  tdy_ols <- broom::tidy(model, conf.int = TRUE)
  
  # 3) Correct robust SE / CI / p (finite df)
  se_hc3 <- sqrt(diag(vcov_hc3))
  ci_hc3 <- lmtest::coefci(model, vcov. = vcov_hc3, level = 0.95)
  
  coef_names <- names(coef(model))
  main_coef  <- coef_names[coef_names != "(Intercept)"][1]
  lh         <- car::linearHypothesis(model, main_coef, vcov. = vcov_hc3, test = "F")
  p_value_hc3 <- lh$`Pr(>F)`[2]
  
  # Build tidy-like robust table (keep same structure you use downstream)
  tdy_hc3 <- tibble::tibble(
    term      = coef_names,
    estimate  = coef(model),
    std.error = se_hc3,
    conf.low  = ci_hc3[, 1],
    conf.high = ci_hc3[, 2],
    p.value   = c(NA_real_, p_value_hc3) # NA for intercept, robust p for slope
  )
  
  gln <- broom::glance(model)
  aug <- broom::augment(model)
  
  # Extract main effect rows
  main_row_ols <- tdy_ols[tdy_ols$term != "(Intercept)", ][1, ]
  main_row_hc3 <- tdy_hc3[tdy_hc3$term != "(Intercept)", ][1, ]
  
  # Diagnostics (unchanged)
  shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value,
                        error = function(e) NA_real_)
  dw <- tryCatch(car::durbinWatsonTest(model), error = function(e) NA)
  bp <- tryCatch(lmtest::bptest(model),        error = function(e) NA)
  
  dw_stat <- ifelse(is.list(dw), dw$dw, NA_real_)
  dw_p    <- ifelse(is.list(dw), dw$p,  NA_real_)
  bp_stat <- ifelse(is.list(bp), as.numeric(bp$statistic), NA_real_)
  bp_p    <- ifelse(is.list(bp), bp$p.value,              NA_real_)
  
  diag_df <- data.frame(
    test      = c("Shapiro-Wilk (normality)",
                  "Durbin-Watson (autocorrelation)",
                  "Breusch-Pagan (heteroscedasticity)"),
    statistic = c(NA,         dw_stat,  bp_stat),
    p_value   = c(shapiro_p,  dw_p,     bp_p),
    reference = c("> 0.05 desirable",
                  "≈ 2.0 indicates independence",
                  "> 0.05 desirable")
  )
  
  # Return: add model & vcov_hc3 so plots can reuse them
  list(
    model          = model,          # <— NEW
    vcov_hc3       = vcov_hc3,       # <— NEW
    
    tidy_ols       = tdy_ols,
    tidy_hc3       = tdy_hc3,
    glance         = gln,
    augment        = aug,
    diagnostics    = diag_df,
    shapiro_p      = shapiro_p,
    dw_stat        = dw_stat,
    dw_p           = dw_p,
    bp_stat        = bp_stat,
    bp_p           = bp_p,
    main_term_ols  = main_row_ols$term,
    main_row_ols   = main_row_ols,
    main_term_hc3  = main_row_hc3$term,
    main_row_hc3   = main_row_hc3
  )
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


get_model_outputs_hc3_multivar <- function(model) {
  
  # HC3 vcov
  vcov_hc3 <- sandwich::vcovHC(model, type = "HC3")
  
  # OLS tidy
  tdy_ols <- broom::tidy(model, conf.int = TRUE)
  
  # Robust SE (manual, reliable)
  se_hc3 <- sqrt(diag(vcov_hc3))
  
  # Robust CI (t-based)
  ci_hc3 <- lmtest::coefci(model, vcov. = vcov_hc3, level = 0.95)
  
  # df for t-tests
  df_resid <- model$df.residual
  
  # Compute HC3 p-values using car::linearHypothesis for each term
  coef_names <- names(coef(model))
  
  pvals_hc3 <- sapply(coef_names, function(term) {
    if (term == "(Intercept)") return(NA_real_)
    lh <- car::linearHypothesis(model, term, vcov. = vcov_hc3, test = "F")
    lh$`Pr(>F)`[2]
  })
  
  # Build HC3 tidy results table
  tdy_hc3 <- tibble::tibble(
    term      = coef_names,
    estimate  = coef(model),
    std.error = se_hc3,
    conf.low  = ci_hc3[, 1],
    conf.high = ci_hc3[, 2],
    p.value   = pvals_hc3
  )
  
  # Other model info
  gln <- broom::glance(model)
  aug <- broom::augment(model)
  
  # Diagnostics
  shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value,
                        error = function(e) NA_real_)
  dw         <- tryCatch(car::durbinWatsonTest(model), error = function(e) NA)
  bp         <- tryCatch(lmtest::bptest(model),        error = function(e) NA)
  
  dw_stat <- ifelse(is.list(dw), dw$dw, NA_real_)
  dw_p    <- ifelse(is.list(dw), dw$p,  NA_real_)
  bp_stat <- ifelse(is.list(bp), as.numeric(bp$statistic), NA_real_)
  bp_p    <- ifelse(is.list(bp), bp$p.value, NA_real_)
  
  list(
    model   = model,
    vcov_hc3 = vcov_hc3,
    tidy_ols = tdy_ols,
    tidy_hc3 = tdy_hc3,
    glance   = gln,
    augment  = aug,
    shapiro_p = shapiro_p,
    dw_stat   = dw_stat,
    dw_p      = dw_p,
    bp_stat   = bp_stat,
    bp_p      = bp_p
  )
}


library(patchwork)

make_4y_sharedx_figure <- function(master, outcomes, input) {
  
  this_xvar <- input$xvar[1]
  xlab      <- input$xlab[1]
  
  plots <- vector("list", 4)
  
  # ---- compute shared x limits ONCE (important)
  x_vals_all <- master[[this_xvar]]
  x_range    <- range(x_vals_all, na.rm = TRUE)
  x_pad      <- 0.10 * diff(x_range)
  x_limits   <- c(x_range[1] - x_pad, x_range[2] + x_pad)
  
  for (i in seq_len(nrow(outcomes))) {
    
    yvar  <- outcomes$yvar[i]
    ylab  <- outcomes$ylab[i]
    
    df_model <- master %>%
      dplyr::select(
        Country,
        xvar = .data[[this_xvar]],
        dplyr::all_of(yvar)
      ) %>%
      dplyr::filter(!is.na(xvar), !is.na(.data[[yvar]]))
    
    # skip tiny samples defensively
    if (nrow(df_model) < 10) {
      plots[[i]] <- ggplot() + theme_void()
      next
    }
    
    model   <- stats::lm(stats::as.formula(paste0(yvar, " ~ xvar")), data = df_model)
    outputs <- get_model_outputs_hc3_multivar(model)
    
    plots[[i]] <- plot_regression_third_sector(
      df          = df_model,
      xvar        = "xvar",
      xlab        = xlab,
      yvar        = yvar,
      ylab        = ylab,
      title       = NULL,
      outfile     = NULL,          # do NOT save individual panels
      outputs     = outputs,
      x_limits    = x_limits,
      show_y_axis = TRUE           # all panels keep y labels
    )
  }
  
  # ---- remove x-axis text & ticks from top row
  plots[[1]] <- plots[[1]] + theme(axis.title.x = element_blank(),
                                   axis.text.x  = element_blank(),
                                   axis.ticks.x = element_blank())
  
  plots[[2]] <- plots[[2]] + theme(axis.title.x = element_blank(),
                                   axis.text.x  = element_blank(),
                                   axis.ticks.x = element_blank())
  
  # ---- assemble 2x2 with shared x visually aligned
  fig <- (plots[[1]] | plots[[2]]) /
    (plots[[3]] | plots[[4]]) +
    plot_layout(guides = "collect")
  
  fig
}



