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
writexl::write_xlsx(desc_global, here::here("outputs", "descriptives", "02_describe_global_numeric_tss.xlsx"))



# 4) Correlations among third-sector lenses (GLOBAL, all countries) ----------
# Variables you want to compare among each other (third-sector lenses).
third_sector_vars <- c(
  "Total_tpt",
  "Paid.staff_tpt",
  "Volunteers_tpt",
  "volunteer_sp547_eurobar"
)

# Keep only variables that exist and are numeric
vars_exist <- intersect(third_sector_vars, names(master))

third_df <- master %>%
  dplyr::select(dplyr::all_of(vars_exist)) %>%
  dplyr::select(where(is.numeric))

# Drop columns with too few non-missing values (e.g., < 20 rows)
min_n <- 20
enough_data <- vapply(third_df, function(x) sum(!is.na(x)), numeric(1)) >= min_n
third_df <- third_df[, enough_data, drop = FALSE]

# (Optional) rename columns to shorter labels for plots
short_names <- tibble::tibble(
  old = names(third_df),
  new = dplyr::recode(old,
                      Total_tpt = "Third sector size (TSS)",
                      Staff_tpt = "TSS-S",
                      Volunteers_tpt = "TSS-V",
                      volunteer_sp547_eurobar = "Eurobarometer: volunteering (%)"
  )
)

names(third_df) <- short_names$new

# Helper: from Hmisc::rcorr object to tidy data frame
tidy_rcorr <- function(rc) {
  r  <- rc$r
  p  <- rc$P
  n  <- rc$n
  
  # Melt to long
  to_df <- function(M, nm) {
    as.data.frame(as.table(M), stringsAsFactors = FALSE) |>
      stats::setNames(c("var1", "var2", nm))
  }
  dfr <- to_df(r, "estimate")
  dfp <- to_df(p, "p_value")
  dfn <- to_df(n, "n")
  
  dplyr::left_join(dfr, dfp, by = c("var1","var2")) |>
    dplyr::left_join(dfn, by = c("var1","var2")) |>
    dplyr::filter(var1 != var2) |>
    dplyr::mutate(across(c(estimate, p_value, n), as.numeric))
}

# Compute Spearman (pairwise complete)
suppressPackageStartupMessages(library(Hmisc))
spear <- Hmisc::rcorr(as.matrix(third_df), type = "spearman")

spear_tidy <- tidy_rcorr(spear)


# 5) Correlations: confounders vs third sector + outcomes ---------------------

third_sector_vars <- c(
  "Total_tpt",
  "log1p_volunteers_staff_per_100k_ifrc_2024",
  "log1p_income_per_100k_ifrc_2024",
  "log1p_people_first_aid_per_100k_ifrc_2024"
)

outcome_vars <- c(
  "coping_capacity_inform",
  "readiness_ndgain",
  "adaptive_capacity_ndgain",
  "log1p_affected_per_100k_15_24_sum_owid"
)

confounder_vars <- c(
  "hdi_undp",
  "gdpPerCapita_tpt",
  "log1p_funding_per_100k_unocha",
  "natural_hazard_inform"
)

all_needed <- c(third_sector_vars, outcome_vars, confounder_vars)

# Keep only those that exist in master and are numeric
vars_exist <- intersect(all_needed, names(master))

corr_raw <- master %>%
  dplyr::select(dplyr::all_of(vars_exist)) %>%
  dplyr::select(where(is.numeric))

# Drop variables with too few non-missing values
min_n <- 20
enough_data <- vapply(corr_raw, function(x) sum(!is.na(x)), numeric(1)) >= min_n
corr_raw <- corr_raw[, enough_data, drop = FALSE]

# Identify which survived the filtering
vars_kept <- names(corr_raw)
third_kept <- intersect(third_sector_vars, vars_kept)
outcome_kept <- intersect(outcome_vars, vars_kept)
conf_kept <- intersect(confounder_vars, vars_kept)

# Compute Spearman correlations
suppressPackageStartupMessages(library(Hmisc))
rc <- Hmisc::rcorr(as.matrix(corr_raw), type = "spearman")

# rc$r is a full matrix; we subset rows = confounders, cols = third+outcomes
r_mat <- rc$r

r_sub <- r_mat[conf_kept, c(third_kept, outcome_kept), drop = FALSE]

# Optional: rename for nicer labels in the heatmap
row_labels <- dplyr::recode(
  conf_kept,
  hdi_undp                      = "HDI",
  gdpPerCapita_tpt              = "GDP per capita (TPT)",
  log1p_funding_per_100k_unocha = "Humanitarian funding (log1p, /100k)",
  natural_hazard_inform         = "Natural hazard risk (INFORM)"
)

col_labels <- dplyr::recode(
  c(third_kept, outcome_kept),
  Total_tpt                           = "Third sector size (TPT)",
  log1p_volunteers_staff_per_100k_ifrc_2024 = "RC volunteers+staff (log1p, /100k)",
  log1p_income_per_100k_ifrc_2024           = "RC income (log1p, /100k)",
  log1p_people_first_aid_per_100k_ifrc_2024 = "First‑aid trained (log1p, /100k)",
  
  coping_capacity_inform              = "INFORM Coping Capacity (%)",
  readiness_ndgain                    = "ND‑GAIN Readiness (%)",
  adaptive_capacity_ndgain            = "ND‑GAIN Adaptive Capacity (%)",
  log1p_affected_per_100k_15_24_sum_owid =
    "Affected by disasters (log1p, /100k)"
)

# Build a data frame suitable for heatmap plotting
corr_rect_df <- as.data.frame(r_sub)
rownames(corr_rect_df) <- row_labels
names(corr_rect_df)    <- col_labels

# Melt to long format
long_rect <- reshape2::melt(
  as.matrix(corr_rect_df),
  varnames = c("Confounder", "Variable"),
  value.name = "r"
)

# Plot rectangular heatmap: x = Variable (third+outcomes), y = Confounder
p_rect <- ggplot(long_rect,
                 aes(x = Variable, y = Confounder, fill = r)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#1a9850",
    midpoint = 0, limits = c(-1, 1), name = "Spearman r"
  ) +
  labs(
    title = "Correlations between confounders\nand third‑sector / outcome variables",
    x = NULL, y = NULL
  ) +
  coord_equal() +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid  = element_blank(),
    plot.title  = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  here::here("outputs", "figures",
             "corr_confounders_vs_thirdsector_outcomes_spearman.png"),
  p_rect, width = 10, height = 4, dpi = 300
)



# 12) Session info snapshot --------------------------------------------------
sink(here::here("outputs", "descriptives", "99_sessionInfo.txt"))
print(sessionInfo())
sink()

message("Descriptives complete. See outputs/descriptives and outputs/figures.")