# 05_descriptives.R
# -----------------------------------------------------------
# Descriptives for master dataset (tpt anchor)
# Ensure you’ve run 04_clean_merge.R so data_clean/master_dataset.rds exists.
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))


# 0) Load data ---------------------------------------------------------------
master <- readRDS(here::here("data_clean", "v2_master_dataset_common.rds"))
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

message("Descriptives complete. See outputs/descriptives and outputs/figures.")