# 04_clean_merge.R
# -----------------------------------------------------------
# Clean and merge all raw datasets into one master dataset
#
# Scales:
# - TPT variables: percent of working-age population (%)
# - INFORM variables: rescaled to 0–100
#       - INFORM Lack of Coping capacity converted to "Coping Capacity" as 100 - Lack of coping capacity
# - ND-GAIN variables: rescaled to 0–100
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

# Load corrections CSV
corrections <- load_country_corrections()

# Load raw datasets
raw <- readRDS(here::here("data_clean", "raw_data.rds"))

# -----------------------
# CLEAN THIRD PILLAR SIZE
# -----------------------
tpt <- raw$third_raw %>%
  dplyr::select(
    Country,
    Paid.staff,
    Volunteers,
    Total,
    gdpPerCapita,
    World.Bank.Income.Group
  ) %>%
  # standardize country names
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    
    # convert numeric-like columns properly
    Paid.staff = as.numeric(as.character(Paid.staff)),
    Volunteers = as.numeric(as.character(Volunteers)),
    Total = as.numeric(as.character(Total)),
    gdpPerCapita = as.numeric(as.character(gdpPerCapita)),
    log1p_gdpPerCapita = log1p(gdpPerCapita)
  ) %>%
  # drop rows with missing key values
  na.omit()%>%
  suffix_vars("tpt")

# -----------------------
# CLEAN INFORM
# -----------------------
inform <- raw$inform_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    lack_coping_cap = as.numeric(as.character(`LACK.OF.COPING.CAPACITY`)),
    natural_hazard = as.numeric(as.character(`Natural.Hazard`))
  ) %>%
  # convert 0–10 scale to 0–100 (percent-like)
  dplyr::mutate(
    lack_coping_cap = lack_coping_cap * 10,
    natural_hazard = natural_hazard * 10
  ) %>%
  # create *coping capacity* = inverse of lack of coping
  dplyr::mutate(
    coping_capacity = 100 - lack_coping_cap
  ) %>%
  dplyr::select(
    Country,
    coping_capacity,
    natural_hazard
  ) %>%
  suffix_vars("inform")


                
# -----------------------
# CLEAN ND-GAIN
# -----------------------
ndgain <- raw$ndgain_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    readiness = as.numeric(as.character(`readiness`)),
    vuln_adaptive_capacity = as.numeric(as.character(`vulnerability_adaptive_capacity`))
  ) %>%
  # convert 0–1 scale to 0–100 (percent-like)
  dplyr::mutate(
    readiness = readiness * 100,
    vuln_adaptive_capacity = vuln_adaptive_capacity * 100
  ) %>%
  # create *adaptive capacity* = inverse of vulnerability adaptive capacity
  dplyr::mutate(
    adaptive_capacity = 100 - vuln_adaptive_capacity
  ) %>%
  dplyr::select(
    Country,
    readiness,
    adaptive_capacity
  ) %>%
  suffix_vars("ndgain")



# -----------------------
# CLEAN POPULATION (World Bank)
# -----------------------
pop <- raw$pop_raw %>%
  dplyr::rename(
    Country = Country.Name,
    population_2024 = "2024"
  ) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    population_2024 = as.numeric(as.character(population_2024))
  ) %>%
  dplyr::select(Country, population_2024)%>%
  suffix_vars("pop_wb")

# -----------------------
# CLEAN OWID DISASTER DATA (all disasters, 2015-2024)
# -----------------------
owid <- raw$owid_raw %>%
  dplyr::rename(
    Year_OWID = Year_OWID,
    affected_per_100k = `Total.number.of.people.affected.by.disasters.per.100,000`
  ) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    Year = as.integer(Year_OWID),
    affected_per_100k = as.numeric(affected_per_100k)
  ) %>%
  # keep only 2015–2024 to match EM-DAT window
  dplyr::filter(Year >= 2015, Year <= 2024) %>%
  # aggregate to one value per country over 2015–24
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    # cumulative affected per 100k over 2015–24
    affected_per_100k_15_24_sum  = sum(affected_per_100k, na.rm = TRUE),
    # average annual affected per 100k over 2015–24
    affected_per_100k_15_24_mean = mean(affected_per_100k, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    log1p_affected_per_100k_15_24_sum  = log1p(affected_per_100k_15_24_sum),
    log1p_affected_per_100k_15_24_mean = log1p(affected_per_100k_15_24_mean)
  ) %>%
  suffix_vars("owid")
 

# -----------------------
# CLEAN HUMANITARIAN AID FUNDING DATA (UN-OCHA) (2024)
# -----------------------
unocha_fund <- raw$unocha_fund %>%
  dplyr::rename(
    funding_usd  = "Funded.(US$)"
  ) %>%
  dplyr::mutate(
    Country        = standardize_country(Country, corrections),
    funding_usd   = as.numeric(funding_usd)
  ) %>%
  # Tag as unocha
  suffix_vars("unocha") %>%
  # -----------------------
# Merge with population and create per‑capita indicators
# -----------------------
dplyr::left_join(pop, by = "Country") %>%
  dplyr::mutate(
    funding_per_100k_unocha = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      funding_usd_unocha / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    
    log1p_funding_per_100k_unocha =
      log1p(funding_per_100k_unocha)
    
  ) %>%
# Drop helper population column (already in master from pop)
dplyr::select(-population_2024_pop_wb)

# -----------------------
# CLEAN EUROBAROMETER SURVEY DATA (2024)
# -----------------------
eurobarometer <- raw$eurobarometer %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    volunteer_sp547 = 100 * volunteer_sp547,
    family_friends_sp547 = 100 * family_friends_sp547,
    neighbors_sp547 = 100 * neighbors_sp547,
    nonprofit_sp547 = 100 * nonprofit_sp547,
    emergency_sp547 = 100 * emergency_sp547,
    authorities_sp547 = 100 * authorities_sp547,
    work_sp547 = 100 * work_sp547,
    private_sp547 = 100 * private_sp547,
    trust_es_authorities_sp547 = 100 * trust_es_authorities_sp547,
    effective_eu_fl546 = 100 * effective_eu_fl546

    ) %>%
  
  
  # Tag as eurobar
  suffix_vars("eurobar")


# -----------------------
# CLEAN HDI (UNDP)
# -----------------------
hdi <- raw$hdi %>%
  dplyr::rename(
    hdi = "HDI_2023"
  ) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    hdi   = as.numeric(hdi)
  ) %>%
  dplyr::select(Country, hdi)%>%
  suffix_vars("undp")


# -----------------------
# MASTER MERGE — THIRD PILLAR AS ANCHOR
# -----------------------
master <- tpt %>%                     # <— anchor dataset
  dplyr::left_join(pop,       by = "Country") %>%
  dplyr::left_join(unocha_fund, by = "Country") %>%
  dplyr::left_join(inform,    by = "Country") %>%
  dplyr::left_join(ndgain,    by = "Country") %>%
  dplyr::left_join(eurobarometer,by = "Country") %>%
  dplyr::left_join(owid,      by = "Country") %>%
  dplyr::left_join(hdi,      by = "Country")
  

# -----------------------
# DIAGNOSTICS
# -----------------------

diag_list <- list(
  inform      = inform$Country,
  ndgain      = ndgain$Country,
  pop         = pop$Country,
  unocha_fund = unocha_fund$Country,
  owid        = owid$Country,
  eurobarometer = eurobarometer$Country,
  hdi = hdi$Country
  )

# 1. For each dataset: which TPT countries are missing?
cat("\n=== MISSING IN DATASETS (relative to Third Pillar) ===\n")
for (nm in names(diag_list)) {
  missing <- setdiff(tpt$Country, diag_list[[nm]])
  if (length(missing) > 0) {
    cat("\n- Missing in", nm, ": ", paste(missing, collapse = ", "), "\n")
  }
}

# 2. Any duplicated country names in master?
dup_countries <- master$Country[duplicated(master$Country)]
if (length(dup_countries) > 0) {
  cat("\n=== DUPLICATED COUNTRIES IN MASTER ===\n")
  print(unique(dup_countries))
} else {
  cat("\nNo duplicated countries in master.\n")
}

# 3. Summary of country counts
cat("\n=== COUNTRY COUNTS ===\n")
cat("TPT anchor countries: ", length(unique(tpt$Country)), "\n")
cat("Master final rows:    ", nrow(master), "\n")

# 4. Countries in master but NOT recognized in tpt (should be 0)
extra <- setdiff(master$Country, tpt$Country)
if (length(extra) > 0) {
  cat("\n=== UNEXPECTED COUNTRIES IN MASTER (not in TPT) ===\n")
  print(extra)
}


# -----------------------
# SAVE OUTPUT
# -----------------------

saveRDS(master, here::here("data_clean", "v2_master_dataset.rds"))
writexl::write_xlsx(master, here::here("data_clean", "v2_master_dataset.xlsx"))

cat("\nMaster dataset created successfully.")

