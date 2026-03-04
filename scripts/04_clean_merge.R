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
    Total,
    population,
    democ,
    gdpPerCapita,
    World.Bank.Income.Group
  ) %>%
  # standardize country names
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    
    # convert numeric-like columns properly
    Total = as.numeric(as.character(Total)),
    population = as.numeric(as.character(population)),
    democ = as.numeric(as.character(democ)),
    gdpPerCapita = as.numeric(as.character(gdpPerCapita))
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
    risk = as.numeric(as.character(`INFORM.RISK`)),
    hazard_exposure = as.numeric(as.character(`HAZARD.&.EXPOSURE`)),
    vulnerability = as.numeric(as.character(`VULNERABILITY`)),
    lack_coping_cap = as.numeric(as.character(`LACK.OF.COPING.CAPACITY`))
  ) %>%
  # convert 0–10 scale to 0–100 (percent-like)
  dplyr::mutate(
    risk = risk * 10,
    hazard_exposure = hazard_exposure * 10,
    vulnerability = vulnerability * 10,
    lack_coping_cap = lack_coping_cap * 10
  ) %>%
  # create *coping capacity* = inverse of lack of coping
  dplyr::mutate(
    coping_capacity = 100 - lack_coping_cap
  ) %>%
  dplyr::select(
    Country,
    risk,
    hazard_exposure,
    vulnerability,
    lack_coping_cap,
    coping_capacity
  ) %>%
  suffix_vars("inform")


                
# -----------------------
# CLEAN ND-GAIN
# -----------------------
ndgain <- raw$ndgain_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    readiness = as.numeric(as.character(`readiness`)),
    readiness_economic = as.numeric(as.character(`readiness_economic`)),
    readiness_governance = as.numeric(as.character(`readiness_governance`)),
    readiness_social = as.numeric(as.character(`readiness_social`)),
    vuln_adaptive_capacity = as.numeric(as.character(`vulnerability_adaptive_capacity`))
  ) %>%
  # convert 0–1 scale to 0–100 (percent-like)
  dplyr::mutate(
    readiness = readiness * 100,
    readiness_economic = readiness_economic * 100,
    readiness_governance = readiness_governance * 100,
    readiness_social = readiness_social * 100,
    vuln_adaptive_capacity = vuln_adaptive_capacity * 100
  ) %>%
  # create *adaptive capacity* = inverse of vulnerability adaptive capacity
  dplyr::mutate(
    adaptive_capacity = 100 - vuln_adaptive_capacity
  ) %>%
  dplyr::select(
    Country,
    readiness,
    readiness_economic,
    readiness_governance,
    readiness_social,
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
# CLEAN FLOOD/WILDFIRE DISASTER DATA (EM-DAT)
# -----------------------
emdat_fw <- raw$emdat_raw_fw %>%
  dplyr::rename(
    disaster_type  = "Disaster.Type",
    start_year     = "Start.Year",
    total_deaths   = "Total.Deaths",
    total_affected = "Total.Affected"
  ) %>%
  dplyr::mutate(
    Country        = standardize_country(Country, corrections),
    total_deaths   = as.numeric(total_deaths),
    total_affected = as.numeric(total_affected)
  ) %>%
  # aggregate per Country × Disaster Type
  dplyr::group_by(Country, disaster_type) %>%
  dplyr::summarise(
    n_events       = dplyr::n(),
    deaths_total   = sum(total_deaths,   na.rm = TRUE),
    affected_total = sum(total_affected, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  # Wide format: separate columns for Flood / Wildfire
  tidyr::pivot_wider(
    id_cols     = Country,
    names_from  = disaster_type,
    values_from = c(n_events, deaths_total, affected_total),
    values_fill = NA
  ) %>%
  # Tag as EM-DAT-derived
  suffix_vars("emdat_fw") %>%
  # -----------------------
# Merge with population and create per‑capita indicators
# -----------------------
dplyr::left_join(pop, by = "Country") %>%
  dplyr::mutate(
    # deaths per 100k population (2024)
    deaths_per_100k_Flood_emdat_fw    = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      deaths_total_Flood_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    deaths_per_100k_Wildfire_emdat_fw = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      deaths_total_Wildfire_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    # affected per 100k population (2024)
    affected_per_100k_Flood_emdat_fw    = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      affected_total_Flood_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    affected_per_100k_Wildfire_emdat_fw = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      affected_total_Wildfire_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    
    # -----------------------
    # Combined totals (Flood + Wildfire)
    # -----------------------
    deaths_total_all_emdat_fw =
      rowSums(dplyr::across(c(deaths_total_Flood_emdat_fw,
                              deaths_total_Wildfire_emdat_fw)), na.rm = TRUE),
    
    affected_total_all_emdat_fw =
      rowSums(dplyr::across(c(affected_total_Flood_emdat_fw,
                              affected_total_Wildfire_emdat_fw)), na.rm = TRUE),
    
    # -----------------------
    # Combined per‑capita measures (per 100k)
    # -----------------------
    deaths_per_100k_all_emdat_fw = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      deaths_total_all_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    ),
    
    affected_per_100k_all_emdat_fw = ifelse(
      !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
      affected_total_all_emdat_fw / population_2024_pop_wb * 1e5,
      NA_real_
    )
    
  )


# -----------------------
# CLEAN FLOOD/WILDFIRE DISASTER DATA (EM-DAT)
# -----------------------
emdat_nt <- raw$emdat_raw_nt %>%
  dplyr::rename(
    disaster_type  = "Disaster.Type",
    start_year     = "Start.Year",
    total_deaths   = "Total.Deaths",
    total_affected = "Total.Affected"
  ) %>%
  dplyr::mutate(
    Country        = standardize_country(Country, corrections),
    total_deaths   = as.numeric(total_deaths),
    total_affected = as.numeric(total_affected)
  ) %>%
  # aggregate per Country
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    n_events       = dplyr::n(),
    deaths_total   = sum(total_deaths,   na.rm = TRUE),
    affected_total = sum(total_affected, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  # Tag as EM-DAT-derived (natural and technological)
  suffix_vars("emdat_nt") %>%
  # -----------------------
# Merge with population and create per‑capita indicators
# -----------------------
dplyr::left_join(pop, by = "Country") %>%
dplyr::mutate(
  deaths_per_100k_emdat_nt = ifelse(
    !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
    deaths_total_emdat_nt / population_2024_pop_wb * 1e5,
    NA_real_
  ),
  
  affected_per_100k_emdat_nt = ifelse(
    !is.na(population_2024_pop_wb) & population_2024_pop_wb > 0,
    affected_total_emdat_nt / population_2024_pop_wb * 1e5,
    NA_real_
  )
)


# 
# # -----------------------
# # CLEAN GDP (World Bank Constant 2015 USD)
# # -----------------------
# gdp <- raw$gdp_raw %>%
#   dplyr::rename(Country = Country.Name) %>%
#   
#   # wide → long
#   tidyr::pivot_longer(
#     cols = tidyselect::starts_with("X"),
#     names_to   = "Year",
#     names_prefix = "X",
#     values_to  = "GDP"
#   ) %>%
#   
#   dplyr::mutate(
#     Year = as.numeric(Year),
#     GDP  = as.numeric(GDP)
#   ) %>%
#   
#   # get the *most recent* non-NA GDP value per country
#   dplyr::arrange(Country, dplyr::desc(Year)) %>%
#   dplyr::group_by(Country) %>%
#   dplyr::filter(!is.na(GDP)) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>%
#   
#   # standardize after grouping (best practice)
#   dplyr::mutate(
#     Country = standardize_country(Country, corrections),
#     GDP_Most_Recent = GDP
#   ) %>%
#   
#   # keep both GDP value and the corresponding year
#   dplyr::select(
#     Country,
#     GDP_year = Year,
#     GDP_Most_Recent
#   )%>%
#   suffix_vars("gdp_wb")



# -----------------------
# MASTER MERGE — THIRD PILLAR AS ANCHOR
# -----------------------
master <- tpt %>%                     # <— anchor dataset 
  dplyr::left_join(inform,       by = "Country") %>%
  dplyr::left_join(ndgain,        by = "Country") %>% 
  dplyr::left_join(pop,        by = "Country") %>%
  dplyr::left_join(emdat_fw,        by = "Country") %>%
  dplyr::left_join(emdat_nt,        by = "Country")




# -----------------------
# DIAGNOSTICS
# -----------------------

diag_list <- list(
  inform      = inform$Country,
  ndgain      = ndgain$Country,
  # gdp       = gdp$Country,
  pop         = pop$Country,
  emdat_fw       = emdat_fw$Country,
  emdat_nt       = emdat_nt$Country
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

saveRDS(master, here::here("data_clean", "master_dataset.rds"))
writexl::write_xlsx(master, here::here("data_clean", "master_dataset.xlsx"))

cat("\nMaster dataset created successfully.")

