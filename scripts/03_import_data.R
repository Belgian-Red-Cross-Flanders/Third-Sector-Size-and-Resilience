# 03_import_data.R

# Load helper scripts
source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

# -----------------------------
# IMPORT RAW DATASETS
# -----------------------------

# 1. Third sector size
third_raw <- read.xlsx(here::here("data_raw", "All variables_thirdpillar.xlsx"))
# 2. INFORM index
inform_raw <- read.xlsx(here::here("data_raw", "INFORM.xlsx"))
# 3. ND-GAIN index
ndgain_raw <- read.xlsx(here::here("data_raw", "ND_GAIN_2023.xlsx"))
# 4. OWID disasters (all disasters, long time series)
owid_raw <- read.xlsx(here::here("data_raw", "OWID_disasters.xlsx"))
# 6. Population data
pop_raw <- read.xlsx(here::here("data_raw", "world_bank_population_2024.xlsx"))
# 7. IFRC people data
ifrc_nat_raw <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="National Societies")
ifrc_firstaid <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People trained in First Aid")
ifrc_income <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="Income") # in CHF, Swiss Franc
# 8. UN-OCHA FTS humanitarian funding
unocha_fund <- read.xlsx(here::here("data_raw", "un_ocha_funding_2024.xlsx"))
# 9. Eurobarometer surveys data
eurobarometer <- read.xlsx(here::here("data_raw", "eurobarometer 2024.xlsx"))
# 10. HDI (UNDP) 2023
hdi <- read.xlsx(here::here("data_raw", "UNDP_HDI.xlsx")) 


# Save raw list ready for cleaning
saveRDS(
  list(
    third_raw = third_raw,
    inform_raw = inform_raw,
    ndgain_raw = ndgain_raw,
    owid_raw = owid_raw,    
    pop_raw = pop_raw,
    ifrc_nat_raw = ifrc_nat_raw,
    ifrc_firstaid = ifrc_firstaid,
    ifrc_income = ifrc_income,
    unocha_fund = unocha_fund,
    eurobarometer = eurobarometer,
    hdi = hdi
  ),
  here::here("data_clean", "raw_data.rds")
)