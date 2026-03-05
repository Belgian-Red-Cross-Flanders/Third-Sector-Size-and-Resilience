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
# 4. EM-DAT disasters (floods and wildfires)
emdat_raw_fw <- read.xlsx(here::here("data_raw", "em_dat_floods_wildfires_2015_24.xlsx"))
# 4. EM-DAT natural and technological disasters 
emdat_raw_nt <- read.xlsx(here::here("data_raw", "em_dat_natural_tech_2015_24.xlsx"))
# 6. Population data
pop_raw <- read.xlsx(here::here("data_raw", "world_bank_population_2024.xlsx"))
# 7. IFRC people data
ifrc_nat_raw <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="National Societies")
ifrc_blood <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People donating blood")
ifrc_liveli <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by livelihoods")
ifrc_cash <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by cash transfer")
ifrc_shelter <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by shelter")
ifrc_risk <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by disaster risk") # merge
ifrc_dev <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by long term ser")
ifrc_resp <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached by disaster resp") # merge
ifrc_climate <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People reached with activities ") # merge
ifrc_firstaid <- read.xlsx(here::here("data_raw", "rc_people_2024.xlsx"), sheet="People trained in First Aid")


# Save raw list ready for cleaning
saveRDS(
  list(
    third_raw = third_raw,
    inform_raw = inform_raw,
    ndgain_raw = ndgain_raw,
    emdat_raw_fw = emdat_raw_fw,
    emdat_raw_nt = emdat_raw_nt,
    pop_raw = pop_raw,
    ifrc_nat_raw = ifrc_nat_raw,
    ifrc_blood = ifrc_blood,
    ifrc_liveli = ifrc_liveli,
    ifrc_cash = ifrc_cash,
    ifrc_shelter = ifrc_shelter,
    ifrc_risk = ifrc_risk,
    ifrc_dev = ifrc_dev,
    ifrc_resp = ifrc_resp,
    ifrc_climate = ifrc_climate,
    ifrc_firstaid = ifrc_firstaid
  ),
  here::here("data_clean", "raw_data.rds")
)
