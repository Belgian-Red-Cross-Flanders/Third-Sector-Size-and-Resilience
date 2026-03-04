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



# Save raw list ready for cleaning
saveRDS(
  list(
    third_raw = third_raw,
    inform_raw = inform_raw,
    ndgain_raw = ndgain_raw,
    emdat_raw_fw = emdat_raw_fw,
    emdat_raw_nt = emdat_raw_nt,
    pop_raw = pop_raw
    
  ),
  here::here("data_clean", "raw_data.rds")
)
