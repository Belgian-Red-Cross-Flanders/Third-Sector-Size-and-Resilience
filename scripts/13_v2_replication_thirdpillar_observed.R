###################################################################################################################################
###################################################################################################################################
### GETTING STARTED ###
###################################################################################################################################
###################################################################################################################################

# Open Read Me "Third Pillar_Read Me.docx"

# Clear Global Environment
rm(list= ls())

# Install Packages (remove # if need to install packages) 
#install.packages("readxl")
#install.packages("xlsx")
#install.packages("writexl")
#install.packages("haven")
#install.packages("wbstats")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("r2weight")
#install.packages("devtools")
#install_github("swager/randomForestCI")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("rnaturalearth")
# 
# # Load Libraries
# library(readxl)
# library(xlsx)
# library(writexl)
# library(haven)
# library(wbstats)
# library(dplyr)
# library(tidyr)
# library(lmtest)
# library(robustbase)
# library(ranger)
# library(vtable)
# library(SuperLearner)
# library("r2weight")
# library(devtools) 
# library(randomForestCI)
# library(randomForest) 
# library(ggplot2)
# library(ggrepel)
# library(gridExtra)
# library(grid)
# library(sf)
# library(rnaturalearth)
# library(tibble)
# library(gridExtra)

# Set working directory to folder where you have "Input" and "Output" folders

###################################################################################################################################
###################################################################################################################################
### IMPORT AND CLEAN DATA ###
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
### POLITY DATA ###
###################################################################################################################################

# Import Polity data
# Source: https://www.systemicpeace.org/inscrdata.html - Polity5 Annual Time-Series, 1946-2018
# This link downloads the excel file: https://www.systemicpeace.org/inscr/p5v2018.xls (Excel file = "pv2018.xls")
polity5 <- read_excel(here::here("data_raw","p5v2018.xls"), col_names = TRUE)

# Impute United States data from 2016 to 2020 (the continued to have a durable government 
# (i.e., there was no regime change or transition period))
polity5$durable[16802] <- 151
polity5$durable[16803] <- 152
polity5$durable[16804] <- 153
polity5$durable[16805] <- 154
polity5$durable[16806] <- 155

# Select Year 2018 only
polity5$year <- as.numeric(polity5$year)
polity5 <- polity5[polity5$year=="2018", ]
polity5$Country <- polity5$country
polity5$countrycode <- polity5$scode


###################################################################################################################################
# Continue Polity Data Analysis
###################################################################################################################################

# Select country (country name), country code, fragment (polity fragmentation),democ (institutionalized democracy), 
#polity (combined polity score), polity 2 (reversed combined polity score), durable (regime durability), polcomp (political competition) 
# See detailed variable explanations here: https://www.systemicpeace.org/inscr/p5manualv2018.pdf
polity5 <- polity5[, c("Country", "countrycode", "polity", "polity2","democ", "fragment", "durable", "polcomp")]

###################################################################################################################################
### WORLD DEVELOPMENT INDICATORS ###
###################################################################################################################################

# Databases can be pulled automatically from the World Bank DataBank to get the latest data (use codes): https://databank.worldbank.org/metadataglossary/all/series

# The following steps are for the database pulled automatically from the World Bank DataBank

# Specify the indicators and countries of interest
#indicators <- c("NY.GDP.MKTP.CD", "NY.GNS.ICTR.ZS", "NY.GNP.MKTP.KD", "NE.CON.GOVT.KD", "NY.GDP.PCAP.KD")
#countries <- "all"

# Retrieve data from the World Bank
#gdp_data <- wb_data(indicator = indicators[1], country = countries)
#gross_saving_data <- wb_data(indicator = indicators[2], country = countries)
#gni_data <- wb_data(indicator = indicators[3], country = countries)
#gov_expenditure_data <- wb_data(indicator = indicators[4], country = countries)
#gdp_per_capita_data <- wb_data(indicator = indicators[5], country = countries)

# Select relevant variables
#gdp_data <- gdp_data[, c("iso3c", "country", "date", "NY.GDP.MKTP.CD")]
#gross_saving_data <- gross_saving_data[, c("iso3c", "country","date", "NY.GNS.ICTR.ZS")]
#gni_data <- gni_data[, c("iso3c","country", "date", "NY.GNP.MKTP.KD")]
#gov_expenditure_data <- gov_expenditure_data[, c("iso3c","country", "date", "NE.CON.GOVT.KD")]
#gdp_per_capita_data <- gdp_per_capita_data[, c("iso3c","country", "date", "NY.GDP.PCAP.KD")]

# Rename columns
#colnames(gdp_data) <- c("countrycode", "Country", "year", "gdp")
#colnames(gross_saving_data) <- c("countrycode","Country", "year", "grossSaving")
#colnames(gni_data) <- c("countrycode","Country", "year", "gni")
#colnames(gov_expenditure_data) <- c("countrycode","Country", "year", "govExpenditure")
#colnames(gdp_per_capita_data) <- c("countrycode","Country", "year", "gdpPerCapita")

# Merge datasets
# Merge the datasets
#wbdata <- inner_join(gdp_data, gross_saving_data, by = c("countrycode","Country", "year"))
#wbdata <- inner_join(wbdata, gni_data, by = c("countrycode","Country", "year")) 
#wbdata <- inner_join(wbdata, gov_expenditure_data, by = c("countrycode","Country", "year"))
#wbdata <- inner_join(wbdata, gdp_per_capita_data, by = c("countrycode","Country", "year"))

# Select Year 2021 only
#wbdata$year <- as.numeric(wbdata$year)
#wbdata<- wbdata[wbdata$year=="2021", ]

# The following steps are to replicate the findings in the paper using the gdppercapita input file "gdpPerCapita.xlsx"

# Input gdp per capita data
wbdata <- read_excel(here::here("data_raw","gdpPerCapita.xlsx"))

# Select data from 2021 only
wbdata<- wbdata[wbdata$year=="2021", ]

###################################################################################################################################
# Additional analysis for Annex (using last five years instead of only 2018) (remove # to replicate Table A2)
###################################################################################################################################

# wbdata <- wbdata[wbdata$year %in% 2017:2021, ]
# wbdata <- wbdata %>%
# group_by(countrycode) %>%
# summarize(
# gdpPerCapita = mean(gdpPerCapita, na.rm = TRUE))

###################################################################################################################################
# Continue World Bank Inidiators Analysis
###################################################################################################################################

# Add Country column with matching country names for each countrycode

# Define a list of country code and country name pairs
country_pairs <- list(
  "ABW" = "Aruba",
  "AFE" = "Africa East",
  "AFG" = "Afghanistan",
  "AFW" = "Africa West",
  "AGO" = "Angola",
  "ALB" = "Albania",
  "AND" = "Andorra",
  "ARB" = "Arab World",
  "ARE" = "United Arab Emirates",
  "ARG" = "Argentina",
  "ARM" = "Armenia",
  "ASM" = "American Samoa",
  "ATG" = "Antigua and Barbuda",
  "AUS" = "Australia",
  "AUT" = "Austria",
  "AZE" = "Azerbaijan",
  "BDI" = "Burundi",
  "BEL" = "Belgium",
  "BEN" = "Benin",
  "BFA" = "Burkina Faso",
  "BGD" = "Bangladesh",
  "BGR" = "Bulgaria",
  "BHR" = "Bahrain",
  "BHS" = "The Bahamas",
  "BIH" = "Bosnia and Herzegovina",
  "BLR" = "Belarus",
  "BLZ" = "Belize",
  "BMU" = "Bermuda",
  "BOL" = "Bolivia",
  "BRA" = "Brazil",
  "BRB" = "Barbados",
  "BRN" = "Brunei Darussalam",
  "BTN" = "Bhutan",
  "BWA" = "Botswana",
  "CAF" = "Central African Republic",
  "CAN" = "Canada",
  "CEB" = "Central Europe and the Baltics",
  "CHE" = "Switzerland",
  "CHI" = "Channel Islands",
  "CHL" = "Chile",
  "CHN" = "China",
  "CIV" = "Cote d'Ivoire",
  "CMR" = "Cameroon",
  "COD" = "Congo, Dem. Rep.",
  "COG" = "Congo, Rep.",
  "COL" = "Colombia",
  "COM" = "Comoros",
  "CPV" = "Cabo Verde",
  "CRI" = "Costa Rica",
  "CSS" = "Caribbean small states",
  "CUB" = "Cuba",
  "CUW" = "Curacao",
  "CYM" = "Cayman Islands",
  "CYP" = "Cyprus",
  "CZE" = "Czech Republic",
  "DEU" = "Germany",
  "DJI" = "Djibouti",
  "DMA" = "Dominica",
  "DNK" = "Denmark",
  "DOM" = "Dominican Republic",
  "DZA" = "Algeria",
  "EAP" = "East Asia & Pacific",
  "EAR" = "Early-demographic dividend",
  "EAS" = "East Asia & Pacific (excluding high income)",
  "ECA" = "Europe & Central Asia",
  "ECS" = "Europe & Central Asia (excluding high income)",
  "ECU" = "Ecuador",
  "EGY" = "Egypt, Arab Rep.",
  "EMU" = "Euro area",
  "ERI" = "Eritrea",
  "ESP" = "Spain",
  "EST" = "Estonia",
  "ETH" = "Ethiopia",
  "EUU" = "European Union",
  "FCS" = "Fragile and conflict affected situations",
  "FIN" = "Finland",
  "FJI" = "Fiji",
  "FRA" = "France",
  "FRO" = "Faroe Islands",
  "FSM" = "Micronesia, Fed. Sts.",
  "GAB" = "Gabon",
  "GBR" = "United Kingdom",
  "GEO" = "Georgia",
  "GHA" = "Ghana",
  "GIB" = "Gibraltar",
  "GIN" = "Guinea",
  "GMB" = "Gambia",
  "GNB" = "Guinea-Bissau",
  "GNQ" = "Equatorial Guinea",
  "GRC" = "Greece",
  "GRD" = "Grenada",
  "GRL" = "Greenland",
  "GTM" = "Guatemala",
  "GUM" = "Guam",
  "GUY" = "Guyana",
  "HIC" = "High income",
  "HKG" = "Hong Kong SAR, China",
  "HND" = "Honduras",
  "HPC" = "Heavily indebted poor countries (HIPC)",
  "HRV" = "Croatia",
  "HTI" = "Haiti",
  "HUN" = "Hungary",
  "IBD" = "IBRD",
  "IBT" = "IBRD only",
  "IDA" = "IDA",
  "IDB" = "IDA & IBRD total",
  "IDN" = "Indonesia",
  "IDX" = "IDA blend",
  "IMN" = "Isle of Man",
  "IND" = "India",
  "INX" = "Not classified",
  "IRL" = "Ireland",
  "IRN" = "Iran, Islamic Rep.",
  "IRQ" = "Iraq",
  "ISL" = "Iceland",
  "ISR" = "Israel",
  "ITA" = "Italy",
  "JAM" = "Jamaica",
  "JOR" = "Jordan",
  "JPN" = "Japan",
  "KAZ" = "Kazakhstan",
  "KEN" = "Kenya",
  "KGZ" = "Kyrgyz Republic",
  "KHM" = "Cambodia",
  "KIR" = "Kiribati",
  "KNA" = "St. Kitts and Nevis",
  "KOR" = "Korea, Rep.",
  "KWT" = "Kuwait",
  "LAC" = "Latin America & the Caribbean (excluding high income)",
  "LAO" = "Lao PDR",
  "LBN" = "Lebanon",
  "LBR" = "Liberia",
  "LBY" = "Libya",
  "LCA" = "St. Lucia",
  "LCN" = "Latin America & Caribbean",
  "LDC" = "Least developed countries: UN classification",
  "LIC" = "Low income",
  "LIE" = "Liechtenstein",
  "LKA" = "Sri Lanka",
  "LMC" = "Lower middle income",
  "LMY" = "Low & middle income",
  "LSO" = "Lesotho",
  "LTE" = "Late-demographic dividend",
  "LTU" = "Lithuania",
  "LUX" = "Luxembourg",
  "LVA" = "Latvia",
  "MAC" = "Macao SAR, China",
  "MAF" = "St. Martin (French part)",
  "MAR" = "Morocco",
  "MCO" = "Monaco",
  "MDA" = "Moldova",
  "MDG" = "Madagascar",
  "MDV" = "Maldives",
  "MEA" = "Middle East & North Africa",
  "MEX" = "Mexico",
  "MHL" = "Marshall Islands",
  "MIC" = "Middle income",
  "MKD" = "North Macedonia",
  "MLI" = "Mali",
  "MLT" = "Malta",
  "MMR" = "Myanmar",
  "MNA" = "Middle East & North Africa (excluding high income)",
  "MNE" = "Montenegro",
  "MNG" = "Mongolia",
  "MNP" = "Northern Mariana Islands",
  "MOZ" = "Mozambique",
  "MRT" = "Mauritania",
  "MUS" = "Mauritius",
  "MWI" = "Malawi",
  "MYS" = "Malaysia",
  "NAC" = "North America",
  "NAM" = "Namibia",
  "NCL" = "New Caledonia",
  "NER" = "Niger",
  "NGA" = "Nigeria",
  "NIC" = "Nicaragua",
  "NLD" = "Netherlands",
  "NOR" = "Norway",
  "NPL" = "Nepal",
  "NRU" = "Nauru",
  "NZL" = "New Zealand",
  "OED" = "OECD members",
  "OMN" = "Oman",
  "OSS" = "Other small states",
  "PAK" = "Pakistan",
  "PAN" = "Panama",
  "PER" = "Peru",
  "PHL" = "Philippines",
  "PLW" = "Palau",
  "PNG" = "Papua New Guinea",
  "POL" = "Poland",
  "PRE" = "Pre-demographic dividend",
  "PRI" = "Puerto Rico",
  "PRK" = "Korea, Dem. People's Rep.",
  "PRT" = "Portugal",
  "PRY" = "Paraguay",
  "PSE" = "West Bank and Gaza",
  "PSS" = "Pacific island small states",
  "PST" = "Post-demographic dividend",
  "PYF" = "French Polynesia",
  "QAT" = "Qatar",
  "ROU" = "Romania",
  "RUS" = "Russian Federation",
  "RWA" = "Rwanda",
  "SAS" = "South Asia",
  "SAU" = "Saudi Arabia",
  "SDN" = "Sudan",
  "SEN" = "Senegal",
  "SGP" = "Singapore",
  "SLB" = "Solomon Islands",
  "SLE" = "Sierra Leone",
  "SLV" = "El Salvador",
  "SMR" = "San Marino",
  "SOM" = "Somalia",
  "SRB" = "Serbia",
  "SSA" = "Sub-Saharan Africa",
  "SSD" = "South Sudan",
  "SSF" = "Sub-Saharan Africa (excluding high income)",
  "SST" = "Small states",
  "STP" = "Sao Tome and Principe",
  "SUR" = "Suriname",
  "SVK" = "Slovak Republic",
  "SVN" = "Slovenia",
  "SWE" = "Sweden",
  "SWZ" = "Eswatini",
  "SXM" = "Sint Maarten (Dutch part)",
  "SYC" = "Seychelles",
  "SYR" = "Syrian Arab Republic",
  "TCA" = "Turks and Caicos Islands",
  "TCD" = "Chad",
  "TEA" = "East Asia & Pacific (IDA & IBRD countries)",
  "TEC" = "Europe & Central Asia (IDA & IBRD countries)",
  "TGO" = "Togo",
  "THA" = "Thailand",
  "TJK" = "Tajikistan",
  "TKM" = "Turkmenistan",
  "TLA" = "Latin America & the Caribbean (IDA & IBRD countries)",
  "TLS" = "Timor-Leste",
  "TMN" = "Middle East & North Africa (IDA & IBRD countries)",
  "TON" = "Tonga",
  "TSA" = "South Asia (IDA & IBRD)",
  "TSS" = "Sub-Saharan Africa (IDA & IBRD countries)",
  "TTO" = "Trinidad and Tobago",
  "TUN" = "Tunisia",
  "TUR" = "Turkey",
  "TUV" = "Tuvalu",
  "TZA" = "Tanzania",
  "UGA" = "Uganda",
  "UKR" = "Ukraine",
  "UMC" = "Upper middle income",
  "URY" = "Uruguay",
  "USA" = "United States",
  "UZB" = "Uzbekistan",
  "VCT" = "St. Vincent and the Grenadines",
  "VEN" = "Venezuela",
  "VGB" = "British Virgin Islands", 
  "VIR" = "U.S. Virgin Islands",
  "VNM" = "Vietnam", 
  "VUT" = "Vanuatu", 
  "WLD" = "World",
  "WSM" = "Samoa",
  "XKX" = "Kosovo",
  "YEM" = "Yemen",
  "ZAF" = "South Africa",
  "ZMB" = "Zambia",
  "ZWE" = "Zimbabwe")

wbdata$Country <- country_pairs[wbdata$countrycode]

wbdata$Country <- as.character(wbdata$Country)

###################################################################################################################################
### INTERNATIONAL LABOUR MARKET DATA ###
###################################################################################################################################

### EMPLOYMENT SECTORS ###

# Import Employment Data 
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "EMP_TEMP_SEX_ECO_NB_A_EN.xlsx")
# Source: ILO STAT Data, "Employment by sex and economic activity (thousands)" 
# ILO Code: EMP_TEMP_SEX_ECO_NB_A_EN
# Link to updated dataset: https://www.ilo.org/ilostat-files/Documents/Excel/Indicator/EMP_TEMP_SEX_ECO_NB_A_EN.xlsx

emp <- read_excel(here::here("data_raw","EMP_TEMP_SEX_ECO_NB_A_EN.xlsx"))

# Add correct/shortened column names 
colnames(emp) <- emp[5,]
colnames(emp)[10] <- "Mining and quarrying"
colnames(emp)[11] <- "Trade"
colnames(emp)[12] <- "Public Administration"
colnames(emp)[1] <- "Country"
colnames(emp)[14] <- "Notes"

# Remove first 5 rows
emp <- emp[-(1:5),]

# Make columns numeric
emp[, 4:ncol(emp)] <- lapply(emp[, 4:ncol(emp)], as.numeric)

# Agriculture Data - AG

# Create new dataframe

ag <- emp[c("Country", "Time", "Sex", "Agriculture")]

# Select years (2017-2021) and sex (Total)
ag <- ag %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
ag <- ag %>%
  pivot_wider(names_from = Time, values_from = Agriculture) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
ag$Agriculture_abs <- rowMeans(ag[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Manufacturing Data - MAN

# Create new dataframe

man <- emp[c("Country", "Time", "Sex", "Manufacturing")]

# Select years (2017-2021) and sex (Total)
man <- man %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
man <- man %>%
  pivot_wider(names_from = Time, values_from = Manufacturing) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
man$Manufacturing_abs <- rowMeans(man[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Construction Data - CON

# Create new dataframe

con <- emp[c("Country", "Time", "Sex", "Construction")]

# Select years (2017-2021) and sex (Total)
con <- con %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
con <- con %>%
  pivot_wider(names_from = Time, values_from = Construction) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
con$Construction_abs <- rowMeans(con[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Trade Data - TRADE

# Create new dataframe

trade <- emp[c("Country", "Time", "Sex", "Trade")]

# Select years (2017-2021) and sex (Total)
trade <- trade %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
trade <- trade %>%
  pivot_wider(names_from = Time, values_from = Trade) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
trade$Trade_abs <- rowMeans(trade[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Public Administration Data - PUB

# Create new dataframe

pub <- emp[c("Country", "Time", "Sex", "Public Administration")]

# Select years (2017-2021) and sex (Total)
pub <- pub %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
pub <- pub %>%
  pivot_wider(names_from = Time, values_from = `Public Administration`) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
pub$Public_abs <- rowMeans(pub[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Other - OTHER (Mining and Quarrying & Not classified)

# Create new dataframe

other <- emp[c("Country", "Time", "Sex", "Mining and quarrying", "Not classified")]

# Create variable other 
other$Other <- rowSums(other[, c("Mining and quarrying", "Not classified")], na.rm = TRUE)

# Select years (2017-2021) and sex (Total) and keep new Other variable only 
other <- other %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total") %>%
  dplyr::select(-c("Mining and quarrying", "Not classified"))

# Transform dataframe so years become the columns 
other <- other %>%
  pivot_wider(names_from = Time, values_from = `Other`) %>%
  dplyr::select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
other$Other_abs <- rowMeans(other[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

### ALL EMPLOYMENT SECTORS TOGETHER ###

# Add all dataframes together into one
sectors <- full_join(ag, man, by = "Country") %>%
  full_join(con, by = "Country") %>%
  full_join(trade, by = "Country") %>%
  full_join(pub, by = "Country") %>%
  full_join(other, by = "Country")
sectors <- sectors[c("Country", "Agriculture_abs", "Manufacturing_abs", 
                     "Construction_abs", "Trade_abs", "Public_abs", "Other_abs")]

# Create Total variable 
sectors$Total <- rowSums(sectors[, c("Agriculture_abs", "Manufacturing_abs", 
                                     "Construction_abs", "Trade_abs", "Public_abs", "Other_abs")], na.rm = TRUE)

# Create percentages
sectors$AG <- (sectors$Agriculture_abs / sectors$Total) * 100
sectors$MAN <- (sectors$Manufacturing_abs / sectors$Total) * 100
sectors$CON <- (sectors$Construction_abs / sectors$Total) * 100
sectors$TRADE <- (sectors$Trade_abs / sectors$Total) * 100
sectors$PUB <- (sectors$Public_abs / sectors$Total) * 100
sectors$OTHER <- (sectors$Other_abs / sectors$Total) * 100

# Create dataframe to add to full database
sectors_database <- sectors %>%
  dplyr::select("Country", "AG", "MAN", "CON", "TRADE", "PUB", "OTHER")

###################################################################################################################################
### POPULATION DATA ###
###################################################################################################################################

# Import Population Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "population.xlsx")
# Source: ILO STAT Data, "Working-age population by sex and age (thousands) - Annual"
# ILO Code: "POP_XWAP_SEX_AGE_NB_A"
# Link to updated dataset:https://ilostat.ilo.org/topics/population-and-labour-force/ 

pop <- read_excel(here::here("data_raw","population.xlsx"))

# Add correct column names
colnames(pop) <- pop[5,]
pop <- pop %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
pop <- pop[-(1:5),]

# Make columns numeric
columns_to_convert_pop <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                            "45-49", "50-54", "55-59", "60-64")
for (column in columns_to_convert_pop) {
  pop[[column]] <- as.numeric(pop[[column]])
}

# Create 15-64 column
pop$'15-64' <- rowSums(pop[,c("15-19", "20-24","25-29","30-34", "35-39", "40-44",
                              "45-49", "50-54","55-59", "60-64")])
# Keep Sex = Total 
SexFilterPop <- "Total"
pop <- subset(pop, Sex == SexFilterPop)

# Keep years 2019, 2020, and 2021
YearFilterPop <- c("2019", "2020", "2021")
pop <- subset(pop, Time %in% YearFilterPop)

# Remove all unnecessary columns 
columns_to_remove_pop <- c("Source", "Sex", "Total", "0-4", "5-9", "10-14", "15-19",
                           "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                           "50-54", "55-59", "60-64", "65+")
pop <- pop %>% dplyr::select(-one_of(columns_to_remove_pop))

# Transform dataframe so Time becomes the columns
pop$Time <- as.numeric(pop$Time)
pop <- pop %>%
  pivot_wider(names_from = Time, values_from = `15-64`)

# Add correct row names
pop <- as.data.frame(pop) 
rownames(pop) <- pop[, 1]

# Create mean population value across 2019,2020, and 2021
pop$population <- rowMeans(pop[, c("2019", "2020", "2021")], na.rm = TRUE)

# Create final dataframe with only country and mean population 
population <- pop[c("Country", "population")]

###################################################################################################################################
### PARTICIPATION DATA ###
###################################################################################################################################

# Import Participation Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "EAP_DWAP_SEX_AGE_RT_A_EN.xlsx")
# Source: ILO STAT Data, "Labour force participation rate by sex and age (%)"
# ILO Code: EAP_DWAP_SEX_AGE_RT_A_EN
# Link to updated dataset: https://ilostat.ilo.org/topics/population-and-labour-force/ 

part <- read_excel(here::here("data_raw","EAP_DWAP_SEX_AGE_RT_A_EN.xlsx"))

# Add correct column names
colnames(part) <- part[5,]
colnames(part)[9] <- "Notes"
part <- part %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
part <- part[-(1:5),]

# Make 15-64 column numeric
part$`15-64` <- as.numeric(part$`15-64`)

# Keep Sex = Total 
SexFilterPart <- "Total"
part <- subset(part, Sex == SexFilterPart)

# Keep years 2017-2021
part$Time <- as.numeric(part$Time)
YearFilterPart <- c("2017", "2018", "2019", "2020", "2021")
part <- subset(part, Time %in% YearFilterPart)

# Remove all unnecessary columns 
columns_to_remove_part <- c("Source", "Sex", "15+", "15-24", "25+", "Notes")
part <- part %>% dplyr::select(-one_of(columns_to_remove_part))

# Transform dataframe so Time becomes the columns
part <- part %>%
  pivot_wider(names_from = Time, values_from = `15-64`) %>%
  dplyr::select("Country", "2017", "2018", "2019", "2020", "2021")

# Add correct row names
part <- as.data.frame(part) 
rownames(part) <- part[, 1]

# Create mean population value across 2019,2020, and 2021
part$participation <- rowMeans(part[, c("2017", "2018", "2019", "2020", "2021")], na.rm = TRUE)

# Create final dataframe with only country and mean population 
participation <- part[c("Country", "participation")]

###################################################################################################################################
### UNEMPLOYMENT DATA ###
###################################################################################################################################

# Import Participation Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "SDG_0852_SEX_AGE_RT_A_EN.xlsx")
# Source: ILO STAT Data, "Unemployment Rate (%)"
# ILO Code: SDG_0852_SEX_AGE_RT_A
# Link to download updated dataset: https://www.ilo.org/ilostat-files/Documents/Excel/Indicator/SDG_0852_SEX_AGE_RT_A_EN.xlsx

unemp <- read_excel(here::here("data_raw","SDG_0852_SEX_AGE_RT_A_EN.xlsx"))

# Add correct column names
colnames(unemp) <- unemp[5,]
colnames(unemp)[8] <- "Notes"
unemp <- unemp %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
unemp <- unemp[-(1:5),]

# Make 15-64 column numeric
unemp$`15+` <- as.numeric(unemp$`15+`)

# Keep Sex = Total 
SexFilterUnemp <- "Total"
unemp <- subset(unemp, Sex %in% SexFilterUnemp)

# Keep years 2017-2021
unemp$Time <- as.numeric(unemp$Time)
YearFilterUnemp <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", 
                     "2019", "2020", "2021")
unemp <- subset(unemp, Time %in% YearFilterUnemp)

# Remove all unnecessary columns 
columns_to_remove_unemp <- c("Source", "Sex", "15-24", "25+", "Notes")
unemp <- unemp %>% dplyr::select(-one_of(columns_to_remove_unemp))

# Transform dataframe so Time becomes the columns
unemp <- unemp %>%
  pivot_wider(names_from = Time, values_from = `15+`) %>%
  dplyr::select("Country", "2012", "2013", "2014", "2015", "2016", "2017", 
         "2018", "2019", "2020", "2021")

# Add correct row names
unemp <- as.data.frame(unemp) 
rownames(unemp) <- unemp[, 1]

# Create mean population value across 2012 - 2021
unemp$unemployment <- rowMeans(unemp[, c("2012", "2013", "2014", "2015", 
                                         "2016", "2017", "2018", "2019", "2020",
                                         "2021")], na.rm = TRUE)

###################################################################################################################################
### TE/EAP (Total Employment/Economically Active Population) RATIO ###
###################################################################################################################################

# Merge part, pop, and unemp dataframes together
labour <- pop %>%
  left_join(part, by = "Country") %>%
  left_join(unemp, by = "Country")

# Select reference area, mean population (2019-2021),mean participation (2017-2021), mean unemployment (2012-2021)
labour <- labour %>%
  dplyr::select("Country", "population", "participation", "unemployment")

# Calculate EAP 
labour$EAP <- ifelse(is.na(labour$participation) | is.na(labour$population), NA, 
                     (labour$participation/100) * labour$population)

# Calculate TE
labour$TE <- ifelse(is.na(labour$participation) | is.na(labour$population), NA, 
                    ((100-labour$unemployment)/100) * labour$EAP)

# Calculate TE/EAP
labour$TE.EAP.ratio <- ifelse(is.na(labour$TE) | is.na(labour$EAP), NA, 
                              (labour$TE / labour$EAP))

###################################################################################################################################
### THIRD PILLAR SIZE INTERNATIONAL ###
###################################################################################################################################

# Import JHU INTL Data
# SOURCE: Lester M. Salamon, S. Wojciech Sokolowski, Megan A. Haddock, and Associates, Explaining Civil Society Development: A Social Origins Approach, (Baltimore: Johns Hopkins University Press, 2017).
# PDF Available in Folder "JHU_2017_Civil Society Studies Comparative Data Tables"
# Manually copied into Excel

jhuintl <- read_excel(here::here("data_raw", "jhuintl.xlsx"))

# Add correct column names
colnames(jhuintl) <- jhuintl[4,]

# Remove first 4 rows
jhuintl <- jhuintl[-(1:4),]

# Add correct row names 
jhuintl <- as.data.frame(jhuintl)
rownames(jhuintl) <- jhuintl[,1]

# Make columns numeric
jhuintl <- jhuintl %>%
  mutate(
    Year = as.numeric(Year),
    `Paid workers` = as.numeric(`Paid workers`),
    `JHU-Volunteers` = as.numeric(`JHU-Volunteers`),
    `JHU-Total` = as.numeric(`JHU-Total`)
  )

# Multiply values by 100 
jhuintl <- jhuintl %>%
  mutate(
    `Paid workers` = `Paid workers` * 100,
    `JHU-Volunteers` = `JHU-Volunteers` * 100,
    `JHU-Total` = `JHU-Total` * 100
  )

# Add TE.EAP.ratio column from labour dataframe to jhuintl dataframe 
jhuintl <- jhuintl %>%
  left_join(labour, by = "Country")
jhuintl <- jhuintl %>%
  dplyr::select("Country", "Year", "Paid workers", "JHU-Volunteers", "JHU-Total", "TE.EAP.ratio")

# Calculate corrected third pillar size for paid workers, JHU-volunteers, JHU-Total using TE.EAP.ratio
jhuintl <- jhuintl %>%
  mutate(Plural_intl = `JHU-Total` / `TE.EAP.ratio`)
jhuintl <- jhuintl %>%
  mutate(`Plural - paid_intl` = `Paid workers` / `TE.EAP.ratio`)
jhuintl <- jhuintl %>%
  mutate(`Plural - volunteer_intl` = `JHU-Volunteers` / `TE.EAP.ratio`)

###################################################################################################################################
### THIRD PILLAR SIZE EUROPEAN ###
###################################################################################################################################

# Import JHU EU Dataset
# SOURCE: The size and composition of the european third sector. Salamon, LM, Sokolowski W. 2018. In "The third sector as a renewable resource for Europe" by Enjolras et al. 
# Manually copied into Excel 

jhueu <- read_excel(here::here("data_raw", "jhueu.xlsx"))


# Remove last few rows (i.e., notes from the table)
jhueu <- jhueu[-c(37:44), ]

# Remove columns without percentage values 
jhueu <- jhueu[, -c(2:10)]

# Add correct column names
colnames(jhueu) <- c(
  "1" = "Country",
  "2" = "NPI paid",
  "3" = "NPI volunteer",
  "4" = "NPI total",
  "5" = "Coops paid",
  "6" = "Coops volunteers",
  "7" = "Coops total",
  "8" = "Social ent",
  "9" = "Direct vol",
  "10" = "Total TSE"
)

# Remove first 2 rows
jhueu <- jhueu[-(1:2),]

# Make columns numeric
jhueu[, -1] <- lapply(jhueu[, -1], as.numeric)

# Add correct row names
jhueu <- as.data.frame(jhueu)
rownames(jhueu) <- jhueu[,1]

# Calculate third pillar size for Plural, Plural-paid, and Plural-volunteer
jhueu <- jhueu %>%
  mutate(Plural_eu = `Total TSE` - `Direct vol`)
jhueu <- jhueu %>%
  mutate(`Plural - paid_eu` = coalesce(`NPI paid`, 0) + coalesce(`Coops paid`, 0) + coalesce(`Social ent`, 0))
jhueu <- jhueu %>%
  mutate(`Plural - volunteer_eu` = coalesce(`NPI volunteer`, 0) + coalesce(`Coops volunteers`, 0))

###################################################################################################################################
### THIRD PILLAR SIZE COMBINED (EUROPEAN PRIORITIZED) ###
###################################################################################################################################

# Combine jhueu and jhuintl dataframes into a new dataframe 
thirdpillar <- full_join(jhueu, jhuintl, by = "Country")

# Keep only summarized variables (Plural, Plural-paid, Plural-volunteer) from jhueu and jhuintl
thirdpillar <- thirdpillar %>%
  dplyr::select(`Country`, 
         `Plural_intl`, `Plural - paid_intl`, `Plural - volunteer_intl`, 
         `Plural_eu`, `Plural - paid_eu`, `Plural - volunteer_eu`)

# Finalize third pillar size for each country 
thirdpillar$Plural <- ifelse(is.na(thirdpillar$Plural_eu), thirdpillar$Plural_intl, thirdpillar$Plural_eu)
thirdpillar$`Plural - paid` <- ifelse(is.na(thirdpillar$`Plural - paid_eu`), thirdpillar$`Plural - paid_intl`, thirdpillar$`Plural - paid_eu`)
thirdpillar$`Plural - volunteer` <- ifelse(is.na(thirdpillar$`Plural - volunteer_eu`), thirdpillar$`Plural - volunteer_intl`, thirdpillar$`Plural - volunteer_eu`)

# Re-order so final third pillar size comes first 
thirdpillar <- thirdpillar %>%
  dplyr::select(`Country`,
         `Plural`, `Plural - paid`, `Plural - volunteer`,
         `Plural_intl`, `Plural - paid_intl`, `Plural - volunteer_intl`, 
         `Plural_eu`, `Plural - paid_eu`, `Plural - volunteer_eu`)

readr::write_csv(
  thirdpillar,
  here::here(
    "data_clean",
    "thirdpillar_replication.csv"
  )
)

# Merge population dataframe, participation dataframe, sectors_database dataframe and thirdpillar dataframe 
database <- full_join(population, participation, by = join_by(Country))
database <- full_join(database, sectors_database, by = join_by(Country))
database <- full_join(database, thirdpillar, by = join_by(Country))


all_countries <- database

# Fix country names and codes to match polity5 data
# Be careful about these country names and codes. 
# It is possible that more countries will be added to the list since this script was written. 
# You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.
all_countries$Country[all_countries$Country == "Bosnia and Herzegovina"] <- "Bosnia"
all_countries$Country[all_countries$Country == "Czechia"] <- "Czech Republic"
all_countries$Country[all_countries$Country == "Côte d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Iran, Islamic Republic of"] <- "Iran"
all_countries$Country[all_countries$Country == "Korea, Republic of"] <- "Korea"
all_countries$Country[all_countries$Country == "Lao People's Democratic Republic"] <- "Laos"
all_countries$Country[all_countries$Country == "Moldova, Republic of"] <- "Moldova"
all_countries$Country[all_countries$Country == "Myanmar"] <- "Myanmar (Burma)"
all_countries$Country[all_countries$Country == "North Macedonia"] <- "Macedonia"
all_countries$Country[all_countries$Country == "Russian Federation"] <- "Russia"
all_countries$Country[all_countries$Country == "Slovakia"] <- "Slovak Republic"
all_countries$Country[all_countries$Country == "Taiwan, China"] <- "Taiwan"
all_countries$Country[all_countries$Country == "United Arab Emirates"] <- "UAE"
all_countries$Country[all_countries$Country == "Venezuela, Bolivarian Republic of"] <- "Venezuela"
all_countries$Country[all_countries$Country == "Viet Nam"] <- "Vietnam"

# Create new dataframe 
countryLevelWbPolity <- polity5

# Merge all_countries with polity 5
all_countries = full_join(all_countries, countryLevelWbPolity, by = join_by(Country))
all_countries <- all_countries %>%
  dplyr::select(Country, countrycode, everything())

# Add correct country codes to all_countries after adding polity data
# Be careful about these country names and codes. 
# It is possible that more countries will be added to the list since this script was written. 
# You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.
all_countries$countrycode[all_countries$Country == "Korea"] <- "KOR"
all_countries$countrycode[all_countries$Country == "Tonga"] <- "TON"
all_countries$countrycode[all_countries$Country == "Iceland"] <- "ISL"
all_countries$countrycode[all_countries$Country == "Belize"] <- "BLZ"
all_countries$countrycode[all_countries$Country == "Barbados"] <- "BRB"
all_countries$countrycode[all_countries$Country == "Maldives"] <- "MDV"
all_countries$countrycode[all_countries$Country == "Saint Lucia"] <- "LCA"
all_countries$countrycode[all_countries$Country == "Samoa"] <- "WSM"
all_countries$countrycode[all_countries$Country == "Vanuatu"] <- "VUT"
all_countries$countrycode[all_countries$Country == "New Caledonia"] <- "NCL"
all_countries$countrycode[all_countries$Country == "Brunei Darussalam"] <- "BRN"
all_countries$countrycode[all_countries$Country == "Switzerland"] <- "CHE"

all_countries <- all_countries %>%
  mutate(countrycode = ifelse(Country == "Algeria", "DZA", countrycode),
         countrycode = ifelse(Country == "Australia", "AUS", countrycode),
         countrycode = ifelse(Country == "Brukina Faso", "BFA", countrycode),
         countrycode = ifelse(Country == "Bangladesh", "BGD", countrycode),
         countrycode = ifelse(Country == "Bosnia and Herzegovina", "BIH", countrycode),
         countrycode = ifelse(Country == "Bosnia", "BIH", countrycode),
         countrycode = ifelse(Country == "Botswana", "BWA", countrycode),
         countrycode = ifelse(Country == "Bulgaria", "BGR", countrycode),
         countrycode = ifelse(Country == "Cambodia", "KHM", countrycode),
         countrycode = ifelse(Country == "Cape Verde", "CPV", countrycode),
         countrycode = ifelse(Country == "Chad", "TCD", countrycode),
         countrycode = ifelse(Country == "Costa Rica", "CRI", countrycode),
         countrycode = ifelse(Country == "Croatia", "HRV", countrycode),
         countrycode = ifelse(Country == "Czechia", "CZE", countrycode),
         countrycode = ifelse(Country == "Denmark", "DNK", countrycode),
         countrycode = ifelse(Country == "France", "FRA", countrycode),
         countrycode = ifelse(Country == "Gambia", "GMB", countrycode),
         countrycode = ifelse(Country == "Georgia", "GEO", countrycode),
         countrycode = ifelse(Country == "Germany", "DEU", countrycode),         
         countrycode = ifelse(Country == "Guatemala", "GTM", countrycode),
         countrycode = ifelse(Country == "Honduras", "HND", countrycode),         
         countrycode = ifelse(Country == "Indonesia", "IDN", countrycode),         
         countrycode = ifelse(Country == "Ireland", "IRL", countrycode),         
         countrycode = ifelse(Country == "Cote d'Ivoire", "CIV", countrycode),         
         countrycode = ifelse(Country == "Kyrgyzstan", "KGZ", countrycode),         
         countrycode = ifelse(Country == "Latvia", "LVA", countrycode),         
         countrycode = ifelse(Country == "Lebanon", "LBN", countrycode),         
         countrycode = ifelse(Country == "Lesotho", "LSO", countrycode),         
         countrycode = ifelse(Country == "Lithuania", "LTU", countrycode),         
         countrycode = ifelse(Country == "Mauritania", "MRT", countrycode),         
         countrycode = ifelse(Country == "Moldova, Republic of", "MDA", countrycode),         
         countrycode = ifelse(Country == "Montenegro", "MNE", countrycode),         
         countrycode = ifelse(Country == "Mongolia", "MNG", countrycode),         
         countrycode = ifelse(Country == "Myanmar", "MMR", countrycode),         
         countrycode = ifelse(Country == "Nepal", "NPL", countrycode),
         countrycode = ifelse(Country == "New Zealand", "NZL", countrycode), 
         countrycode = ifelse(Country == "Nigeria", "NGA", countrycode), 
         countrycode = ifelse(Country == "Niger", "NER", countrycode), 
         countrycode = ifelse(Country == "Netherlands", "NLD", countrycode), 
         countrycode = ifelse(Country == "Oman", "OMN", countrycode), 
         countrycode = ifelse(Country == "Paraguay", "PRY", countrycode), 
         countrycode = ifelse(Country == "Philippines", "PHL", countrycode), 
         countrycode = ifelse(Country == "Portugal", "PRT", countrycode), 
         countrycode = ifelse(Country == "Romania", "ROU", countrycode), 
         countrycode = ifelse(Country == "South Africa", "ZAF", countrycode), 
         countrycode = ifelse(Country == "El Salvador", "SLV", countrycode), 
         countrycode = ifelse(Country == "Serbia", "SRB", countrycode), 
         countrycode = ifelse(Country == "Sierra Leone", "SLE", countrycode), 
         countrycode = ifelse(Country == "Slovakia", "SVK", countrycode), 
         countrycode = ifelse(Country == "Spain", "ESP", countrycode), 
         countrycode = ifelse(Country == "Sri Lanka", "LKA", countrycode), 
         countrycode = ifelse(Country == "Sweden", "SWE", countrycode), 
         countrycode = ifelse(Country == "Thailand", "THA", countrycode), 
         countrycode = ifelse(Country == "Togo", "TGO", countrycode), 
         countrycode = ifelse(Country == "United Arab Emirates", "ARE", countrycode), 
         countrycode = ifelse(Country == "United Kingdom", "GBR", countrycode), 
         countrycode = ifelse(Country == "Uruguay", "URY", countrycode),
         countrycode = ifelse(Country == "Venezuela, Bolivarian Republic of", "VEN", countrycode), 
         countrycode = ifelse(Country == "Viet Nam", "VNM", countrycode), 
         countrycode = ifelse(Country == "Zambia", "ZMB", countrycode), 
         countrycode = ifelse(Country == "Zimbabwe", "ZWE", countrycode), 
         countrycode = ifelse(Country == "Austria", "AUT", countrycode), 
         countrycode = ifelse(Country == "Slovenia", "SVN", countrycode), 
         countrycode = ifelse(Country == "Switzerland", "CHE", countrycode)
  )

# Merge all_countries with wbdata
all_countries = full_join(all_countries, wbdata, by = (c("Country", "countrycode")))
all_countries <- all_countries %>%
  dplyr::select(Country, countrycode, everything())

# Change all country names to be match
# Be careful about these country names and codes. 
# It is possible that more countries will be added to the list since this script was written. 
# You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.

all_countries$Country[all_countries$Country == "Bosnia and Herzegovina"] <- "Bosnia"
all_countries$Country[all_countries$Country == "Czechia"] <- "Czech Republic"
all_countries$Country[all_countries$Country == "Côte d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Cote d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Iran, Islamic Republic of"] <- "Iran"
all_countries$Country[all_countries$Country == "Korea, Republic of"] <- "Korea"
all_countries$Country[all_countries$Country == "Lao People's Democratic Republic"] <- "Laos"
all_countries$Country[all_countries$Country == "Moldova, Republic of"] <- "Moldova"
all_countries$Country[all_countries$Country == "Myanmar"] <- "Myanmar (Burma)"
all_countries$Country[all_countries$Country == "North Macedonia"] <- "Macedonia"
all_countries$Country[all_countries$Country == "Russian Federation"] <- "Russia"
all_countries$Country[all_countries$Country == "Slovakia"] <- "Slovak Republic"
all_countries$Country[all_countries$Country == "Taiwan, China"] <- "Taiwan"
all_countries$Country[all_countries$Country == "United Arab Emirates"] <- "UAE"
all_countries$Country[all_countries$Country == "Venezuela, Bolivarian Republic of"] <- "Venezuela"
all_countries$Country[all_countries$Country == "Viet Nam"] <- "Vietnam"
all_countries$Country[all_countries$Country == "Cabo Verde"] <- "Cape Verde"
all_countries$Country[all_countries$Country == "Egypt, Arab Rep."] <- "Egypt"
all_countries$Country[all_countries$Country == "Gambia, The"] <- "Gambia"
all_countries$Country[all_countries$Country == "Iran, Islamic Rep."] <- "Iran"
all_countries$Country[all_countries$Country == "Korea South"] <- "Korea"
all_countries$Country[all_countries$Country == "Korea, Rep."] <- "Korea"
all_countries$Country[all_countries$Country == "Lao PDR"] <- "Laos"
all_countries$Country[all_countries$Country == "Kyrgyz Republic"] <- "Kyrgyzstan"
all_countries$Country[all_countries$Country == "Turkiye"] <- "Turkey"
all_countries$Country[all_countries$Country == "Venezuela, RB"] <- "Venezuela"

# Remove columns that are not needed in analysis and re-order columns
all_countries <- all_countries %>%
  dplyr::select(Country, population, participation, 
         AG, MAN, CON, TRADE, PUB, OTHER, 
         Plural, `Plural - paid`, `Plural - volunteer`, 
         polity, democ, durable, gdpPerCapita)

# Aggregate data so there is one row per country (instead of >1)
all_countries <- all_countries %>%
  group_by(Country) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup()

# Convert NaN to NA
all_countries <- lapply(all_countries, function(x) ifelse(is.nan(x), NA, x))
all_countries <- as.data.frame(all_countries)

# Make the names of countries a factor
names_all_countries <- all_countries[,1]

# Make Countries the row names 
rownames(all_countries) <- names_all_countries

# Remove Country column
all_countries[,1] <- NULL


write_xlsx(database,here::here("data_clean", "replication_Database_thirdpillar.xlsx"))
