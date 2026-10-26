# Third Sector Size and Resilience

This repository contains the data preparation, analysis, and
figure‑generation pipeline for a research project examining the
relationship between third sector capacity and national resilience
outcomes, including readiness, adaptive capacity, coping capacity, and
disaster impacts. The project uses cross‑national data and focuses on
multiple dimensions of third sector (paid employment and volunteers, adding to the total size).

## Project structure

The repository is organized as follows:

```         
.
├── scripts/
│   ├── 01_load_packages.R
│   ├── 02_functions.R
│   ├── 03_import_data.R
│   ├── 04_clean_merge.R
│   ├── 05_descriptives.R
│   ├── 06_streamline_regressions.R
│   ├── 07_eurobarometer_analysis.R
│   ├── 08_four_plots_outcomes.R
│   ├── 08_four_plots_outcomes.R
│   ├── 09_descriptives_figures.R
│   ├── 10_eu_plots.R
│   ├── 11_v2_regression_tables.R
│   ├── 12_v2_joint_tss_models.R
│   ├── v2_LOO_analysis.R
│   └── 13_v2_replication_thirdpillar_observed.R
│
├── data_raw/
│   └── Raw data from all original sources (not modified)
│
├── data_clean/
│   └── Cleaned and merged datasets used for analysis
│
├── outputs/
│   ├── Tables (Excel)
│   └── Figures used in the manuscript
│
└── README.md
```

## Data Sources

The analysis draws on multiple international data sources, including:

-   Third Sector Size (TSS) dataset (Schiltz, F., MacKay, K. J., &
    Vandekerckhove, P. (2024). Measuring the Size of the “Third Pillar”:
    A Global Dataset. The World Bank Economic Review, 38(4), 861-873.
    <https://doi.org/10.1093/wber/lhae012> )

-   ND‑GAIN Index (Readiness and Adaptive Capacity)

-   INFORM Index (Coping Capacity)

-   EM‑DAT disaster impacts (processed via OWID)

-   World Bank population data

-   Humanitarian funding data (UNOCHA)

-   GDP per capita (World Bank) and HDI (UNDP)

-   Eurobarometer survey data on disaster‑response volunteering (EU
    subsample)

Raw data are stored in data_raw/.

## Script overview

Scripts are designed to be run sequentially.

### 01_load_packages.R 

Loads all required R packages used throughout the project.

### 02_functions.R 

Helper and utility functions used across scripts.

⚠️ Note: this file currently contains legacy and unused functions and
will be cleaned in future revisions.

### 03_import_data.R 

Imports and harmonizes all raw data sources used in the paper

No analysis is conducted at this stage.

### 04_clean_merge.R 

Cleans, harmonizes, and merges all datasets into a master analytical
dataset, using TSS as the reference frame (left‑joined).

The resulting dataset is saved to data_clean/ and used in all subsequent
analyses.

### 05_descriptives.R 

Produces global descriptive statistics and missingness checks

### 06_streamline_regressions.R 

Runs all regression models reported in the paper, including:

-   Bivariate models

-   Models with controls (hazard exposure, humanitarian aid, GDP, HDI - added one by one and then in sets)

-   Robust standard errors

Results are exported as structured tables to outputs/.

### 07_eurobarometer_analysis.R 

Exploratory analysis of the Eurobarometer survey data for the EU
subsample, focusing on disaster‑response volunteering, trust, and
preparedness‑related attitudes.

### 08_four_plots_outcomes/predictors.R 

Generates the main regression plots used in the manuscript, either for common outcomes or common predictors

### 09_descriptives_figures.R 

Creates descriptive figures (distributions, correlations, summaries).

### 10_eu_plots.R 

Produces figures specific to the EU volunteering analysis.

### 11_v2_regression_tables.R 

Makes the regression tables for the manuscript.

### 12_v2_joint_tss_models.R 

Joint TSS-P and TSS-V model to assess the influence of each TSS part.

### 13_v2_replication_thirdpillar_observed.R 

Attempt on replicating the third pillar dataset to get the observed data only in a robustness check in which we don't include the model-based estimates of TSS and its parts.

### v2_LOO_analysis.R 

Leave-one-out analysis to assess if there are countries biasing the results.

## Outputs

outputs/ contains final tables and figures used in the paper. Tables are
saved as Excel files. Figures are saved in publication‑ready formats.

All outputs are fully reproducible from the scripts.

## Reproducibility Notes

The project is written in R.
