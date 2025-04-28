---
title: "Technical Documentation"
output: 
    html_document:
        toc: true
        toc_depth: 2
        number_sections: true
        theme: cerulean
        highlight: tango
        code_folding: hide
        fig_caption: true
        self_contained: false
---

This documentation contains notes about the project.  
Eventually, it will be technical documentation for the project but for now is **under construction**.

## settings.R 
This file contains the settings for the project, including global constants (written in all caps) and the paths to the data files. These constants are used throughout the project to ensure consistency and simplify configuration.

### Key Constants in settings.R

#### General Settings
- **CSD_NAMES**: A list of municipalities of interest (`Langford`, `Dawson Creek`, `Smithers`, `Kamloops`).
- **CENSUS_BASIS**: The census year used for the analysis (2021).
- **CANCENSUS_YEAR**: The formatted census year for use with the `cancensus` package.

#### File Paths
- **LAN_FOLDER**: The base folder for the project, configured using the `safepaths` package.
- **SRC_DATA_FOLDER**: The folder containing processed source data files.
- **RAW_DATA_FOLDER**: The folder containing raw input data files.
- **DT_DATA_FOLDER**: The folder containing raw drive time data.
- **SHAPEFILE_OUT**: The folder where processed shapefiles are saved.
- **MAP_OUT**: The folder where map visualizations are saved.
- **TABLES_OUT**: The folder where summary tables are saved.

#### File Patterns
- **NO_ERRS_FILE_PATTERN**: A pattern used to identify valid drive time files.
- **INPUT_ADDR_DA_PATTERN**: A pattern for address files with dissemination area information.

#### Columns
- **REQUIRED_COLS**: The required columns in the input data files.
- **POP_COLS**: The population-related columns used in the analysis.

#### Visualization Settings
- **MAP_THEME**: The default theme for map visualizations.
- **FILL_THEME**: The default fill scale for map visualizations.

---

## 00-make-data.R

This script prepares the data required for subsequent analysis, including creating visuals and tables. It processes raw input files, generates shapefiles, and creates crosswalks for geographic units.

### Key Steps in Data Preparation

1. **Preprocessing Drive Time Files**:
   - The script reads raw drive time files for each locality, cleans the data, and extracts relevant columns.
   - The processed files are saved to the `SRC_DATA_FOLDER`.

2. **Creating Shapefiles**:
   - Shapefiles for Census Subdivisions (CSDs), Dissemination Areas (DAs), and Dissemination Blocks (DBs) are created using the `bcmaps` and `bcdata` packages.
   - These shapefiles are saved to the `SHAPEFILE_OUT` folder.

3. **Generating Crosswalks**:
   - A crosswalk is created to link dissemination blocks (DBs) to dissemination areas (DAs) and Census Subdivisions (CSDs).
   - This crosswalk is used to aggregate data at different geographic levels.

4. **Population Data**:
   - Population data for DAs, DBs, and CSDs is retrieved using the `cancensus` package.
   - The data is cleaned and saved for use in later scripts.

5. **Service BC Location Data**:
   - The script identifies the nearest Service BC locations for each address and saves the results.

6. **Output Files**:
   - The processed data is saved in both full and reduced formats (filtered by the municipalities of interest) for use in subsequent scripts.

### Outputs
The data prepared in this script will be used to:
- Generate summary tables in `01-descriptive-tables.R`.
- Create map visualizations in `02-map-plots.R`.

---

## 01-descriptive-tables.R

This script loads the processed data files created in `00-make-data.R` and calculates summary statistics for drive times and distances. The results are aggregated at three geographic levels:

1. **Dissemination Block (DB)**: The smallest geographic unit for which population and dwelling counts are disseminated.
2. **Dissemination Area (DA)**: A small, relatively stable geographic unit composed of one or more adjacent dissemination blocks.
3. **Census Subdivision (CSD)**: A municipality or equivalent geographic area used for census purposes.

### Key Steps in Summary Table Creation

1. **Loading Processed Data**:
   - The script reads the processed drive time and population data files created in `00-make-data.R`.

2. **Calculating Summary Statistics**:
   - Summary statistics such as mean, variance, skewness, kurtosis, and quantiles are calculated for drive times and distances.
   - The statistics are grouped by the geographic levels (DB, DA, and CSD).

3. **Data Validation**:
   - The script performs data checks to identify missing or low-count regions.
   - Regions with fewer than five observations are flagged for further investigation.

4. **Output Files**:
   - The aggregated summary tables are saved to the `TABLES_OUT` folder for use in subsequent scripts.

### Outputs
The summary tables generated in this script will be used to:
- Create thematic maps in `02-map-plots.R`.
- Provide quantitative insights for accessibility analysis.

---

## 02-map-plots.R

This script uses the processed shapefiles and summary statistics to create thematic maps. The maps visualize accessibility metrics, such as the number of addresses or average drive times, at different geographic levels.

---

## Future Work
- Expand the analysis to include additional municipalities and public services.
- Incorporate socio-economic and demographic factors into the analysis.
- Improve the documentation for each script and function.