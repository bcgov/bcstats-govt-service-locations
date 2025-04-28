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

### Key Steps in Map Creation

1. **Loading Processed Data**:
   - The script reads the processed shapefiles and summary statistics created in `00-make-data.R` and `01-descriptive-tables.R`.
   - Shapefiles for Census Subdivisions (CSDs), Dissemination Areas (DAs), and Dissemination Blocks (DBs) are loaded.

2. **Filtering Data**:
   - The data is filtered to include only the municipalities of interest, as defined in the `CSD_NAMES` constant.

3. **Building Thematic Maps**:
   - The script uses the `ggplot2` package to create thematic maps.
   - Accessibility metrics, such as the number of addresses or average drive times, are visualized using color-coded fill scales.
   - Custom themes and labels are applied to enhance readability.

4. **Adding Service BC Locations**:
   - The locations of Service BC offices are plotted on the maps using point markers.
   - Labels for municipalities are added, with adjustments to avoid overlapping.

5. **Output Files**:
   - The generated maps are saved as image files in the `MAP_OUT` folder.

### Outputs
The maps generated in this script provide visual insights into accessibility metrics and are used to:
- Highlight geographic disparities in service access.
- Support decision-making for resource allocation and service planning.

### Example Maps
- **Pilot Regions Map**: Displays the pilot municipalities (e.g., Langford, Dawson Creek, Smithers, Kamloops) and their associated Service BC locations.
- **Accessibility Metrics Map**: Visualizes metrics such as the number of addresses or average drive times at the Dissemination Block (DB) level.


---


### Purpose
This script creates a map that highlights the pilot regions (`Langford`, `Dawson Creek`, `Smithers`, `Kamloops`) and plots the locations of Service BC offices within these regions. It serves as a foundational visualization for understanding the spatial context of the analysis.

### Key Features

1. **BC Map**:
   - The base map of British Columbia is generated using the `bcmaps` package.
   - The map is transformed to the appropriate coordinate reference system (CRS 3005).

2. **Locality Shapefiles**:
   - Shapefiles for Census Subdivisions (CSDs) are loaded and filtered to include only the pilot municipalities.
   - Centroids of the CSDs are calculated and adjusted to avoid overlapping labels.

3. **Service BC Locations**:
   - The locations of Service BC offices are loaded from a CSV file and plotted as yellow markers on the map.

4. **Customizations**:
   - Labels for municipalities are added with adjustments to improve readability.
   - A color-coded fill scale is applied to distinguish between localities.

5. **Output**:
   - The final map is saved as `pilot_regions.png` in the `MAP_OUT` folder.

### Outputs
The map generated in this script provides:
- A visual representation of the pilot municipalities.
- The geographic locations of Service BC offices within these regions.

### Example Map
The map includes:
- **Regions**: Highlighted in distinct colors for each locality.
- **Service BC Locations**: Marked with yellow symbols.
- **Municipality Labels**: Positioned to avoid overlap with other map elements.

This visualization is a key component of the project, offering a clear and intuitive way to understand the spatial distribution of the pilot regions and their Service BC locations.

---

### How It Fits



## Future Work
- Expand the analysis to include additional municipalities and public services.
- Incorporate socio-economic and demographic factors into the analysis.
- Improve the documentation for each script and function.