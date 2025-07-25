[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)


## Project Description

This repository houses R code for analyzing the current geographic location of essential public services (e.g., Service BC locations, hospitals and schools) in British Columbia. Using spatial analysis techniques, the code scripts calculate accessibility metrics, aggregated at both the municipal level and by smaller geographic units (i.e. Census Dissemination Areas (DAs) or Dissemination Blocks (DBs)).  Accessibility metrics are based on factors such as population density distribution, facility locations, and travel times (initially by car).

The primary goal is to identify potential geographic disparities in service access, highlighting potentially underserved populations or underutilized facilities. The derived data tables and visualizations provide quantitative insights to support service planning, resource allocation, and equitable service delivery strategies.  They allow for for analysis focused on specific targeted municipalities or regions of interest.

### Phase 1

The first phase of the project focuses on the drive times to Service BC locations for four municipalities (Smithers, Langford, Kamloops and Dawson Creek), with plans to expand to other public services in future phases. The analysis will be conducted at the Dissemination Block (DB) level, with the option to aggregate results to larger geographic units as needed.

### Phase 2

Phase 2 builds on these advances by expanding the analysis, refining methodologies, and applying the approach to all 65 Service BC offices, and all CSD regions in BC.  This work is done in collaboration with Service BC; the aggregated data tables are developed for their purposes. 

*Note: The code and analytical methodology are currently under development.*
 
## Secure Data Access

Accessing project files and data requires the [`safepaths`](https://github.com/bcgov/safepaths) R package. This package securely manages the LAN paths to the data, abstracting sensitive location details from the user. VPN connection is required to use and configure `safepaths` as well as the LAN location of all data files. 

## Installation and Usage

**R:** This project requires a recent version of R (4.0.0 or later).

**R Packages:** Install required packages using:

```R
# Manually install packages:
install.packages(c(
  "sf", "tidyverse", "glue", "janitor", "e1071", "remotes",
  "scales", "rmapshaper", "bcmaps", "snakecase", "readxl", 
  "bcdata", "tigris", "spatstat", "stars", "terra", "fs", "ggnewscale"
))

remotes::install_github("bcgov/safepaths")
```

**Safepaths Configuration:** Follow the [safepaths package documentation](https://github.com/bcgov/safepaths) for initial setup. You will need the project's specific LAN path key from the project maintainers to configure access to the required datasets.

## How to Run the Analysis

This section describes how to run the R scripts to perform the drive time analysis after completing the installation and setup. The process involves five scripts executed sequentially (there may be additional scripts in the future).

**Prerequisites:**

1.  **Raw Data:** Ensure the necessary raw input data files are placed in the directory structure expected by the `settings.R` script. By default, data is assumed to be kept in the location specified by `RAW_DATA_FOLDER`. The raw files should match the naming conventions expected by the scripts (containing locality IDs, etc.).
2.  **Review Configuration Settings:** Open and review the `R/settings.R` script. Verify that:
    *   The path returned from `safepaths::use_network_path()` correctly points to your network or local data storage location.
    *   The values in `EXPECTED_LOCALITIES` list matches the localities you intend to process, and for which data exists.
    *   Paths like `SRC_DATA_FOLDER`, `SHAPEFILE_DIR` and other patterns/constants defined in `settings.R` align with your input data structure and project needs.

**Execution:**
Run the following scripts in the order specified:

1.  **Preprocessing Tabular Data:**
    *   **Script:** `00-make-data.R`.
        *   **Purpose:** This script identifies the latest raw data file for each locality, performs initial validation and preprocessing using the `preprocess_locs` function, and writes intermediate, cleaned files to the `SRC_DATA_FOLDER` (as defined in `settings.R`).
        *   **How to Run:** This script can be run from source: `source("R/00-make-data.R")`
        *   **Expected Output:** Watch the console for messages indicating which files are being processed, warnings about missing/extra localities, potential file overwrites, or errors. Upon successful completion, processed files (e.g., `address_with_da_locality_XXX.csv`) will be present in the `SRC_DATA_FOLDER`.

2. **Shapefile Processing**:
    *   **Script:** `00b-make-map-data.R`.
        *   **Purpose**: Loads provincial DA and DB shapefiles, filters them to keep only the specific localities defined in the crosswalk file (reducing size for development/analysis), and writes these processed, filtered shapefiles to the SHAPEFILE_DIR (as defined in settings.R). This prepares the geographic data needed for mapping.
        *   **How to Run**: This script can be run from source: `source("R/00b-make-map-data.R")`
        *   **Expected Output**: Watch the console for messages indicating which files are being processed, warnings about invalid inputs, potential file overwrites, or errors. Upon success, filtered shapefiles (e.g., containing *DA_for_locality_*... or similar in their names) will be present in the `SHAPEFILE_DIR`. 

3.  **Aggregated Tables and Calculations:**
    *   **Script:** `01-descriptive-tables.R`.
        *   **Purpose:** This script reads the processed files generated by the previous step, performs data quality checks, calculates summary statistics at the DB, DA and locality level, merges population data, identifies Service BC locations and writes the final analysis output files.
        *   **How to Run:** This script can be run from source: `source("R/01-descriptive-tables.R")` 
        *   **Expected Output:** Check the console for messages about data loading, data quality warnings (e.g., handling NAs or negative values), file overwrite warnings, or errors. Successful completion will result in the final summary CSV files (e.g., `db_average_times_dist_loc_all.csv`, `da_average_times_dist_loc_all.csv`) being written to the `SRC_DATA_FOLDER`.

4.  **Mapping Results:**
    *   **Script:** `02-map-plots.R`.
        *   **Purpose:** Loads the processed DA/DB shapefiles (from `00b-make-map-data.R`) and the drive time summary statistics CSV files (from `01-descriptive-tables.R`). It merges these datasets and uses the `build_map` function (from `R/fxns/plots.r`) to generate and save a series of map visualization based on the configured theme (`MAP_THEME` in `settings.R`).
        *   **How to Run:** Manually adjust inputs to the `build_map` function, save and source the file: `source("R/02-map-plots.R")`
        *   **Expected Output:** Check the console for messages about data loading, data quality warnings (e.g., handling NAs or negative values), file overwrite warnings, or errors.  Successful completion will result in a series of map visualizations (e.g., PNG, SVG) saved to the output directory specified within the script or `settings.R`. 

    *   **Script:** `02b-plot-localities.R`.
        *   **Purpose:** Generates a map visualizing the geographic locations of the specific municipalities/localities included in the analysis, providing context within British Columbia. It uses the processed shapefiles and the service BC locations processed previously.
        *   **How to Run:** This script can be run from source: `source("R/02b-plot-localities.R")`
        *   **Expected Output:** Check the console for messages about data loading, data quality warnings (e.g., handling NAs or negative values), file overwrite warnings, or errors.  Successful completion will result in a map image file (e.g., PNG, SVG) saved to disk, showing the study area boundaries overlaid on a map of BC.

5.  **Creating and Analyzing Catchment Areas:**
    *   **Script:** `03a-create-catchments.R`.
        *   **Purpose:** This script assigns every Dissemination Block (DB) to a Service BC facility catchment area. It first uses drive time data to assign DBs to the nearest facility, then applies a spatial proximity method for any unassigned DBs. This process ensures complete geographic coverage with no gaps.
        *   **How to Run:** This script can be run from source: `source("R/03a-create-catchments.R")`
        *   **Expected Output:** Upon successful completion, the script generates a complete DB assignments CSV file (`complete_db_assignments.csv`) in the `SRC_DATA_FOLDER` and additional statistics tables in the `TABLES_OUT` directory. The console will display information about the number of DBs assigned using each method.

    *   **Script:** `03b-plot-catchments.R`.
        *   **Purpose:** This script creates several maps visualizing the Service BC facility catchment areas defined in the previous step. It generates both pilot-region focused maps and province-wide visualizations, showing both the original drive-time based assignments and the complete catchment assignments including spatial proximity assignments.
        *   **How to Run:** This script can be run from source: `source("R/03b-plot-catchments.R")`
        *   **Expected Output:** The script generates multiple map visualizations saved to the `MAP_OUT/complete_catchments` directory, including pilot region overviews, original drive-time catchments, and complete catchment maps. These include both focused maps of the pilot regions and provincial overviews.

    *   **Script:** `03c-sbc-centric-stats.R`.
        *   **Purpose:** This script creates detailed metrics for each individual Service BC location based on their catchment areas. It generates population projections, driving distance metrics, demographic analyses, and various visualizations for each facility. The script also produces population pyramids showing age and gender distribution for each catchment.
        *   **How to Run:** This script can be run from source: `source("R/03c-sbc-centric-stats.R")`
        *   **Expected Output:** The script generates multiple CSV files in the `TABLES_OUT` directory containing drive metrics, CSD counts, and population metrics for each Service BC location. It also creates visualization files in the `MAP_OUT` directory, including drive distance histograms, drive distance maps, and population pyramids for each facility.

The final analysis output files will be available in the location specified by `SRC_DATA_FOLDER` within your configured data directory structure. You can then use these files for further analysis, reporting, or visualization.


## Data Sources

This analysis relies on the following primary data sources:

**Geocoded Address Points (via BC Geocoder API)**

Geographic coordinates for project-relevant addresses were generated using the British Columbia Physical Address Geocoder REST API service ([https://www2.gov.bc.ca/gov/content/data/geographic-data-services/location-services/geocoder](https://www2.gov.bc.ca/gov/content/data/geographic-data-services/location-services/geocoder)). This API performs address validation and returns point location data (latitude/longitude or projected coordinates). The output dataset utilized in this project includes these derived coordinates along with the corresponding Statistics Canada Dissemination Block (DB) identifier (blockID field from the API response), enabling spatial linkage. Records returned by the geocoder that lacked valid coordinates were excluded from the analysis dataset.

While the source address list input to the geocoder is restricted under license, the derived geocoded point coordinates used for the analysis are open. 

**NFA Data:** 

(Description pending - this dataset represents output from the Nearest Facility Analysis pipeline, details to be added.)

**Geographic Boundary Data Crosswalk**

A geographic crosswalk between different census geographies (Dissemination Blocks, Dissemination Areas, and Census Subdivisions) was created based on the Dissemination Block shapefile available from the BC Data Catalogue. This crosswalk facilitates data linkage across disparate census geographic hierarchies, enabling the aggregation or translation of data between administrative or statistical units.

**Digital Boundary Files**

Digital boundary files in GeoPackage format (.gpkg) defining the geographic extents of Census Subdivisions (CSDs), Dissemination Areas (DAs) and Dissemination Blocks (DBs) were obtained from the BC Data Catalogue:
- [Census Subdivisions (CSDs)](https://catalogue.data.gov.bc.ca/dataset/current-census-subdivision-boundaries)
- [Dissemination Areas (DAs)](https://catalogue.data.gov.bc.ca/dataset/current-census-dissemination-areas)
- [Dissemination Blocks (DBs)](https://catalogue.data.gov.bc.ca/dataset/current-census-dissemination-blocks)

These vector datasets serve as the geometric base for cartographic visualization and underpin geospatial operations within the analysis (used as input for `00b-make-map-data.R`).

**Census Population and Dwelling Counts**

Aggregate population and dwelling count data at the Dissemination Block (DB) level were extracted from the Statistics Canada 2021 Census. This dataset provides the foundational demographic inputs for calculating population density metrics and deriving other relevant socio-spatial indicators for the designated study regions (used in `01-descriptive-tables.R`).

**Population Projections**

Population projections at the Census Subdivision (CSD) level were obtained from BC Stats through the BC Data Catalogue ([Sub-Provincial Population Projections](https://catalogue.data.gov.bc.ca/dataset/bc-sub-provincial-population-estimates-and-projections)). These projections provide future population estimates by age and gender, allowing for demographic analysis of service catchment areas over time. The projections are used in `03c-sbc-centric-stats.R` to estimate future service demands and demographic composition within Service BC facility catchment areas.


## Guiding Principles

This project operates under the following guidelines:

1.  This GitHub repository stores only code. All data resides on secure LAN storage accessed via `safepaths`.
2.  The analysis utilizes data that contain no Personal Information (PI) or other sensitive information.  
3.  In line with BC Government digital principles, the analytic code is developed openly to promote transparency and reproducibility.


## How to Contribute

If you would like to contribute to the guide, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms and conform to the project Guiding Principles.


## Contact 

For questions about this project or to obtain the necessary `safepaths` configuration, please contact [Bonnie Ashcroft](https://github.com/BonnieJRobert) or open an issue in this repository.

## License

Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

