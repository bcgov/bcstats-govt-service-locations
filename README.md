[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)


## Project Description

This repository houses R code for analyzing the current geographic location of Service BC locations in British Columbia. Using spatial analysis techniques, the code scripts calculate accessibility metrics, aggregated at both the Census Subdivision (CSD) level and Service BC Location.  Accessibility metrics are based on factors such as population density distribution, facility locations, and travel times (initially by car).

The primary goal is to identify potential geographic disparities in service access, highlighting potentially underserved populations or underutilized facilities. The derived data tables and visualizations provide quantitative insights to support service planning, resource allocation, and equitable service delivery strategies.  They allow for for analysis focused on specific targeted municipalities or regions of interest.

The first phase of the project focused on the drive times to Service BC locations for four municipalities (Smithers, Langford, Kamloops and Dawson Creek) and subsequently expanded to the wider province. Much of the analysis is conducted at the Dissemination Block (DB) level, with metrics aggregated to larger geographic areas as needed.
 
## Secure Data Access

Accessing project files and data requires the [`safepaths`](https://github.com/bcgov/safepaths) R package. This package securely manages the LAN paths to the data, abstracting sensitive location details from the user. VPN connection is required to use and configure `safepaths` as well as the LAN location of all data files. 


## Data Storage Structure

All data is stored in a hierarchical network folder structure accessed by leveraging [`safepaths`](https://github.com/bcgov/safepaths) functionality.

```
{LAN_FOLDER}/2025 Government Service Locations/
├── data/
    ├── source/          # Processed intermediate files
    └── raw/             # Raw input data
        └── nearest_facility_BC/  # Drive time analysis results
└── outputs/
    ├── tables/          # Final output tables  
    └── visuals/         # Maps and visualizations
```

## Installation and Usage

**R:** This project requires a recent version of R (4.0.0 or later).

**R Packages:** Install required packages using:

```R
# Manually install packages:
install.packages(c(
  "sf", "tidyverse", "glue", "janitor", "e1071", "remotes",
  "svglite", "scales", "rmapshaper", "bcmaps", "snakecase", 
  "tigris", "spatstat", "stars", "terra", "fs", "ggnewscale" # Added packages for new scripts
))


remotes::install_github("bcgov/safepaths")
```

**Safepaths Configuration:** Follow the [safepaths package documentation](https://github.com/bcgov/safepaths) for initial setup. You will need the project's specific LAN path key from the project maintainers to configure access to the required datasets.


## Global Constants and Configuration

Global constants and configuration settings used throughout the Service BC data pipeline are centralized in a `settings.R` file. This file defines key parameters, file paths, and data validation requirements and is required to run the pipeline scripts. 


## How to Run the Analysis

This section describes how to run the R scripts to perform the drive time analysis after completing the installation and setup. 

### Prerequisites

**Raw Data**
Ensure the necessary raw input data files are placed in the directory structure expected by the `settings.R` script. By default, data is assumed to be kept in the location specified by `RAW_DATA_FOLDER`. 

**Configuration Settings**
Open and review the `R/settings.R` script. Verify:

- The path returned from `safepaths::use_network_path()` correctly points to your network or local data storage location.
- The values in `EXPECTED_LOCALITIES` list matches the localities you intend to process, and for which data exists.
- Paths like `SRC_DATA_FOLDER`, `SHAPEFILE_DIR` and other patterns/constants defined in `settings.R` align with your input data structure and project needs.

### Execution

Run the following scripts in the order specified:

**Preprocessing Tabular Data:**

- `00-make-data.R`: This script preprocesses raw data files, generates shapefiles for census subdivisions and dissemination blocks, processes population projections, and creates correspondence files linking dissemination blocks, dissemination areas, and census subdivisions. It writes cleaned and reduced datasets to the `SRC_DATA_FOLDER` and shapefiles to the `SHAPEFILE_OUT` directory.  The script generates multiple outputs, including processed CSV files, shapefiles, and RDS files. 

**Aggregated Tables and Calculations:**

- `01-descriptive-tables.R`: this script reads processed files generated in the previous step, performs data quality checks, calculates summary statistics at the Dissemination Block (DB) and Census Subdivision (CSD) levels, merges population data, and identifies Service BC locations. It writes the final analysis output files to the specified directories.

- `01b-compare-metrics.R`: this script generates a scatter plot to compare driving time and driving distance at the Dissemination Block (DB) level, grouped by Census Subdivision (CSD). It reads preprocessed data from the SRC_DATA_FOLDER, calculates driving time in minutes, and creates a scatter plot with linear regression lines for each CSD. The plot is saved as a PNG file in the VISUALS_OUT/csd-drive-distance-plots directory.

- `01c-bin-distances.R`: this script processes driving time and distance data to group (bin) dissemination blocks (DBs) by driving distance to the nearest Service BC location. It calculates the total count and percentage of addresses within each distance bin for each Census Subdivision (CSD). The script outputs the results in both long and wide formats for reporting.

**Mapping Results:**

- `02-map-plots.R`: this script loads processed Dissemination Block (DB) shapefiles and drive time summary statistics CSV files produced earlier in the workflow. It merges these datasets and leverages a custom function `build_map()` to generate and save a series of map visualizations. The maps display quantitative information (e.g., mean driving distance) at the DB level for each Census Subdivision (CSD), based on the configured theme (MAP_THEME) and other parameters defined in settings.R.

- `02b-box-violin-plots.R`: this script generates statistical visualizations (box plots and violin plots) to show the distribution of driving distances to the nearest Service BC office across different Census Subdivisions (CSDs) in British Columbia. It uses preprocessed Dissemination Block (DB)-level data to compare access metrics between regions.

- `02c-density_map.R`: this script generates spatial density maps to visualize the distribution of driving distances to the nearest Service BC office for different Census Subdivisions (CSDs) in British Columbia. It uses kernel density estimation to create smoothed heatmaps of driving distances, overlaying them on CSD boundaries.

- `02d-csd-demographics.R` the script processes population projections, census data, and geographic crosswalks to calculate statistics such as total population and median age for each different Census Subdivisions (CSDs) in British Columbia. The script also generates population pyramids to visualize the age and gender distribution within each CSD, creating both individual and combined visualizations. 

**Creating and Analyzing Catchment Areas:**

- `03a-create-catchments.R`: the script assigns every Dissemination Block (DB) to a Service BC facility catchment area. It first uses drive time data to assign DBs to the nearest facility, then applies a spatial proximity method for any unassigned DBs. This process ensures complete geographic coverage with no gaps.

- `03b-plot-catchments.R`: the script creates several maps visualizing the Service BC facility catchment areas defined in the previous step. It generates both pilot-region focused maps and province-wide visualizations, showing both the original drive-time based assignments and the complete catchment assignments including spatial proximity assignments.

- `03c-sbc-centric-stats.R`: the script creates detailed metrics for each individual Service BC location based on their catchment areas. It generates population projections, driving distance metrics, demographic analyses, and various visualizations for each facility. The script also produces population pyramids showing age and gender distribution for each catchment.

- `03d-sbc-density-maps.R`: this script generates spatial density maps to visualize drive times to the nearest Service BC office for each facility's catchment area. Using kernel density estimation, the script creates smoothed heatmaps of drive times, highlighting areas within each catchment. It overlays these heatmaps with geographic boundaries, Service BC locations, and points representing addresses. The script ensures that maps are created for each catchment region, with additional handling for areas lacking drive time data. These maps provide insights into the spatial distribution of accessibility within Service BC catchments.

    *   **Script:** `04-sbc-servicebc-catchments.R`.
        *   **Purpose:** This script generates a map visualizing Service BC catchment areas based on Census Subdivision (CSD) boundaries. It overlays Service BC office locations, highlights assigned CSDs for each facility, and includes labels for pilot municipalities.
        *   **How to Run:** This script can be run from source: `source("R/04-sbc-servicebc-catchments.R")`
        *   **Expected Output:** The script creates a map image file (`bc-csd-sbc-FROM-SERVICE-BC.png`) saved in the `MAP_OUT/complete_catchments` directory. The map includes CSD boundaries, Service BC office locations, and facility assignments for pilot regions.

 **Generating Statistical Tables for Service BC:**
    *   **Script:** `05a-csd-data-tables.R`.
        *   **Purpose:** This script generates a comprehensive table of Census Subdivision (CSD)-level statistics, including population estimates, age distributions, rurality proportions, and drive time metrics. It combines data from multiple sources to provide a detailed summary for each CSD.
        *   **How to Run:** This script can be run from source: `source("R/05a-csd-data-tables.R")`
        *   **Expected Output:** The script creates a CSV file (`csd-statistics-for-SBC.csv`) saved in the `TABLES_OUT` directory. The table includes metrics such as population projections, median driving distances, and proportions of rural populations for each CSD.   

     *   **Script:** `05b-sbc-data-tables.R`.
        *   **Purpose:** This script generates detailed statistics for each Service BC location, including population projections, drive time metrics, rurality proportions, and demographic breakdowns. It combines data from multiple sources to provide a comprehensive summary for each Service BC facility.
        *   **How to Run:** This script can be run from source: `source("R/05b-sbc-data-tables.R")`
        *   **Expected Output:** The script creates a CSV file (`sbc-location-statistics-for-SBC.csv`) saved in the `FOR_SBC_OUT` directory. The table includes metrics such as population projections, median driving distances, age group distributions, and rurality classifications for each Service BC location.

 **Analyzing Urban and Rural Classifications:**
    *   **Script:** `05c-rural-analysis.R`.
        *   **Purpose:** This script analyzes urban and rural classifications for addresses and Census Dissemination Blocks (DBs) using two methods: Canada Post Forward Sortation Areas (FSAs) and Statistics Canada's population centers. It generates summaries at the provincial, catchment, and Census Subdivision (CSD) levels, and creates visualizations to compare urban and rural classifications across methods.
        *   **How to Run:** This script can be run from source: `source("R/05c-rural-analysis.R")`
        *   **Expected Output:** The script generates multiple outputs:
            - Summary tables of urban and rural classifications at the provincial, catchment, and CSD levels, saved as CSV files in the `FOR_SBC_OUT` directory.
            - Comparison tables of urban and rural classifications across methods, saved as CSV files.
            - Visualizations of urban and rural areas, including maps and plots, saved as image files in the `MAP_OUT/rural-analysis` directory.    

     *   **Script:** `05d-plot-rural-urban.R`.
        *   **Purpose:** This script generates visualizations showing urban and rural classifications for Dissemination Blocks (DBs) within population centers. It uses shapefiles and preprocessed data to create plots for each population center, highlighting the spatial distribution of urban and rural areas.
        *   **How to Run:** This script can be run from source: `source("R/05d-plot-rural-urban.R")`
        *   **Expected Output:** The script creates SVG files for each population center, saved in the `FOR_SBC_OUT/rural-method-misc` directory. These visualizations show the urban and rural classifications for DBs within each population center, providing insights into the spatial patterns of rurality.

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

