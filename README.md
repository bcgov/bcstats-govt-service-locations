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

1.  Ensure the necessary raw input data files are placed in the `RAW_DATA_FOLDER` directory as specified in `settings.R`.
2.  Open and review the `R/settings.R` script. Verify that:
    *   The path returned from `safepaths::use_network_path()` correctly points to your network or local data storage location.
    *   The values in `CSD_NAMES` and `CSDIDS` match the Census Subdivisions you intend to process, and for which data exists.
    *   Paths like `SRC_DATA_FOLDER`, `SHAPEFILE_OUT` and other constants defined in `settings.R` align with your input data structure and project needs.

**Execution:**
Run the following scripts in the order specified. Each script can be run from source using: `source("R/script-name.R")`

1. `00-make-data.R` Reads raw drive time data, creates geographic crosswalks between census geographies (DB, DA, CSD), and retrieves population data and projections. Produces both full provincial datasets and reduced datasets filtered to the Census Subdivisions of interest, along with Service BC location data and DB-level population projections. Outputs data files to SRC_DATA_FOLDER.
2. `01a-descriptive-tables.R` Calculates summary statistics for drive times and distances at DB, DA, and CSD levels, merging with population data from the 2021 Census. Performs data quality checks including identifying regions with low address counts and calculates the number of Service BC locations per CSD. Outputs summary statistics tables to SRC_DATA_FOLDER.

3. `01b-compare-metrics.R` Creates scatter plots comparing driving time versus driving distance metrics at the dissemination block level. Saves scatter plot visualizations to the VISUALS_OUT directory.

4. `01c-bin-distances.R` Bins drive distance data into distance categories (Under 1 km, 1-5 km, 5-10 km, 10-20 km, 20+ km) and calculates address counts and percentages by CSD. Creates binned distance summary tables showing the distribution of addresses across distance ranges. Outputs binned data tables to TABLES_OUT.

5. `02a-map-plots.R` Loads DB-level shapefile and drive time data, merging them with Service BC location coordinates to create map-ready datasets. Uses the `build_map` function to generate customizable maps displaying accessibility metrics at the dissemination block level.

6. `02b-box-violin-plots.R` Creates box plots and violin plots showing distribution of drive times to Service BC offices across different regions. Outputs statistical distribution plots as PNG files in the VISUALS_OUT directory.

7. `02c-density_map.R` Creates spatial density maps using kernel density estimation to show drive time heatmaps for census subdivisions. Saves SVG density maps to the MAP_OUT directory.

8. `02d-csd-demographics.R` Analyzes demographic characteristics and population projections for census subdivisions with Service BC locations. Creates population pyramids and demographic summary statistics. Outputs demographic analysis files to TABLES_OUT and population pyramid visualizations to MAP_OUT.

9. `03a-create-catchments.R` Assigns every Dissemination Block (DB) to a Service BC facility catchment area using drive time data and spatial proximity methods to ensure complete geographic coverage. Generates a complete DB assignments CSV file and additional statistics tables to the TABLES_OUT directory.

10. `03b-plot-catchments.R` Creates maps visualizing Service BC facility catchment areas with both pilot-region focused and province-wide visualizations. Saves multiple map visualizations to the MAP_OUT directory including catchment overviews and complete catchment maps.

11. `03c-sbc-centric-stats.R` Creates detailed metrics for each Service BC location based on their catchment areas, including population projections, driving distance metrics, and demographic analyses. Outputs multiple CSV files with drive metrics and population data to TABLES_OUT, plus visualization files including population pyramids to VISUALS_OUT.

12. `03d-sbc-density-maps.R` Creates spatial density maps showing drive times for Service BC catchment regions using kernel density estimation. Generates PNG density maps for each Service BC facility's catchment area to VISUALS_OUT.

13. `04-sbc-servicebc-catchments.R` Creates overview maps showing BC Census Subdivision boundaries, Service BC locations, and pilot CSD catchment areas. Produces overview maps displaying Service BC locations and catchment boundaries across British Columbia. Saves maps to MAP_OUT.

The final analysis output files will be available in the location specified by `SRC_DATA_FOLDER` within your configured data directory structure. You can then use these files for further analysis, reporting, or visualization.


## Data Sources

This analysis relies on the following primary data sources:

**Geocoded Address Points (via BC Geocoder API)**

Geographic coordinates for project-relevant addresses were generated using the British Columbia Physical Address Geocoder REST API service ([https://www2.gov.bc.ca/gov/content/data/geographic-data-services/location-services/geocoder](https://www2.gov.bc.ca/gov/content/data/geographic-data-services/location-services/geocoder)). This API performs address validation and returns point location data (latitude/longitude or projected coordinates). The output dataset utilized in this project includes these derived coordinates along with the corresponding Statistics Canada Dissemination Block (DB) identifier (blockID field from the API response), enabling spatial linkage. Records returned by the geocoder that lacked valid coordinates were excluded from the analysis dataset.

While the source address list input to the geocoder is restricted under license, the derived geocoded point coordinates used for the analysis are open data. 

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

