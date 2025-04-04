[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)


## Project Description

This repository houses R code for analyzing the geographic accessibility of essential public services (e.g., government centers, hospitals, schools) in British Columbia. Using spatial analysis techniques, it calculates accessibility scores, typically aggregated to small geographic units like Statistics Canada Dissemination Areas (DAs) or Dissemination Blocks (DBs), based on factors like population distribution (density), facility locations, and travel times (initially driving).

The primary goal is to identify potential geographic disparities in service access – highlighting potentially underserved populations or underutilized facilities. The outputs aim to provide quantitative insights to support service planning, resource allocation, and equitable service delivery strategies, allowing for analysis focused on specific targeted municipalities or regions of interest.

*Note: The code and analytical methodology are currently under development.*
 
## Secure Data Access

Accessing project files and data requires the [`safepaths`](https://github.com/bcgov/safepaths) R package. This package securely manages the LAN paths to the data, abstracting sensitive location details from the user. VPN connection is required to use and configure `safepaths` as well as the LAN location of all data files. 

## Installation

**R:** This project requires a recent version of R (4.0.0 or later).

**R Packages:** Install required packages using:

```R
# Manually install packages:
install.packages(c("sf", "tidyverse", "glue", "janitor", "e1071", "remotes"))
remotes::install_github("bcgov/safepaths")
```

**Safepaths Configuration:** Follow the [safepaths package documentation](https://github.com/bcgov/safepaths) for initial setup. You will need the project's specific LAN path key from the project maintainers to configure access to the required datasets.

## Data Sources

This analysis relies on the following primary data sources:

**Geocoded Address Points (via BC Geocoder API)**

Geographic coordinates for project-relevant addresses were generated using the British Columbia Physical Address Geocoder REST API service ([https://geocoder.api.gov.bc.ca/](https://geocoder.api.gov.bc.ca/)). This API performs address validation and returns point location data (latitude/longitude or projected coordinates). The output dataset utilized in this project includes these derived coordinates along with the corresponding Statistics Canada Dissemination Block (DB) identifier (blockID field from the API response), enabling spatial linkage. Records returned by the geocoder that lacked valid coordinates were excluded from the analysis dataset.

While the source address list input to the geocoder is restricted under license, the derived geocoded point coordinates used the analysis are open. 

**NFA Data:** 

(Description pending - this dataset represents output from the Nearest Facility Analysis pipeline, details to be added.)

**Geographic Concordance File**

A geographic concordance file was acquired from Statistics Canada ([statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21](statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21)) to facilitate data linkage across disparate census geographic hierarchies (e.g., linking Dissemination Areas to Census Subdivisions). This file enables the aggregation or translation of data between administrative or statistical units. Note: This dataset has been acquired but not yet implemented in the current analysis workflow.

**Digital Boundary Files**

Digital boundary files in Shapefile format (.shp), defining the geographic extents of Population Centers (PC), Dissemination Areas (DAs) and Dissemination Blocks (DBs), were obtained from Statistics Canada ([https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21)). These vector datasets served as the geometric base for cartographic visualization and underpinning geospatial operations within the analysis.

**Census Population and Dwelling Counts**

Aggregate population and dwelling count data at the Dissemination Area (DA) level were extracted from the Statistics Canada 2021 Census profile tables ([https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001502&geocode=A000259](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001502&geocode=A000259) - Table 98-10-0015-02]. This dataset provided the foundational demographic inputs for calculating population density metrics and deriving other relevant socio-spatial indicators for the designated study regions.

## Guiding Principles

This project operates under the following guidelines:

1.  This GitHub repository stores only code. All data resides on secure LAN storage accessed via `safepaths`.
2.  The analysis utilizes data that contain no Personal Information (PI) or other sensitive information.  
3.  In line with BC Government digital principles, the analytic code is developed openly to promote transparency and reproducibility.


## How to Contribute

If you would like to contribute to the guide, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms and conform to the project Guiding Principles.


## Contact 

For questions about this project or to obtain the necessary `safepaths` configuration, please contact Bonnie Ashcroft at bonnie.ashcroft@gov.bc.ca or open an issue in this repository.

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

