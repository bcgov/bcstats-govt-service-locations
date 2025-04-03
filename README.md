[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)


## Project Description

 
## Project Files and Access

Accessing project files and data requires the `safepaths` R package. This package securely manages the LAN paths to the data, abstracting sensitive location details from the user. VPN connection is required to use and configure `safepaths` as well as the LAN location of all data files. 

Please refer to the safepaths package documentation for installation and usage instructions: https://github.com/bcgov/safepaths, and the project maintainers for project's folder LAN path.

## Data Sources

This project uses data from the following sources:

**Geocoded Address Points (via BC Geocoder API)**

Geographic coordinates for project-relevant addresses were generated using the British Columbia Physical Address Geocoder REST API service ([Base URL: https://geocoder.api.gov.bc.ca/]). This API performs address validation and returns point location data (latitude/longitude or projected coordinates). The output dataset utilized in this project includes these derived coordinates along with the corresponding Statistics Canada Dissemination Block (DB) identifier (blockID field from the API response), enabling spatial linkage. Records returned by the geocoder that lacked valid coordinates were excluded during further processing.

While the source address list input to the geocoder is restricted under license, the derived geocoded point coordinates identifiers used in further analysis are open. 

**NFA Data:** 

(Description pending - this dataset represents output from the Nearest Facility Analysis pipeline, details to be added.)

**Geographic Concordance File**

A geographic concordance file was acquired from Statistics Canada ([statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21]) to facilitate data linkage across disparate census geographic hierarchies (e.g., linking Dissemination Areas to Census Subdivisions). This file enables the aggregation or translation of data between administrative or statistical units. Note: This dataset has been acquired but not yet implemented in the current analysis workflow.

**Digital Boundary Files (Shapefile Format)**

Digital boundary files in ESRI Shapefile format (.shp), defining the geographic extents of Dissemination Areas (DAs) and Dissemination Blocks (DBs), were obtained from Statistics Canada ([https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21]). These vector datasets served as the geometric base for cartographic visualization and underpinning geospatial operations within the analysis.

**Census Population and Dwelling Counts**

Aggregate population and dwelling count data at the Dissemination Area (DA) level were extracted from the Statistics Canada 2021 Census profile tables ([https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001502&geocode=A000259] - Table 98-10-0015-0]. This dataset provided the foundational demographic inputs for calculating population density metrics and deriving other relevant socio-spatial indicators for the designated study region [or specify 'for selected DAs based on criteria X' if applicable].

## Guiding Principles

This project operates under the following guidelines:

1.  **Code Repository Only:** This GitHub repository stores only code. All data resides on secure LAN storage accessed via `safepaths`.
2.  **Non-Sensitive Data Focus:**  The analysis utilizes data that contain no Personal Information (PI) or other sensitive information.  
3.  **Open Code:** In line with BC Government digital principles, the analytic code is developed openly to promote transparency and reproducibility.


## How to Contribute

If you would like to contribute to the guide, please see our [CONTRIBUTING](CONTRIBUTING.md) guideleines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

In addition, no contributor will push data to this repository as no data will be stored in this repository. 

## License

Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

