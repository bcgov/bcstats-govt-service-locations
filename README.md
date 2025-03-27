# Notes

Geodata team has updated the data with Dissemintaion Block id but can add any admin boundary id if requested 
Geodata fixed those addresses that does not have valid coordinates or are not connected to the road network. - check if this has been done in our files?  

## Data Sources

This project uses data from the following sources:

1. Dissemination Geographies Relationship File (not used yet)
StatsCan’s 2021 Census uses a hierarchical geographic framework. At one level, census subdivisions (CSDs) represent municipalities and similar administrative areas,  while at a lower level, dissemination areas (DAs) are created by aggregating dissemination blocks into contiguous areas (typically with 400–700 people).  DAs usually nest within CSD boundaries.  To help users link these different levels, Statistics Canada provides the Dissemination Geographies Relationship File to connect DAs to higher geographic units such as CSDs, census tracts, and beyond.  
This file (along with related correspondence files) was used to cross-reference and integrate data across the geographic hierarchy. 

    https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21

2. Dissemination Area Boundary Files 

StatsCan provided data to the project for boundary files for dissemination areas (DAs) and dissemination blocks (DBs) in various formats.  The shapefile format was used to create maps and perform spatial analysis.  

    https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21

3. Population Data

    https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001502&geocode=A000259 


## How to work with a bare repository

see https://www.saintsjd.com/2011/01/what-is-a-bare-git-repository/

the remote for this is a bare clone created with:
create a bare clone in shared directory

`git clone --bare remoteness-index 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'`

you can clone a copy of this repo

`git clone 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'`

check if remote is set

`git remote -v`
`git remote add origin 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'`

you should be able to add and commit changes

`git add .`
`git commit -m "commit message"`
`git push origin main`

and pull as usual

`git pull origin main`

branches work as well but...looking at remote I don't see where the new branch is specified

switch branches as usual

`git checkout -b test`

commit changes and push 

`git push origin test`
`git pull origin test`

you may need to set the upstream branch

`git push --set-upstream origin test`

## How to Contribute

If you would like to contribute to the guide, please see our [CONTRIBUTING](CONTRIBUTING.md) guideleines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

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