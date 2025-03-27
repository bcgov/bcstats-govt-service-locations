# Notes

Geodata team has updated the data with Dissemintaion Block id but can add any admin boundary id if requested 
Geodata fixed those addresses that does not have valid coordinates or are not connected to the road network. - check if this has been done in our files?  


The project team has been given permission by executive to do this work in the open based on:

1. no data is stored in GitHub
2. the data used for this analysis step has no PI
3. defaulting to the open follows our digital principles and 
4. this will enable transparency
5. allow for easier easier progress. 

If at any time the "risk" changes we can convert the repo to bcgov-c GitHub.

## Data Sources

This project uses data from the following sources:

**Geocoder/NFA data output (include metadata)** 

The original address data used by the geocoder is restricted under licencing, however the output is open source. As such, we have not included any of the source data in this project.  

**Dissemination Geographies Relationship File (not used yet)**

A mapping file was (along with related correspondence files) was used to cross-reference and integrate data across different geographic hierarchy. In particular from CSD to DA's. The files were sourced from Statistics Canada and downloaded from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21


**Dissemination Area Boundary Files**

Boundary Shape Files (.shp) for Dissemination Areas (DAs) and Dissemination Blocks (DBs) were sourced from Statistics Canada.  These files were used to create maps and perform spatial analysis.  The shape files were downloaded from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21


**Population Data**

Population and dwelling counts for selected dissemination areas were sourced from Statistics Canada’s 2021 Census.  This data was used to calculate population density and other demographic statistics.  The data was downloaded from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001502&geocode=A000259 

Statistics Canada. Table 98-10-0015-02  Population and dwelling counts: Canada, provinces and territories and dissemination areas. DOI: https://doi.org/10.25318/9810001501-eng

## Sample data assets

Drew suggested we can include some open Vancouver address data attached as a release asset so ppl can run the code as well, which I think is a great idea. 

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