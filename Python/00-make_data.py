# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This script loads raw csv data files containing spatial data for addresses in 
# a defined municipality in BC (loc).  The datasets are processed to be ready 
# for use for data analytics. 

#------------------------------------------------------------------------------
# Import req'd modules, libraries and source constants
#------------------------------------------------------------------------------

import os
import glob
import pandas as pd
import re
from Python.modules.safepaths import *


#------------------------------------------------------------------------------
# define input and output paths.  assume network path is set
#------------------------------------------------------------------------------
# set_network_path("<UNC>") # Check out why I need to set the path each time.
data_folder = use_network_path()
data_path = os.path.join(data_folder, "data", "raw")
out_folder = os.path.join(data_folder, "data", "source", "python-test")

if not os.path.exists(out_folder):
    os.makedirs(out_folder)

#------------------------------------------------------------------------------
# get the most recently provisioned files
#------------------------------------------------------------------------------

files = glob.glob(os.path.join(data_path, "**", "*_no_errors.csv"), recursive=True)
files = [f for f in files if os.path.basename(f).startswith("locality_")]

# sort by date created
file_info = [(f, os.path.getmtime(f)) for f in files]
sorted_files = sorted(file_info, key=lambda x: x[1], reverse=True)

# and keep only the most recent file for each locality
unique_files = {}
for f, mtime in sorted_files:
    match = re.search(r'locality_(\d{3})', os.path.basename(f))
    if match:
        loc_code = match.group(1)
        if loc_code not in unique_files:
            unique_files[loc_code] = (f,mtime)
sorted_files = list(unique_files.values())

if len(sorted_files) < 1:
    raise ValueError("Not enough files matching the pattern 'no_errors.csv'")

#------------------------------------------------------------------------------
# Pre-process drive time files for each locality and merge as a single file
#------------------------------------------------------------------------------

file_path = sorted_files[0][0]
new_da_servicebc_df = pd.read_csv(file_path, dtype=str)

for i in range(1, len(sorted_files)):
    file_path = sorted_files[i][0]
    df = pd.read_csv(file_path, dtype=str)
    new_da_servicebc_df = pd.concat([new_da_servicebc_df, df], ignore_index=True)

address_sf_with_da = (
        new_da_servicebc_df
    .rename(columns=lambda x: re.sub(r'[^A-Za-z0-9_]+', '', x).lower())
    .query("tag == 'servicebc'")
    .rename(columns={"site_albers_x": "address_albers_x", "site_albers_y": "address_albers_y"})
    .assign(
            daid=lambda df: df["dissemination_block_id"].str[:8],
        drv_time_sec=lambda df: pd.to_numeric(df["drv_time_sec"]),
        drv_dist=lambda df: pd.to_numeric(df["drv_dist"]),
        address_albers_x=lambda df: pd.to_numeric(df["address_albers_x"]),
        address_albers_y=lambda df: pd.to_numeric(df["address_albers_y"]),
    )
)

# write output to csv
address_sf_with_da.to_csv(os.path.join(out_folder, f"drivetime-dist-with_da.csv"), index=False)