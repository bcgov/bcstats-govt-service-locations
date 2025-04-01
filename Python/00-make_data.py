import os
import glob
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import re
from pathlib import Path

# Assuming configuration.R sets a variable called network_path
# You'll need to replace this with the actual logic from your R configuration
# For now, I'll define a placeholder function
def use_network_path():
    """Placeholder for R's safepaths::use_network_path()"""
    # Replace with your actual network path retrieval logic
    return "G://Operations/Data Science and Analytics/2025 Government Service Locations" # example, replace with your actual path

#------------------------------------------------------------------------------
# geodata team creates drive times files for service bc
# with unique id in place of civic address
# zik files are just zip files, so read_csv can handle them directly.
# currently only service bc locations are needed
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: Locality 420
#------------------------------------------------------------------------------

# data for service bc with unique id
data_folder = use_network_path()
data_path = os.path.join(data_folder, "data", "raw")

# get the most recent file.  This could be used to process all of them
files = glob.glob(os.path.join(data_path, "**", "*_no_errors.csv"), recursive=True)
file_info = [(f, os.path.getmtime(f)) for f in files]
sorted_files = sorted(file_info, key=lambda x: x[1], reverse=True)
if len(sorted_files) > 1:
    file_path = sorted_files[1][0]
else:
    raise ValueError("Not enough files matching the pattern 'no_errors.csv'")

# get the locality number from the file path
loc=227
#loc_match = re.search(rf"{re.escape(data_path)}(.*)/locality_([0-9][0-9][0-9])(.*)", file_path)
#if loc_match:
#    loc = loc_match.group(2)
#else:
#    raise ValueError(f"Could not extract locality from file path: {file_path}")

out_folder = os.path.join(data_folder, "data", "source", f"locality_{loc}")

if not os.path.exists(out_folder):
    os.makedirs(out_folder)

new_da_servicebc_df = pd.read_csv(file_path, dtype=str)

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

# drop the address coordinates
address_sf_with_da.to_csv(os.path.join(out_folder, f"python_test_address_with_da_loc_{loc}.csv"), index=False)