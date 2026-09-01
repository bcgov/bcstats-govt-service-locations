# =============================================================================
# Required packages
# =============================================================================
import os
import re
from io import BytesIO
import time

import requests
import pandas as pd
import geopandas as gpd
import shapely.geometry #import Point
import matplotlib.pyplot as plt

import bcdata
import pycancensus as pc


# =============================================================================
# Helper functions
# =============================================================================


# -----------------------------------------------------------------------
# Function: ensure_dir
#
# Description: A quick file check to ensure folder directory path exists. 
#              Partial replacement for R safepaths library
# ------------------------------------------------------------------------

def ensure_dir(path):
    os.makedirs(path, exist_ok=True)



# -----------------------------------------------------------------------
# Function: download_bcdc_resource
#
# Description: Download a BC Data Catalogue resource via CKAN API and read it into a DataFrame.
#              Works well for CSV/Excel resources with a direct download URL.
#
# Inputs:
#   - package_id: dataset/package id
#   - resource_id: resource id
#
# Outputs:
#   - Returns a pandas dataframe
# ------------------------------------------------------------------------

def download_bcdc_resource(package_id: str, resource_id: str) -> pd.DataFrame:

    # API endpoint
    base = "https://catalogue.data.gov.bc.ca/api/3/action"

    # Pull package metadata to find the resource URL
    pkg = requests.get(f"{base}/package_show", params={"id": package_id}, timeout=120)
    pkg.raise_for_status()
    pkg_json = pkg.json()
    if not pkg_json.get("success"):
        raise RuntimeError(f"package_show failed for {package_id}: {pkg_json}")

    resources = pkg_json["result"].get("resources", [])
    res = next((r for r in resources if r.get("id") == resource_id), None)
    if res is None:
        raise ValueError(f"Resource {resource_id} not found in package {package_id}")

    url = res.get("url")
    if not url:
        raise ValueError(f"No URL found for resource {resource_id}")

    # Download
    r = requests.get(url, timeout=300)
    r.raise_for_status()

    # Try read by format / extension
    fmt = (res.get("format") or "").lower()
    #name = (res.get("name") or "").lower()
    if "csv" in fmt or url.lower().endswith(".csv"):
        return pd.read_csv(BytesIO(r.content))
    if "xlsx" in fmt or url.lower().endswith(".xlsx"):
        return pd.read_excel(BytesIO(r.content))
    if "xls" in fmt or url.lower().endswith(".xls"):
        return pd.read_excel(BytesIO(r.content))

    # Fallback: attempt CSV
    return pd.read_csv(BytesIO(r.content))



# -----------------------------------------------------------------------
# Function: download_bcdc_resource
#
# Description: Fetch WFS (web feature service) geodata via BCdata package and return a GeoDataFrame
#              bcdata.get_data supports returning a GeoDataFrame when as_gdf=True
#
# Inputs:
#   - layer_key: In theory can be a catalogue slug/package name or object name; some UUIDs may also work
#                Currently uses DB_LAYER_KEY and CSD_LAYER_KEY parameters set below
#   - query: optional query to format search
#   - crs: optional ESPG geodetic parameter (coordinate reference system)
#
# Outputs:
#   - Returns a geodataframe
# ------------------------------------------------------------------------

def get_wfs_geodata(layer_key: str, query: str = None, crs=None) -> gpd.GeoDataFrame:

    gdf = bcdata.get_data(layer_key, query=query, as_gdf=True)
    if not isinstance(gdf, gpd.GeoDataFrame):
        raise TypeError(f"Expected GeoDataFrame from bcdata.get_data(as_gdf=True), got {type(gdf)}")
    if crs is not None:
        gdf = gdf.to_crs(crs)
    return gdf

# -----------------------------------------------------------------------
# Function: assign_dbs
#
# Description: Assigns each dissemination block (DB) directly to the
# nearest Service BC facility location based on centroid distance. Mirrors the R assign_dbs() behavior.
#
# Inputs:
#   - db_shapefile: A GeoDataFrame object containing all DB geometries with `dbid` column
#   - assigned_facility: Data frame with columns `dbid` and `assigned`
#   - facility_locations: A GeoDataFrame object containing Service BC facility locations (EPSG:3005)
#   - verbose: Whether to print progress messages (default TRUE)
#
# Outputs:
#   - Returns an updated data frame with DB assignments
#
# Assumptions:
#   - db_shapefile must be A GeoDataFrame object with spatial geometries
#   - facility_locations must be A GeoDataFrame object with point geometries and a
#     'nearest_facility' column identifying each location
# ------------------------------------------------------------------------

def assign_dbs(db_gdf: gpd.GeoDataFrame,
               assigned_facility: pd.DataFrame,
               facility_locations: gpd.GeoDataFrame,
               verbose: bool = True) -> pd.DataFrame:

    # Join existing assignments
    db = db_gdf.merge(assigned_facility, on="dbid", how="left")

    unassigned = db[db["assigned"].isna()].copy()
    if unassigned.shape[0] == 0:
        if verbose:
            print("No unassigned DBs found.")
        return assigned_facility.copy()

    if facility_locations.shape[0] == 0:
        raise ValueError("No facility locations provided.")

    # Ensure unique facilities
    facilities = facility_locations.drop_duplicates(subset=["nearest_facility"]).copy()

    if verbose:
        print(f"Processing {len(unassigned)}:, unassigned DBs")
        print(f"Finding nearest facility among {len(facilities)}:, locations")

    # Ensure consistent CRS 
    if unassigned.crs != facilities.crs:
        if verbose:
            print("Transforming geometries to ensure consistent CRS...")
        facilities = facilities.to_crs(unassigned.crs)

    # Centroids
    if verbose:
        print("Calculating centroids for unassigned DBs...")
    unassigned_centroids = unassigned.copy()
    unassigned_centroids["geometry"] = unassigned_centroids.geometry.centroid

    # Nearest join (GeoPandas sjoin_nearest if available)
    try:
        joined = gpd.sjoin_nearest(
            unassigned_centroids[["dbid", "geometry"]],
            facilities[["nearest_facility", "geometry"]],
            how="left",
            distance_col="min_distance"
        )
        new_assignments = pd.DataFrame({
            "dbid": joined["dbid"].astype(str),
            "assigned": joined["nearest_facility"].astype(str),
            "assignment_method": "nearest_facility",
            "min_distance": joined["min_distance"].astype(float)
        })
    except Exception as e:
        # Fallback: brute force
        if verbose:
            print("sjoin_nearest unavailable; falling back to brute-force distance. Reason:", e)

        fac_geom = facilities.set_index("nearest_facility").geometry
        fac_names = fac_geom.index.to_list()

        rows = []
        for dbid, geom in zip(unassigned_centroids["dbid"], unassigned_centroids.geometry):
            dists = fac_geom.distance(geom)
            idx = int(dists.values.argmin())
            rows.append((str(dbid), str(fac_names[idx]), "nearest_facility", float(dists.iloc[idx])))

        new_assignments = pd.DataFrame(rows, columns=["dbid", "assigned", "assignment_method", "min_distance"])

    # Combine with existing assignments
    if assigned_facility is None or assigned_facility.empty:
        complete = new_assignments.copy()
    else:
        base = assigned_facility.copy()
        base["assignment_method"] = "drive_time"
        base["min_distance"] = float("nan")
        complete = pd.concat([base, new_assignments], ignore_index=True)

    return complete



# -----------------------------------------------------------------------
# Function: clean_names
#
# Description: Rough equivalent of janitor::clean_names()
#   Lowercase, replace non-alphanum with _, collapse repeats, strip edges
#
# Inputs:
#   - cols: A list of column names
#
# Outputs:
#   - Returns an updated list with cleaned column names
# ------------------------------------------------------------------------

def clean_names(cols):

    out = []
    for c in cols:
        c2 = str(c).strip().lower()
        c2 = re.sub(r"[^a-z0-9]+", "_", c2)
        c2 = re.sub(r"_+", "_", c2).strip("_")
        out.append(c2)
    return out


# -----------------------------------------------------------------------
# Function: weighted_median
#
# Description: Compute weighted median of values with non-negative weights.
#              Used to calculated weighted_ages
#
# Inputs:
#   - values: The input on which to calculate weights
#   - weights: the weighting parameters
#
# Outputs:
#   - Returns a decimal value
# ------------------------------------------------------------------------

def weighted_median(values, weights):
    """
    Compute weighted median of values with non-negative weights.
    """
    s = pd.DataFrame({"v": values, "w": weights}).dropna()
    s = s[s["w"] > 0].sort_values("v")
    if s.empty:
        return float("nan")
    csum = s["w"].cumsum()
    cutoff = s["w"].sum() / 2.0
    return float(s.loc[csum >= cutoff, "v"].iloc[0])



# -----------------------------------------------------------------------
# Function: get_db_population_bc
#
# Description: Download Dissemination Block population data for all of BC using the pycancensus package.
#              Uses chunking.  Function is a fallback to the initial request in the script below, which uses no chunking.
#                   - First tries DB by Census subdivision.
#                   - If a CSD DB request fails, falls back to DB by Dissemination area within that CSD.
#
# Inputs:
#   - dataset: The input on which to calculate weights
#   - pr: the area to use. Typically 59 which is BC. Set in the parameters section
#   - vector: Additional parameters to control what level of census data to pull. 
#             Keeping this greyed out as is seems to crash the api with too large of a request.
#   - sleep_s: time ti sleep between chunks
#   - fallback_sleep_s: time to sleep between chunks during fallback
#   - quiet: show terminal output from api or not
#   - use_cache: cahce data to reduce multiple api request size 
#
# Outputs:
#   - out: returns a pandas dataframe with the cnesus population data by DB
#   - failures: returns a list of CSDs that were unsuccessful and went to the fallback method for DAs 
#
# Assumptions:
#   - assumes that a cancensus api key is available on the path
#   - if fallback to DA chunking triggers, assumes that no duplication in DBs will occur in dataset.
#   - If weirdness if observed can add a uniqueness check later.
# ------------------------------------------------------------------------

def get_db_population_bc(
    dataset,
    pr,
    vector,
    sleep_s=0.2,
    fallback_sleep_s=0.2,
    quiet=False,
    use_cache=True
):
    """
    Download DB-level population for all of BC by chunking requests.
    - First tries DB by CSD (municipality).
    - If a CSD DB request fails, falls back to DB by DA within that CSD.
    Returns a concatenated DataFrame.
    """

    #Find BC CSD region identifiers
    regions_df = pc.get_census(
                dataset=dataset,
                regions={"PR": pr},
                level="CSD",                 
                geo_format=None,
                quiet=quiet,
                use_cache=use_cache
            )
    bc_csds = regions_df['GeoUID']
    print("census subdivisions", bc_csds)
    
    chunks = []
    failures = []

    #Go through chunks to get DBs in each CSD for BC
    for i, csd in enumerate(bc_csds, start=1):
        if not quiet:
            print(f"[{i}/{len(bc_csds)}] CSD={csd} -> DB")

        try:
            #Try DB directly for the CSD
            print("Trying: ",csd)
            df = pc.get_census(
                dataset=dataset,
                regions={"CSD": csd},
                #vectors=[vector],
                level="DB",                 
                geo_format=None,
                quiet=quiet,
                use_cache=use_cache
            )
            if len(df):
                chunks.append(df)

        except Exception as e:
            #Fall back: get DAs for that CSD, then DB per DA
            failures.append((csd, str(e)))
            if not quiet:
                print(f"  -> {csd} DB failed ({e}); falling back to DA->DB")

            # Get all DAs within this CSD
            da_df = pc.get_census(
                dataset=dataset,
                regions={"CSD": csd},
                #vectors=[vector],           
                level="DA",
                geo_format=None,
                quiet=quiet,
                use_cache=use_cache
            )

            # Get GeoUID for DAs in CSD 
            da_ids = da_df["GeoUID"].astype(str).tolist()
            print(da_ids)
            
            #now get DB for DAs in CSD
            for da in da_ids:
                try:
                    db_df = pc.get_census(
                        dataset=dataset,
                        regions={"DA": da},
                        #vectors=[vector],
                        level="DB",
                        geo_format=None,
                        quiet=True,
                        use_cache=use_cache
                    )
                    if len(db_df):
                        chunks.append(db_df)
                except Exception as e2:
                    if not quiet:
                        print(f"    -> DA {da} DB failed: {e2}")
                time.sleep(fallback_sleep_s)

        time.sleep(sleep_s)

    out = pd.concat(chunks, ignore_index=True) if chunks else pd.DataFrame()
    return out, failures



    