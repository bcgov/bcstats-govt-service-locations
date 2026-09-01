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



# -----------------------------------------------------------------------
# Function: summarize_accessibility_from_csv
#
# Description: Summarize population accessibility metrics from the DB-level
#              centroid assignment output CSV.
#              Calculates:
#                   - the share of projected population living within a
#                     specified centroid-distance threshold from the assigned
#                     SBC facility
#                   - the population-weighted average centroid distance to
#                     the assigned SBC facility
#
# Inputs:
#   - csv1_path: path to the DB centroid assignment CSV output.
#                Expected file is usually db_centroid_assignments.csv or
#                db_centroid_assignments_updated.csv.
#   - population_year: projection year to use for the population denominator.
#                      Must correspond to an existing population column such as
#                      pop_2025, pop_2030, or pop_2035.
#   - distance_threshold_km: distance threshold in kilometres.
#                            Default is 15 km. The function converts this to
#                            metres before comparing against centroid_distance_m.
#
# Outputs:
#   - Returns a dictionary containing:
#       - population_year
#       - distance_threshold_km
#       - total_population
#       - population_within_threshold
#       - population_within_threshold_pct
#       - weighted_avg_distance_m
#       - weighted_avg_distance_km
#
# Assumptions:
#   - csv1_path contains one row per DB assignment.
#   - The CSV contains centroid_distance_m and the selected population column.
#   - centroid_distance_m is measured in metres.
#   - The distance is straight-line centroid distance, not road network distance
#     or actual travel distance.
#   - Rows with missing centroid_distance_m are excluded from both the numerator
#     and denominator.
#   - Population values that cannot be parsed are treated as zero.
# ------------------------------------------------------------------------

def summarize_accessibility_from_csv(
    csv1_path: str,
    population_year: int,
    distance_threshold_km: float = 15
) -> dict:
    """
    Calculate population share within a centroid-distance threshold and
    weighted average centroid distance.

    Inputs:
      - csv1_path: path to db_centroid_assignments.csv
      - population_year: projection year, e.g. 2025, 2030, 2035
      - distance_threshold_km: threshold in kilometres

    Returns:
      - total_population
      - population_within_threshold
      - population_within_threshold_pct
      - weighted_avg_distance_m
      - weighted_avg_distance_km
    """
    df = pd.read_csv(csv1_path)

    pop_col = f"pop_{population_year}"
    required_cols = {pop_col, "centroid_distance_m"}
    missing = required_cols - set(df.columns)

    if missing:
        raise ValueError(f"Missing required columns: {', '.join(sorted(missing))}")

    df[pop_col] = pd.to_numeric(df[pop_col], errors="coerce").fillna(0)
    df["centroid_distance_m"] = pd.to_numeric(df["centroid_distance_m"], errors="coerce")

    valid = df[df["centroid_distance_m"].notna()].copy()

    total_population = valid[pop_col].sum()

    if total_population == 0:
        raise ValueError(f"Total population is zero for {pop_col}; cannot calculate metrics.")

    threshold_m = distance_threshold_km * 1000

    population_within_threshold = valid.loc[
        valid["centroid_distance_m"] <= threshold_m,
        pop_col
    ].sum()

    population_within_threshold_pct = (
        population_within_threshold / total_population * 100
    )

    weighted_avg_distance_m = (
        valid[pop_col] * valid["centroid_distance_m"]
    ).sum() / total_population

    return {
        "population_year": population_year,
        "distance_threshold_km": distance_threshold_km,
        "total_population": round(total_population, 1),
        "population_within_threshold": round(population_within_threshold, 1),
        "population_within_threshold_pct": round(population_within_threshold_pct, 2),
        "weighted_avg_distance_m": round(weighted_avg_distance_m, 1),
        "weighted_avg_distance_km": round(weighted_avg_distance_m / 1000, 2),
    }



# -----------------------------------------------------------------------
# Function: summarize_accessibility_for_scenarios
#
# Description: Summarize population within a centroid-distance threshold for
#              each scenario output inside one updated main_updated()
#              experiment folder.
#
#              Example folder structure:
#
#              OUTPUT_DIRECTORY/
#                  add_13_locations_abbotsford_and_spallumcheen_and_ca_11385c55/
#                      single_abbotsford/
#                          db_centroid_assignments_updated.csv
#                      single_spallumcheen/
#                          db_centroid_assignments_updated.csv
#                      ...
#                      all_new_locations/
#                          db_centroid_assignments_updated.csv
#
# Inputs:
#   - output_root:
#         Root output folder, usually OUTPUT_DIRECTORY.
#
#   - experiment_dir:
#         Optional specific experiment folder.
#         If provided, function reads scenario subfolders from this folder.
#         Example:
#             Path(OUTPUT_DIRECTORY) / "add_13_locations_abbotsford_and_spallumcheen_and_ca_11385c55"
#
#   - scenario_results:
#         Optional list returned by main_updated().
#         If provided, the function reads csv1_path from scenario_results.
#
#   - population_year:
#         Projection year to use. Must match a column such as pop_2025.
#
#   - distance_threshold_km:
#         Centroid-distance threshold in kilometres. Default = 15.
#
# Outputs:
#   - Returns one summary row per scenario:
#       - experiment_name
#       - scenario_name
#       - new_location_name
#       - population_year
#       - distance_threshold_km
#       - total_population
#       - population_within_15km
#       - population_within_15km_pct
#       - csv1_path
#
# Assumptions:
#   - Scenario file name is db_centroid_assignments_updated.csv.
#   - Distance field is centroid_distance_m.
#   - Population field is pop_YYYY.
#   - Distance is straight-line centroid distance, not road-network distance.
# ------------------------------------------------------------------------

def summarize_accessibility_for_scenarios(
    output_root: str = None,
    experiment_dir: str = None,
    scenario_results: list = None,
    population_year: int = 2025,
    distance_threshold_km: float = 15
) -> pd.DataFrame:
    """
    Return population within the distance threshold for each new-location
    scenario and the all_new_locations scenario.
    """

    # ---------------------------------------------------------------------
    # Helper: local clean_names fallback
    # ---------------------------------------------------------------------
    def clean_names_local(cols):
        """
        Use project clean_names() if available; otherwise use local fallback.
        """
        if "clean_names" in globals():
            return clean_names(cols)

        import re

        out = []
        for c in cols:
            c2 = str(c).strip().lower()
            c2 = re.sub(r"[^a-z0-9]+", "_", c2)
            c2 = re.sub(r"_+", "_", c2).strip("_")
            out.append(c2)

        return out

    # ---------------------------------------------------------------------
    # Helper: find latest experiment folder
    # ---------------------------------------------------------------------
    def find_latest_experiment_dir(output_root_path: Path) -> Path:
        """
        Find the most recently modified experiment folder that contains
        scenario-level db_centroid_assignments_updated.csv files.
        """

        candidate_dirs = []

        for folder in output_root_path.iterdir():
            if not folder.is_dir():
                continue

            scenario_csvs = list(
                folder.rglob("db_centroid_assignments_updated.csv")
            )

            if scenario_csvs:
                candidate_dirs.append(folder)

        if not candidate_dirs:
            raise ValueError(
                "No experiment folders found under output_root. "
                "Expected subfolders containing db_centroid_assignments_updated.csv."
            )

        return max(candidate_dirs, key=lambda p: p.stat().st_mtime)

    # ---------------------------------------------------------------------
    # Helper: convert scenario folder name to readable new-location name
    # ---------------------------------------------------------------------
    def scenario_to_location_name(scenario_name: str) -> str:
        """
        Convert scenario_name to readable location name.
        """

        scenario_name = str(scenario_name)

        if scenario_name == "all_new_locations":
            return "All new locations"

        if scenario_name.startswith("single_"):
            location = scenario_name.replace("single_", "", 1)
        else:
            location = scenario_name

        return location.replace("_", " ").title()

    # ---------------------------------------------------------------------
    # Helper: summarize one DB assignment CSV
    # ---------------------------------------------------------------------
    def summarize_one_db_assignment_csv(
        csv1_path: Path,
        scenario_name: str,
        experiment_name: str
    ) -> dict:
        """
        Summarize one db_centroid_assignments_updated.csv file.
        """

        db = pd.read_csv(csv1_path)
        db.columns = clean_names_local(db.columns)

        pop_col = f"pop_{population_year}"

        required_cols = {
            pop_col,
            "centroid_distance_m"
        }

        missing_cols = required_cols - set(db.columns)

        if missing_cols:
            raise ValueError(
                f"{csv1_path} missing required columns after clean_names(): "
                f"{sorted(missing_cols)}. Available columns: {list(db.columns)}"
            )

        db[pop_col] = pd.to_numeric(db[pop_col], errors="coerce").fillna(0)
        db["centroid_distance_m"] = pd.to_numeric(
            db["centroid_distance_m"],
            errors="coerce"
        )

        distance_threshold_m = distance_threshold_km * 1000

        # Change: Exclude rows with missing distance from within-threshold count.
        valid_distance = db["centroid_distance_m"].notna()

        total_population = db[pop_col].sum()

        population_within_threshold = db.loc[
            valid_distance
            & (db["centroid_distance_m"] <= distance_threshold_m),
            pop_col
        ].sum()

        population_within_threshold_pct = (
            population_within_threshold / total_population * 100
            if total_population > 0
            else float("nan")
        )

        return {
            "experiment_name": experiment_name,
            "scenario_name": scenario_name,
            "new_location_name": scenario_to_location_name(scenario_name),
            "population_year": population_year,
            "distance_threshold_km": distance_threshold_km,
            "total_population": total_population,
            f"population_within_{distance_threshold_km:g}km": population_within_threshold,
            f"population_within_{distance_threshold_km:g}km_pct": population_within_threshold_pct,
            "csv1_path": str(csv1_path)
        }

    # ---------------------------------------------------------------------
    # Build scenario item list
    # ---------------------------------------------------------------------
    scenario_items = []

    # Option 1: Use scenario_results returned by main_updated()
    if scenario_results:
        for result in scenario_results:
            if "csv1_path" not in result:
                raise ValueError(
                    "Each scenario result must contain csv1_path. "
                    f"Problem result: {result}"
                )

            csv1_path = Path(result["csv1_path"])

            if not csv1_path.exists():
                raise FileNotFoundError(f"Scenario CSV not found: {csv1_path}")

            scenario_output_dir = Path(
                result.get("scenario_output_dir", csv1_path.parent)
            )

            experiment_name = (
                Path(result.get("combination_output_dir")).name
                if result.get("combination_output_dir")
                else scenario_output_dir.parent.name
            )

            scenario_items.append({
                "experiment_name": experiment_name,
                "scenario_name": result.get("scenario_name", scenario_output_dir.name),
                "csv1_path": csv1_path
            })

    # Option 2: Scan experiment folder
    else:
        if experiment_dir is not None:
            experiment_dir_path = Path(experiment_dir)

        else:
            if output_root is None:
                raise ValueError(
                    "Provide either scenario_results, experiment_dir, or output_root."
                )

            output_root_path = Path(output_root)

            if not output_root_path.exists():
                raise FileNotFoundError(f"output_root not found: {output_root}")

            # Change: Automatically use latest experiment folder if experiment_dir is not provided.
            experiment_dir_path = find_latest_experiment_dir(output_root_path)

        if not experiment_dir_path.exists():
            raise FileNotFoundError(f"experiment_dir not found: {experiment_dir_path}")

        scenario_csvs = sorted(
            experiment_dir_path.rglob("db_centroid_assignments_updated.csv")
        )

        if not scenario_csvs:
            raise ValueError(
                f"No db_centroid_assignments_updated.csv files found under: "
                f"{experiment_dir_path}"
            )

        for csv1_path in scenario_csvs:
            scenario_dir = csv1_path.parent

            scenario_items.append({
                "experiment_name": experiment_dir_path.name,
                "scenario_name": scenario_dir.name,
                "csv1_path": csv1_path
            })

    # ---------------------------------------------------------------------
    # Summarize scenarios
    # ---------------------------------------------------------------------
    rows = []

    for item in scenario_items:
        rows.append(
            summarize_one_db_assignment_csv(
                csv1_path=item["csv1_path"],
                scenario_name=item["scenario_name"],
                experiment_name=item["experiment_name"]
            )
        )

    result = pd.DataFrame(rows)

    # ---------------------------------------------------------------------
    # Round numeric outputs
    # ---------------------------------------------------------------------
    population_within_col = f"population_within_{distance_threshold_km:g}km"
    population_pct_col = f"population_within_{distance_threshold_km:g}km_pct"

    result["total_population"] = pd.to_numeric(
        result["total_population"],
        errors="coerce"
    ).round(1)

    result[population_within_col] = pd.to_numeric(
        result[population_within_col],
        errors="coerce"
    ).round(1)

    result[population_pct_col] = pd.to_numeric(
        result[population_pct_col],
        errors="coerce"
    ).round(2)

    # ---------------------------------------------------------------------
    # Sort: individual scenarios first, all_new_locations last
    # ---------------------------------------------------------------------
    result["scenario_sort_order"] = result["scenario_name"].apply(
        lambda x: 999 if str(x) == "all_new_locations" else 1
    )

    result = (
        result
        .sort_values(
            ["scenario_sort_order", population_within_col, population_pct_col, "scenario_name"],
            ascending=[True, False, False, True]
        )
        .drop(columns=["scenario_sort_order"])
        .reset_index(drop=True)
    )

    return result