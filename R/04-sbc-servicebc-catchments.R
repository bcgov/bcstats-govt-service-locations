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

# Simple test map showing BC Census Subdivision boundaries and Service BC locations
# as well as our pilot CSDs with the Service BC CSD methodology based catchments. 


# Load required libraries
library(tidyverse)
library(sf)
library(bcmaps)
library(glue)
library(janitor)
library(readxl)  # For read_excel function

# Source settings file to get necessary paths
source("R/settings.R")

# Get BC boundary as base map
message("Loading BC boundary...")
bc_outline <- bc_bound() %>%
  st_transform(crs = 3005)  # Transform to BC Albers projection

# Get BC Census Subdivision boundaries
message("Loading CSD boundaries...")
csd_boundaries <- census_subdivision() %>%
  clean_names() %>%
  st_transform(crs = 3005)  # Ensure consistent CRS

# Get centroids for labels
csd_centroids <- st_centroid(csd_shapefile) 
csd_centroids_nudged <- csd_centroids |> 
  mutate(
      geom = case_when(
        csd_name == 'Dawson Creek' ~ geom + c(-150000, 40000),
        csd_name == 'Langford' ~ geom + c(130000, 10000), # move east for langford
        TRUE ~ geom + c(0, 70000)  # move label north to not overlap 
      )
  ) %>%
  st_set_crs(st_crs(csd_centroids))

# Load Service BC locations
message("Loading Service BC locations...")
servicebc <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv"), 
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), remove = FALSE, crs = 3005)

# Define CSDs assigned to each facility according to centroid mapping
csd_facility_assignments  <- read_excel(
  glue("{RAW_DATA_FOLDER}/from_service_bc/Municipality(CSD)toSBCofficeMappying_updated (1).xlsx"),
  sheet = "CSDtoSBC"
)  %>% 
    mutate(CSD_ID = as.character(CSD_ID)) %>%
    filter(Nearest_office %in% c("Dawson Creek", "Victoria", "Smithers", "Kamloops")) 

# Join the assignment data to the CSD boundaries
csd_boundaries <- csd_boundaries %>%
  left_join(csd_facility_assignments, by = c("census_subdivision_id" = "CSD_ID"))

# Create map
message("Creating map...")
map_plot <- ggplot() +  # Add BC outline as base layer
  geom_sf(data = bc_outline,
          fill = "white",
          color = "black",
          size = 0.5) +
  # Add CSD boundaries - filled by facility assignment if available
  geom_sf(data = csd_boundaries, 
          aes(fill = Nearest_office),, 
          color = "darkgrey", 
          size = 0.2, 
          alpha = 0.7) +
  # Add Service BC locations as points
  geom_sf(data = servicebc, 
          shape = 23,  # Diamond shape
          fill = "yellow",
          color = "black",
          size = 3, 
          stroke = 1.1) +  # Add labels and theme
  # pilot labels
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size = 4, 
    fontface = 'bold'
  ) +
  labs(
    title = "Service BC Catchment Areas: CSD Method",
    subtitle = "With CSD Boundaries and Service BC Office Locations",
    fill = "Assigned to Facility",
    color = NULL
  ) +  
  # Set fill colors for assigned facilities
  scale_fill_viridis_d(
    option = "turbo",
    na.value = NA,#"white",  # CSDs without assignment get default color
    drop = FALSE
  ) +
  # Use coord_sf for proper projection display
  coord_sf(expand = FALSE) +  # No extra white space
  MAP_THEME +  theme(
    legend.position = "None",  
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove background grid lines
  )

# Print map to screen
message("Displaying map...")
print(map_plot)

# Save the map
maps_folder <- file.path(MAP_OUT, "complete_catchments")
if (!dir.exists(maps_folder)) {
  dir.create(maps_folder, recursive = TRUE)
}
ggsave(
  filename = "bc-csd-sbc-FROM-SERVICE-BC.png",
  plot = map_plot,
  path = maps_folder,
  width = 10,
  height = 8,
  dpi = 300
)

