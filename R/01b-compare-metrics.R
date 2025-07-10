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

#------------------------------------------------------------------------------
# Load req'd libraries and source constants and other settings
#------------------------------------------------------------------------------

source("R/settings.R")

# Ensure output directory exists
output_dir <- file.path(VISUALS_OUT, "csd-drive-distance-plots")

#------------------------------------------------------------------------------
# Read data from source folder
#------------------------------------------------------------------------------

crosswalk <-
  read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

db_stats <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced_db_average_times_dist_all_locs.csv", col_types = cols(.default = "c"))) %>%
  clean_names()

csd_stats <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced_csd_average_times_dist_all_locs.csv", col_types = cols(.default = "c"))) %>%
  clean_names()

# ------------------------------------------------------------------------------
# create scatter plot to compare driving time to driving distance at the DB level

message("Generating Scatter Plot...")

outfile <- to_snake_case(glue("scatter plot mean_drv_dist_time by csd"))

plot.data <- db_stats %>%
  select(dbid, csd_name, csdid, drv_dist_mean, drv_time_sec_mean) %>%
  mutate(drv_dist_mean = as.numeric(drv_dist_mean),
         drv_time_sec_mean = as.numeric(drv_time_sec_mean),
         drv_time_min_mean = drv_time_sec_mean / 60)

scatter_plot <- plot.data %>%
  ggplot(aes(x = drv_dist_mean, y = drv_time_min_mean)) +
  geom_point(aes(color = csd_name)) +
  # plot a smooth line through each group of points
  geom_smooth(aes(color = csd_name), method = "lm", se = FALSE) +
  labs(title = "Driving Distance vs Time to Nearest Service BC Office",
       subtitle = "Dissemination block averages, by census subdivision",
       caption = "Source: BC Stats, 2025",
       x = "\nDriving Distance (km)",
       y = "Driving Time (minutes)\n",
       color = "") +
  SCATTER_PLOT_THEME +
  COLOR_THEME_D +
  facet_wrap( ~csd_name, scales = "free")

# Save the plot
ggsave(
  filename = glue("{output_dir}/{outfile}.png"),
  plot = scatter_plot,
  width = 8,
  height = 7,
  device = "png"
)

# clean up the environment
rm(list = ls())
gc()