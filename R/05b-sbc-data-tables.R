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


drivetime_data_reduced %>%
group_by(assigned) %>%
  summarize(n_under_5 = sum(drv_dist < 5.0, na.rm=TRUE), 
            n_5_to_20 = sum(drv_dist >= 5.0 & drv_dist < 20.0, na.rm=TRUE),
            n_20_plus = sum(drv_dist >= 20.0, na.rm=TRUE))

drivetime_data_reduced |>
  distinct(assigned, assignment_method, csdid, dbid) |>
  # expand drivetime data to include all years of interest for each row
    expand_grid(
        tibble(year = rep(unique(db_projections_transformed$year)))
    ) |>
  left_join(
    db_projections_transformed %>% 
    filter(gender=='T') %>% 
    select(dbid, year, age, population, total, area_sq_km), 
    by = c("dbid", 'year')
    ) |> 
  filter(!is.na(csdid)) -> df

  df %>%
  group_by(assigned, year) %>% 
  summarize(pop = sum(population, na.rm=TRUE)) %>%
  pivot_wider(names_from = year, values_from = pop, values_fill = 0) 

  df %>% mutate(age_grp = case_when(
    age < 19 ~ "0-19",
    age >= 19 & age < 65 ~ "19-64",
    age >= 65 ~ "65+"
  )) %>%
  filter(year == 2025) %>%
  group_by(assigned, age_grp) %>% 
  summarize(pop = sum(population, na.rm=TRUE)) %>%
  pivot_wider(names_from = age_grp, values_from = pop, values_fill = 0)

