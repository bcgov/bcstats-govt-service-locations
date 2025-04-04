# function to process each locality
preprocess_locs <- function(fl, loc, data_folder, output_folder, reqd_cols, facility_tag) {

  data <- read_csv(fl, col_types = cols(.default = "c")) %>%
    clean_names()

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue("error processing locality {loc}: not all required columns are found in data"))
    return(NULL)
  }

  data <- data%>%
    filter(tag == FACILITY_TAG) %>% 
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y) %>%
    mutate(daid = str_sub(dissemination_block_id, 1, 8),
           drv_time_sec = as.numeric(drv_time_sec),
           drv_dist = as.numeric(drv_dist),
           address_albers_x = as.numeric(address_albers_x),
           address_albers_y = as.numeric(address_albers_y))


  # write output to a csv file
  output_filename <- glue("{output_folder}/address_with_da_locality_{loc}.csv") # could be passed to function

  # Check if the file already exists and warn if overwriting
  if (file.exists(output_filename)) {
  message(glue::glue("Overwriting existing file for locality {loc}: {output_filename}")) # nolint
  }
  } else {
    message(glue::glue("Creating new file for locality {loc}: {output_filename}")) # nolint
  }
}
  data %>%
    write_csv(output_filename)

  return(data)

}
