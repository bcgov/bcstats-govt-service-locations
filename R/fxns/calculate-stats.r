# Helper function to calculate summary statistics
calculate_drivetime_stats <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      drv_time_sec = as.numeric(drv_time_sec),
      drv_dist = as.numeric(drv_dist) 
    ) %>%
    summarise(
      mn_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
      mn_drv_dist = mean(drv_dist, na.rm = TRUE),
      qnt0_drv_time_sec = quantile(drv_time_sec, probs = 0, na.rm = TRUE),
      qnt1_drv_time_sec = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),
      qnt2_drv_time_sec = quantile(drv_time_sec, probs = 0.5, na.rm = TRUE),
      qnt3_drv_time_sec = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),
      qnt4_drv_time_sec = quantile(drv_time_sec, probs = 1, na.rm = TRUE),
      qnt0_drv_dist = quantile(drv_dist, probs = 0, na.rm = TRUE),
      qnt1_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),
      qnt2_drv_dist = quantile(drv_dist, probs = 0.5, na.rm = TRUE),
      qnt3_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),
      qnt4_drv_dist = quantile(drv_dist, probs = 1, na.rm = TRUE),
      var_drv_time_sec = var(drv_time_sec, na.rm = TRUE),
      var_drv_dist = var(drv_dist, na.rm = TRUE),
      # Ensure moments package is loaded or use moments::skewness etc.
      skw_drv_time_sec = skewness(drv_time_sec, na.rm = TRUE, type = 1), 
      skw_drv_dist = skewness(drv_dist, na.rm = TRUE, type = 1),
      kurt_drv_time_sec = kurtosis(drv_time_sec, na.rm = TRUE, type = 1),
      kurt_drv_dist = kurtosis(drv_dist, na.rm = TRUE, type = 1),
      n_address = n_distinct(fid), # Make sure 'fid' exists in 'data'
      .groups = "drop" # Automatically ungroups after summarise
    )
}
