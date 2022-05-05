# identify outliers
identify_data_outliers <- function(device_data_logs) {
  if (nrow(device_data_logs) == 0) return(device_data_logs)

  identify_outliers <- function(x) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    H <- 1.5 * IQR(x, na.rm = TRUE)
    !between(x, qnt[1] - H, qnt[2] + H)
  }

  device_data_logs %>%
    group_by(exp_id, device_name, data_key) %>%
    mutate(
      outlier = identify_outliers(data_value)
    ) %>%
    ungroup()
}

#' Generate data log summary
#' @export
ll_summarize_data_logs <- function(data_logs, exclude_outliers = FALSE, slope_denom_units = "hour") {

  # column names
  drift_col <- str_c("lin_drift_per_", slope_denom_units)
  drift_se_col <- str_c(drift_col, "_se")
  drift_pval_col <- str_c(drift_col, "_pval")

  # calculate summary
  data_logs %>%
    ll_calculate_duration(slope_denom_units) %>%
    {
      if(exclude_outliers) filter(identify_data_outliers(.), !outlier)
      else .
    } %>%
    group_by(exp_id, device_name, data_key, data_units) %>%
    summarize(
      n = dplyr::n(),
      mean = mean(data_value, na.rm = TRUE),
      sd = sd(data_value, na.rm = TRUE),
      fit = list(lm(data_value ~ duration))
      # not that informative
      #cor_obj = list(cor.test(data_value, duration)),
      #time_cor = map_dbl(cor_obj, ~.x$estimate),
    ) %>%
    mutate(
      est = map(fit, broom::tidy),
      !!drift_col := map_dbl(est, ~filter(.x, term == "duration")$estimate),
      !!drift_se_col := map_dbl(est, ~filter(.x, term == "duration")$std.error),
      drift_R2 = map_dbl(fit, ~glance(.x)$r.squared)
      # same as time cor pval
      #!!drift_pval_col := map_dbl(est, ~filter(.x, term == "duration")$p.value)
    ) %>%
    ungroup() %>%
    arrange(exp_id, data_key, device_name) %>%
    select(-fit, -est, -device_name)
}
