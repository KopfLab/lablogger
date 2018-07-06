


#' Plot device data logs
#'
#' @export
plot_device_data_logs <- function(device_data_logs) {
  device_data_logs %>%
    mutate(
      group = str_c(device_name, data_key, data_key_prefix),
      trace = ifelse(!is.na(data_units) & nchar(data_units) > 0, str_c(data_key, " [", data_units, "]"), data_key)
    ) %>%
    ggplot() +
    aes(x = datetime, y = data_value, color = trace, group = group) +
    geom_line() +
    facet_grid(trace ~ exp_id, scales = "free") +
    scale_x_datetime() +
    theme_bw() +
    labs(x = NULL, y = NULL)

}
