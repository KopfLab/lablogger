



#' Plot device data logs
#'
#' @param duration_units specify a time unit (e.g. "mins") to indicate whether x axis should be displayed as a duration since the first data point within each experiment, if NULL x axis is displayed as regular date time.
#' @param date_breaks formate the datetime breaks if not plotting duration (i.e. is ignored if duration_units is provided)
#' @export
ll_plot_device_data_logs <- function(device_data_logs, filter = NULL, show_error_range = FALSE, duration_units = NULL, date_breaks = NULL, quiet = default(quiet)) {

  filter_quo <- enquo(filter)

  # plot df
  plot_df <- device_data_logs %>%
    # no missing values
    dplyr::filter(!is.na(data_value)) %>%
    # duration
    {
      if (!is.null(duration_units))
        group_by(., exp_id) %>% ll_calculate_duration(duration_units) %>% ungroup()
      else .
    } %>%
    # filter
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    # grouping and trace with units
    mutate(
      group = str_c(device_name, data_key, data_group),
      data_trace =
        ifelse(!is.na(data_units) & nchar(data_units) > 0, str_c(data_key, " [", data_units, "]"), data_key),
      group_trace =
        ifelse(!is.na(data_group), str_c(data_group, " ", data_trace), data_trace)
    )

  # info messages
  if (!quiet) {
    glue("Info: plotting {nrow(plot_df)} data log entries",
         "{if(!quo_is_null(filter_quo)) str_c(\" (filtered with '\", quo_text(filter_quo), \"')\") else ''}... ") %>%
      message()
  }

  # plot
  p <- plot_df %>%
    ggplot() +
    aes(y = data_value, color = data_trace, group = group)

  # error range
  if (show_error_range) {
    p <- p %+% aes(fill = data_trace) +
      geom_ribbon(
        data = function (df) filter(df, !is.na(data_sd)),
        mapping = aes(ymin = data_value - data_sd, ymax = data_value + data_sd, color = NULL),
        alpha = 0.3
      )
  }

  p <- p +
    geom_line() +
    facet_grid(data_trace ~ exp_id, scales = "free") +
    theme_bw() +
    labs(x = NULL, y = NULL)

  # data groups
  if (any(!is.na(plot_df$data_group))) {
    p <- p %+% aes(linetype = data_group)
  }

  # duration plot aesthetics
  if (!is.null(duration_units)) {
    p <- p %+% aes(x = duration) +
      labs(x = str_c("Duration [", duration_units, "]"))
  } else {
    if (!is.null(date_breaks))
      p <- p %+% aes(x = datetime) + scale_x_datetime(date_breaks = date_breaks)
    else
      p <- p %+% aes(x = datetime) + scale_x_datetime()
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "")
  }


  return(p)
}
