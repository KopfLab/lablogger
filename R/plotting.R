# add data traces, and groups for plotting
prepare_data_for_plotting <- function(device_data_logs) {
  if (nrow(device_data_logs) == 0) return(device_data_logs)
  device_data_logs %>%
  # grouping and trace with units
  mutate(
    group = paste(exp_id, device_name, data_key, sep = "_"),
    data_trace =
      ifelse(!is.na(data_units) & nchar(data_units) > 0, sprintf("%s [%s]", data_key, data_units), data_key)
  )
}

#' Plot device data logs
#'
#' This function helps to visualize data logs retrieved with \code{\link{ll_get_device_data_logs}} or read in from downloaded data logs.
#'
#' @param device_data_logs data logs
#' @param duration_breaks specify a duration unit or interval (e.g. "hours" or "20 mins") to indicate whether x axis should be displayed as a duration since the first data point within each experiment. If interval is specified, this will be used for axis intervals, if just time unit the axis will have regular auto-ticks. If duration_breaks = NULL (the default), the x axis is displayed as regular date time (set date_breaks instead).
#' @param date_breaks formate the datetime breaks if not plotting duration (i.e. is ignored if duration_units is provided)
#' @param include_device_info whether to include the device info (name and index) in the data trace label
#' @param overlay_experiments whether to overlay the experiments instead of paneling (default is panels). This usually makes most sense if x axis is a duration (set via duration units)
#' @family data logs functions
#' @export
ll_plot_device_data_logs <- function(device_data_logs, filter = NULL, show_error_range = FALSE, exclude_outliers = FALSE, duration_breaks = NULL, date_breaks = NULL, include_device_info = FALSE, overlay_experiments = FALSE, quiet = default(quiet)) {

  filter_quo <- rlang::enquo(filter)

  # duration
  if(!is.null(duration_breaks)) {
    duration_matches <- stringr::str_match(duration_breaks, "(\\d+\\.?\\d*)?\\s?([a-zA-Z]+)")
    duration_interval <- as.numeric(duration_matches[1,2])
    duration_units <- duration_matches[1,3]
  }

  # plot df
  plot_df <- device_data_logs %>%
    # no missing values
    dplyr::filter(!is.na(data_value)) %>%
    # duration
    {
      if (!is.null(duration_breaks))
        group_by(., exp_id) %>% ll_calculate_duration(duration_units) %>% ungroup()
      else .
    } %>%
    # filter
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    # grouping and trace with units
    prepare_data_for_plotting()

  # outliers
  if (exclude_outliers) {
    plot_df <- plot_df %>% identify_data_outliers() %>%
      mutate(data_value = ifelse(outlier, NA_real_, data_value))
  }

  # info messages
  if (!quiet) {
    glue("Info: plotting {nrow(plot_df)} data log entries",
         "{if(!quo_is_null(filter_quo)) str_c(\" (filtered with '\", quo_text(filter_quo), \"')\") else ''}... ") %>%
      message()
  }

  # plot
  p <- ggplot(plot_df) + aes(y = data_value)

  if (include_device_info) {
    p <- p %+% aes(color = sprintf("%s (%d): %s", device_name, data_idx, data_trace), group = group)
  } else {
    p <- p %+% aes(color = data_trace, group = group)
  }

  # error range
  if (show_error_range) {
    if (include_device_info) {
      p <- p + aes(fill = sprintf("%s (%d): %s", device_name, data_idx, data_trace))
    } else {
      p <- p + aes(fill = data_trace)
    }
    p <- p +
      geom_ribbon(
        data = function (df) dplyr::filter(df, !is.na(data_sd)),
        mapping = aes(ymin = data_value - data_sd, ymax = data_value + data_sd, color = NULL),
        alpha = 0.3
      )
  }

  p <- p + geom_line() + theme_bw()

  # experiments overlay
  if (overlay_experiments) {
    # traces overlay
    p <- p %+% aes(linetype = exp_id) +
      facet_grid(data_trace ~ ., scales = "free")
  } else {
    # panel
    p <- p +
      facet_grid(data_trace ~ exp_id, scales = "free")
  }

  # duration plot aesthetics
  if (!is.null(duration_breaks)) {
    p <- p %+% aes(x = duration) +
      labs(x = str_c("Duration [", duration_units, "]"))
    if (!is.na(duration_interval))
      p <- p %+% ggplot2::scale_x_continuous(breaks = seq(0, max(plot_df$duration), by = duration_interval))
  } else {
    if (!is.null(date_breaks))
      p <- p %+% aes(x = datetime) + scale_x_datetime(date_breaks = date_breaks)
    else
      p <- p %+% aes(x = datetime) + scale_x_datetime()
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = NULL)
  }

  p <- p + labs(y = NULL, color = NULL, fill = NULL, linetype = NULL)

  return(p)
}
