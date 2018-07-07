# devices ====

#' Retrieve devices
#' @param group_id devices from which group to fetch
#' @return devices
#' @export
c3_get_devices <- function(group_id = default(group_id), filter = NULL, in_use_only = TRUE, con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving {if(in_use_only) 'in-use' else 'all'} devices ",
         "for group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "...") %>% message(appendLF = FALSE)
  }
  df <- tbl(con, "devices") %>%
    dplyr::filter(group_id == group_id_value, in_use | !in_use_only) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    collect()
  if (!quiet) glue("found {nrow(df)}.") %>% message()
  return(df)
}

#' Get device id
#'
#' Convenience shortcut function to get the database integer device identifier for one or multiple devices.
#' @export
c3_get_device_ids <- function(device_names, group_id = default(group_id), quiet = default(quiet)) {
  device_names_filter <- unique(device_names)
  devices <- c3_get_devices(group_id = group_id, filter = !!quo(device_name %in% c(!!!device_names_filter)), quiet = quiet)
  device_name_to_id_map <- setNames(devices$device_id, devices$device_name)
  if (any(missing <- !device_names %in% names(device_name_to_id_map))) {
    glue("the folowing device_name(s) do not exist for group '{group_id}' and can therefore not be mapped to id(s): {collapse(device_names[missing], sep = ', ')}") %>%
      stop(call. = FALSE)
  }
  return(device_name_to_id_map[device_names])
}

# exeperiments =====

#' Retrieve experiments
#' @param group_id experiments from which group to fetch
#' @return experiments
#' @export
c3_get_experiments <- function(group_id = default(group_id), filter = NULL, con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving experiments for group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "...") %>% message(appendLF = FALSE)
  }
  df <- tbl(con, "experiments") %>%
    dplyr::filter(group_id == group_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    collect()
  if (!quiet) glue("found {nrow(df)}.") %>% message()
  return(df)
}

#' Retrieve experiment devices
#'
#' Returns experiment-device links (only for active/in-use devices) joined with experiment and devices tables so filter conditions can be applied on the devices as well.
#'
#' @inheritParams c3_get_experiments
#' @return experiments_devices
#' @export
c3_get_experiment_devices <- function(
    group_id = default(group_id), filter = NULL,
    select = c(exp_id, exp_name, recording, device_id, device_name, particle_id, data_key_prefix, data_idx),
    con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving experiment-device links for active devices in group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}... ") %>%
      message(appendLF = FALSE)
  }

  exp_devices <- tbl(con, "experiment_device_data") %>%
    left_join(tbl(con, "devices"), by = "device_id") %>%
    left_join(tbl(con, "experiments"), by = c("exp_id", "group_id")) %>%
    dplyr::filter(group_id == group_id_value, in_use) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    dplyr::select(!!select_quo, device_name, data_idx) %>%
    collect() %>%
    # arrange
    arrange(device_name, data_idx) %>%
    dplyr::select(!!select_quo)


  if (!quiet) glue("found {nrow(exp_devices)} links. ") %>% message()
  return(exp_devices)
}

# logs ====

#' Retrieve state logs
#'
#' Returns state logs joined with devices table so filter conditions can be applied on the devices as well. Sorted by descending order (i.e. latest record first).
#'
#' @param filter what filter conditions to apply, if any (forwarded to \link[dplyr]{filter})
#' @param select what columns to select (forwarded to \link[select]{select}), by default a selection of the most commonly used columns
#' @param max_rows if provided, only selects the indicated number of rows (more efficient this way than part of the filter)
#' @param convert_to_TZ if provided, converts the log_datetime to the provided timezone (by default the local one stored in \code{Sys.getenv("TZ")}). If NULL, will keep it as UTC.
#' @return device state logs
#' @export
c3_get_device_state_logs <- function(
  group_id = default(group_id), filter = NULL,
  select = c(device_id, device_name, device_state_log_id, log_datetime, log_type, log_message, starts_with("state"), notes),
  max_rows = NULL,
  convert_to_TZ = Sys.getenv("TZ"),
  con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving device state logs for devices in group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "{if(!is.null(max_rows)) str_c(\", limited to \", max_rows, \" rows max\") else ''}... ") %>%
    message(appendLF = FALSE)
  }

  logs <- tbl(con, "device_state_logs") %>%
    left_join(tbl(con, "devices"), by = "device_id") %>%
    arrange(desc(device_state_log_id)) %>%
    dplyr::filter(group_id == group_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    {
      if (!is.null(max_rows)) dplyr::filter(., row_number() <= max_rows)
      else .
    } %>%
    dplyr::select(!!select_quo) %>%
    collect()


  if (!quiet) glue("found {nrow(logs)} entries. ") %>% message(appendLF = FALSE)

  if ("log_datetime" %in% names(logs)) {
    # server stores everything in UTC
    logs <- mutate(logs, log_datetime = force_tz(log_datetime, "UTC"))

    # local TZ conversion
    if (!is.null(convert_to_TZ)) {
      if (!quiet) glue("Converting to timezone '{convert_to_TZ}'.") %>%
        message(appendLF = FALSE)
      logs <- mutate(logs, log_datetime = with_tz(log_datetime, convert_to_TZ))
    }
  }
  message("")

  return(logs)
}

#' Retrieve data logs
#'
#' Returns data logs joined with experiments, devices, and experiment_device_data (for prefix only) tables so filter conditions can be applied on these as well. Sorted by descending order (i.e. latest record first).
#'
#' @param filter what filter conditions to apply, if any (forwarded to \link[dplyr]{filter})
#' @param select what columns to select (forwarded to \link[select]{select}), by default a selection of the most commonly used columns
#' @param max_rows if provided, only selects the indicated number of rows (more efficient this way than part of the filter)
#' @param convert_to_TZ if provided, converts the log_datetime to the provided timezone (by default the local one stored in \code{Sys.getenv("TZ")}). If NULL, will keep it as UTC.
#' @return device state logs
#' @export
c3_get_device_data_logs <- function(
  group_id = default(group_id), filter = NULL,
  select = c(exp_id, exp_name, device_id, device_name, device_data_log_id, datetime, data_idx, starts_with("data_key"), starts_with("data_")),
  max_rows = NULL,
  convert_to_TZ = Sys.getenv("TZ"),
  con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving device data logs for devices in group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "{if(!is.null(max_rows)) str_c(\", limited to \", max_rows, \" rows max\") else ''}... ") %>%
      message(appendLF = FALSE)
  }

  logs <- tbl(con, "device_data_logs") %>%
    left_join(tbl(con, "devices"), by = "device_id") %>%
    left_join(dplyr::select(tbl(con, "experiment_device_data"), exp_device_data_id, exp_id, data_key_prefix), by = "exp_device_data_id") %>%
    left_join(tbl(con, "experiments"), by = c("exp_id", "group_id")) %>%
    arrange(desc(device_data_log_id)) %>%
    dplyr::filter(group_id == group_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    {
      if (!is.null(max_rows)) dplyr::filter(., row_number() <= max_rows)
      else .
    } %>%
    # for time offset calculations
    mutate(datetime = log_datetime) %>%
    dplyr::select(!!select_quo, log_time_offset) %>%
    collect()

  if (!quiet) glue("found {nrow(logs)} entries. ") %>% message(appendLF = FALSE)

  if (nrow(logs) == 0) return(logs)

  if ("log_datetime" %in% names(logs))
    logs <- mutate(logs, log_datetime = force_tz(log_datetime, "UTC"))
  if ("datetime" %in% names(logs))
    logs <- mutate(logs, datetime = force_tz(datetime - log_time_offset, "UTC"))

  # local TZ conversion
  if (!is.null(convert_to_TZ) && any(c("log_datetime", "datetime") %in% names(logs))) {
    if (!quiet) glue("Converting to timezone '{convert_to_TZ}'.") %>%
      message(appendLF = FALSE)
    if ("log_datetime" %in% names(logs))
      logs <- mutate(logs, log_datetime = with_tz(log_datetime, convert_to_TZ))
    if ("datetime" %in% names(logs))
      logs <- mutate(logs, datetime = with_tz(datetime, convert_to_TZ))
  }
  message("")

  # after time offset calculation
  return(logs %>% dplyr::select(!!select_quo))
}

# other ====

#' retrieve active cameras
#' @export
c3_get_cameras <- function(con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) cat("\nInfo: retrieving active cameras... ")
  df <- tbl(con, "cameras") %>%
    filter(active) %>%
    collect()
  if (!quiet) cat(glue("found {nrow(df)}\n\n"))
  return(df)
}

#' Synchronize device names
#' @note no longer necessary, sync happens during logging
#' Update device names in the database from the cloud.
#' @return devices with updated device names
#' @export
c3_snyc_device_names_from_cloud <- function(in_use_only = TRUE, con = default(con), access_token = default(access_token), quiet = default(quiet)) {

  stop("this function is deprecated", call. = FALSE)
  con <- validate_db_connection(enquo(con))

  devices <-
    c3_get_devices(in_use_only = in_use_only, con = con, quiet = quiet) %>%
    mutate(
      device_name = map_chr(device_particle_id, c3_get_device_name_from_cloud, access_token = access_token, quiet = quiet),
      last_name_sync = with_tz(now(), tzone = "UTC"),
      values = map2_chr(device_particle_id, device_name, ~sprintf("('%s', '%s')", .x, str_replace(.y, fixed("'"), "''")))
    )

  sql <-
    glue(
      "update devices as tbl set device_name = map.device_name
      from (values {collapse(devices$values, ',\n')}) as map(device_particle_id, device_name)
      where map.device_particle_id = tbl.device_particle_id")

  if (!quiet) cat(glue("\nInfo: updating device names in DB... "))
  result <- dbExecute(con, sql)
  if (!quiet) cat(glue("{result} updated successfully.\n\n"))

  # return the updated devices invisibly
  return(invisible(devices %>% select(-values)))
}
