# devices ====

#' Retrieve devices
#' @param group_id devices from which group to fetch
#' @return devices
#' @export
ll_get_devices <- function(group_id = default(group_id), filter = NULL,
                           select = c(device_id, particle_id, device_name, device_type_id, device_type_desc),
                           in_use_only = TRUE, con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving {if(in_use_only) 'in-use' else 'all'} devices ",
         "for group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "...") %>% message(appendLF = FALSE)
  }
  df <- tbl(con, "devices") %>%
    left_join(tbl(con, "device_types"), by = "device_type_id") %>%
    dplyr::filter(group_id == group_id_value, in_use | !in_use_only) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    dplyr::select(!!select_quo) %>%
    collect()
  if (!quiet) glue("found {nrow(df)}.") %>% message()
  return(df)
}

#' Get device id
#'
#' Convenience shortcut function to get the database integer device identifier for one or multiple devices.
#' @export
ll_get_device_ids <- function(device_names, group_id = default(group_id), quiet = default(quiet)) {
  device_names_filter <- unique(device_names)
  devices <- ll_get_devices(group_id = group_id, filter = !!quo(device_name %in% !!device_names_filter), quiet = quiet)
  device_name_to_id_map <- setNames(devices$device_id, devices$device_name)
  if (any(missing <- !device_names %in% names(device_name_to_id_map))) {
    glue("the folowing device_name(s) do not exist for group '{group_id}' and can therefore not be mapped to id(s): {glue::glue_collapse(device_names[missing], sep = ', ')}") %>%
      stop(call. = FALSE)
  }
  return(device_name_to_id_map[device_names])
}

# exeperiments =====

#' Retrieve experiments
#' @param group_id experiments from which group to fetch
#' @return experiments
#' @export
ll_get_experiments <- function(group_id = default(group_id), filter = NULL, convert_to_TZ = Sys.timezone(), con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving experiments for group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "... ") %>% message(appendLF = FALSE)
  }
  df <- tbl(con, "experiments") %>%
    dplyr::filter(group_id == group_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    arrange(desc(recording), desc(last_recording_change)) %>%
    collect()

  if (!quiet) glue("found {nrow(df)}.") %>% message(appendLF = FALSE)

  if ("last_recording_change" %in% names(df)) {
    # local TZ conversion
    if (!is.null(convert_to_TZ)) {
      if (!quiet) glue("Converting last_recording_change to timezone '{convert_to_TZ}'.") %>%
        message(appendLF = FALSE)
      df <- mutate(df, last_recording_change = with_tz(last_recording_change, convert_to_TZ))
    }
  }
  message("")

  return(df)
}

#' Retrieve experiment device links
#'
#' Returns experiment-device links (only for active/in-use devices) joined with experiment, devices and device types tables so filter conditions can be applied on any of these as well.
#'
#' @inheritParams ll_get_experiments
#' @return experiments_devices
#' @export
ll_get_experiment_device_links <- function(
    group_id = default(group_id), filter = NULL,
    select = c(exp_device_data_id, exp_id, recording, device_id, device_name, particle_id, data_group, data_idx, active),
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
    left_join(tbl(con, "device_types", by = "device_type_id")) %>%
    left_join(tbl(con, "experiments"), by = c("exp_id", "group_id")) %>%
    dplyr::filter(group_id == group_id_value, in_use) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    dplyr::select(!!select_quo, device_name, data_idx) %>%
    collect()

  if (nrow(exp_devices) > 0)
    exp_devices <- exp_devices %>%
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
#' @param convert_to_TZ if provided, converts the log_datetime to the provided timezone (by default the local one stored in \code{Sys.timezone()}). If NULL, will keep it as UTC.
#' @return device state logs
#' @export
ll_get_device_state_logs <- function(
  group_id = default(group_id), filter = NULL,
  select = c(device_id, device_name, device_state_log_id, log_datetime, log_type, log_message, starts_with("state"), notes),
  max_rows = NULL,
  convert_to_TZ = Sys.timezone(),
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
      if (!is.null(max_rows)) dplyr::filter(., dplyr::row_number() <= max_rows)
      else .
    } %>%
    dplyr::select(!!select_quo) %>%
    collect()


  if (!quiet) glue("found {nrow(logs)} records. ") %>% message(appendLF = FALSE)

  # local TZ conversion
  if (!is.null(convert_to_TZ) && "log_datetime" %in% names(logs)) {
    if (!quiet) glue("Converting to timezone '{convert_to_TZ}'.") %>%
      message(appendLF = FALSE)
    logs <- mutate(logs, log_datetime = with_tz(log_datetime, convert_to_TZ))
  }
  message("")

  return(logs)
}

#' Retrieve data logs
#'
#' Returns data logs from the database joined with experiments, devices, and experiment_device_data (for prefix only) tables so filter conditions can be applied on these as well. Sorted by descending order (i.e. latest record first). Note that the group_id filter restricts the selected experiments, device data may theoretically come from devices not (or no longer) associated with the same group although this is rarely the case.
#'
#' @param filter what filter conditions to apply, if any (forwarded to \link[dplyr]{filter})
#' @param select what columns to select (forwarded to \link[dplyr]{select}), by default a selection of the most commonly used columns
#' @param max_rows if provided, only selects the indicated number of rows (more efficient this way than part of the filter)
#' @param convert_to_TZ converts the log_datetime to the provided timezone (by default the system's local timezone stored in \code{Sys.timezone()}).
#' @return device data logs
#' @family data logs functions
#' @export
ll_get_device_data_logs <- function(
  group_id = default(group_id), filter = NULL,
  select = c(exp_id, device_id, device_name, device_data_log_id, datetime, data_idx, starts_with("data_key"), starts_with("data_")),
  max_rows = NULL,
  convert_to_TZ = Sys.timezone(),
  con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  group_id_value <- group_id

  if (!quiet) {
    glue("Info: retrieving device data logs associated with experiments in group '{group_id_value}'",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "{if(!is.null(max_rows)) str_c(\", limited to \", max_rows, \" rows max\") else ''}... ") %>%
      message(appendLF = FALSE)
  }

  logs <-
    tbl(con, "device_data_logs") %>%
    left_join(dplyr::select(tbl(con, "devices"), -group_id), by = "device_id") %>%
    left_join(dplyr::select(tbl(con, "experiment_device_data"), exp_device_data_id, exp_id, data_group), by = "exp_device_data_id") %>%
    left_join(tbl(con, "experiments"), by = "exp_id") %>%
    arrange(desc(device_data_log_id)) %>%
    dplyr::filter(group_id == group_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    {
      if (!is.null(max_rows)) dplyr::filter(., dplyr::row_number() <= max_rows)
      else .
    } %>%
    # for time offset calculations
    mutate(datetime = log_datetime) %>%
    dplyr::select(!!select_quo, log_time_offset) %>%
    collect()

  if (!quiet) glue("found {nrow(logs)} entries. ") %>% message(appendLF = FALSE)

  if (nrow(logs) == 0) {
    message()
    return(logs)
  }

  # offset
  if ("datetime" %in% names(logs))
    logs <- mutate(logs, datetime = datetime - log_time_offset)

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

#' Reset device data logs cache
#'
#' @param exp_id experiment ID(s)
#' @export
ll_reset_exp_device_data_logs_cache <- function(exp_id, quiet = default(quiet)) {

  # cache paths
  cache_paths <- data_frame(
    exp_id = exp_id,
    path = file.path("cache", str_c(exp_id, "_data_logs.rds")),
    exists = file.exists(path)
  ) %>% filter(exists)

  # info
  if (!quiet) {
    glue("Info: resetting local data logs cache for experiment(s) '{glue::glue_collapse(exp_id, sep = ', ')}'... ") %>%
      message(appendLF = FALSE)
  }

  # write cache
  if (nrow(cache_paths) > 0) {
    with(cache_paths, walk(path, ~file.remove(.x)))
  }

  if (!quiet) message("complete.")
}

#' Get device data logs for specific experiment(s)
#'
#' Also supports efficient caching of the retrieved data.
#'
#' @inheritParams ll_get_device_state_logs
#' @param exp_id experiment ID(s)
#' @param ... forwarded to ll_get_device_state_logs
#' @param cache whether to cache the data logs
#' @param reac_cache whether to read the cache
#' @export
ll_get_exp_device_data_logs <- function(exp_id, group_id = default(group_id), ..., cache = TRUE, read_cache = TRUE, quiet = default(quiet)) {

  # cache paths
  cache_paths <- data_frame(
    exp_id = exp_id,
    path = file.path("cache", str_c(exp_id, "_data_logs.rds")),
    exists = file.exists(path)
  )

  # logs
  logs <- data_frame()

  # read cache
  if (read_cache && any(cache_paths$exists)) {

    if (!quiet) {
      glue("Info: reading data logs from local cache for experiment(s) ",
           "'{glue::glue_collapse(filter(cache_paths, exists)$exp_id, sep = ', ')}'... ") %>%
        message(appendLF = FALSE)
    }

    logs <- cache_paths %>% filter(exists) %>%
      mutate(data = map(path, read_rds)) %>%
      select(-exp_id, -path, -exists) %>%
      unnest(data)

    if (!quiet) glue("recovered {nrow(logs)} records; querying database for newer records...") %>% message()

    # more complex filter to account for each experiment
    filter_quo <-
      cache_paths %>% select(exp_id) %>%
      left_join(logs %>% group_by(exp_id) %>% summarize(max_device_data_log_id = max(device_data_log_id)), by = "exp_id") %>%
      with(ifelse(
        !is.na(max_device_data_log_id),
        sprintf("(exp_id == \"%s\" & device_data_log_id > %g)", exp_id, max_device_data_log_id),
        sprintf("exp_id == \"%s\"", exp_id))) %>%
      collapse (sep = " | ") %>%
      parse_expr()
  } else {
    # simple filter for all experiments
    filter_quo <- quo(exp_id %in% !!cache_paths$exp_id)
  }

  # fetch from database
  db_logs <- ll_get_device_data_logs(group_id = group_id, filter = !!filter_quo, ..., quiet = quiet)
  logs <- bind_rows(db_logs, logs)

  # safety check
  if (nrow(logs) == 0) return(logs)

  # write cache
  if (cache && nrow(db_logs) > 0) {
    if (!quiet) {
      glue("Info: updating local data logs cache for experiment(s) '{glue::glue_collapse(exp_id, sep = ', ')}'... ") %>%
        message(appendLF = FALSE)
    }

    if(!dir.exists("cache")) dir.create("cache")
    logs %>%
      left_join(cache_paths, by = "exp_id") %>%
      select(-exists) %>%
      nest(-path) %>%
      with(walk2(data, path, ~write_rds(x = .x, path = .y)))

    if (!quiet) message("complete.")
  }

  return(logs)
}

# other ====

#' retrieve active cameras
#' @export
ll_get_cameras <- function(con = default(con), quiet = default(quiet)) {
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
ll_snyc_device_names_from_cloud <- function(in_use_only = TRUE, con = default(con), access_token = default(access_token), quiet = default(quiet)) {

  stop("this function is deprecated", call. = FALSE)
  con <- validate_db_connection(enquo(con))

  devices <-
    ll_get_devices(in_use_only = in_use_only, con = con, quiet = quiet) %>%
    mutate(
      device_name = map_chr(device_particle_id, ll_get_device_name_from_cloud, access_token = access_token, quiet = quiet),
      last_name_sync = with_tz(now(), tzone = "UTC"),
      values = map2_chr(device_particle_id, device_name, ~sprintf("('%s', '%s')", .x, str_replace(.y, fixed("'"), "''")))
    )

  sql <-
    glue(
      "update devices as tbl set device_name = map.device_name
      from (values {glue::glue_collapse(devices$values, ',\n')}) as map(device_particle_id, device_name)
      where map.device_particle_id = tbl.device_particle_id")

  if (!quiet) cat(glue("\nInfo: updating device names in DB... "))
  result <- dbExecute(con, sql)
  if (!quiet) cat(glue("{result} updated successfully.\n\n"))

  # return the updated devices invisibly
  return(invisible(devices %>% select(-values)))
}
