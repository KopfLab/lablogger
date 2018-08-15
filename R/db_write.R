# groups ======

#' Add a new group record
#' @param group_id the unique identifier/name of the group. Will error if id already exists.
#' @param desc the description for the device group (optional)
#' @export
ll_add_group <- function(group_id, desc = NA, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) glue("\nInfo: add new group '{group_id}'... ") %>% message(appendLF = FALSE)
  data <- data_frame(group_id = group_id, group_desc = desc)
  run_insert_sql(data, "groups", con, quiet = quiet)
  return(invisible(data));
}

# devices ====

#' Add new device
#' @param device_name device name / id, must be unique for each group (will error if not)
#' @param desc device description
#' @param device_type_id device type name, must exist in database (will error if not)
#' @param group_id group name, must exist in database (will error if not)
#' @param particle_id optional, will be automatically filled in the first time the device logs to the database
#' @param in_use whether device is in use (if not, cannot log any data)
#' @export
ll_add_device <- function(device_name, desc = NA, device_type_id = "undefined", group_id = default(group_id), particle_id = NA, in_use = TRUE, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) glue("\nInfo: add new device '{device_name}' for group '{group_id}'... ") %>%
    message(appendLF = FALSE)
  data <- data_frame(device_name, device_desc = desc, device_type_id, group_id, particle_id, in_use)
  run_insert_sql(data, "devices", con, quiet = quiet)
  return(invisible(data));
}

# experiments ====

#' Add new experiment
#' @param exp_id the unique identifier of the experiment (usually a few letter code). Will error if id already exists.
#' @param exp_name longer name of the experiment
#' @param desc device description
#' @param device_type_id device type name, must exist in database (will error if not)
#' @param group_id group name, must exist in database (will error if not)
#' @param particle_id optional, will be automatically filled in the first time the device logs to the database
#' @param in_use whether device is in use (if not, cannot log any data)
#' @export
ll_add_experiment <- function(exp_id, exp_desc = NA, group_id = default(group_id), con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (missing(exp_id)) stop("must supply an experiment id", call. = FALSE)
  if (!quiet) glue("\nInfo: add new experiment '{exp_id}' for group '{group_id}'... ") %>%
    message(appendLF = FALSE)
  data <- data_frame(exp_id, exp_desc, group_id, recording = FALSE)
  run_insert_sql(data, "experiments", con, quiet = quiet)
  return(invisible(data));
}

#' Link device to experiment
#'
#' Note that for the same device, experiment and index, only one record can exist (will error if already exists).
#'
#' @param exp_id the unique identifier of the experiment (usually a few letter code). Has to exist already in the data base.
#' @param device_ids one or multiple device ids to add (by default is inferred from the device names)
#' @param device_names names of the device(s) to link (define alternatively to the device id, will determine device_ids internally)
#' @param data_idxs the data indices to map for the experiment, must be same length as device_names/device_ids
ll_add_experiment_devices <- function(exp_id, device_names, data_idxs, device_ids = ll_get_device_ids(device_names, quiet = quiet), data_group = NA, group_id = default(group_id), con = default(con), quiet = default(quiet)) {

  if (missing(exp_id)) stop("must supply an existing experiment id", call. = FALSE)
  if (missing(data_idxs) || !is.numeric(data_idxs)) stop("must supply integer data indices", call. = FALSE)
  if (length(device_ids) != length(data_idxs) && !(length(device_ids) == 1 || length(data_idxs) == 1))
      stop("not the same number of device ids and data indices provided", call. = FALSE)

  if (!quiet) {
    device_info <- { if(!missing(device_names)) device_names else device_ids } %>%
      str_c(" #", data_idxs) %>% collapse(sep = ", ")
    glue("\nInfo: linking device data ({device_info}) to experiment '{exp_id}'... ") %>%
    message(appendLF = FALSE)
  }
  data <- data_frame(exp_id, device_id = device_ids, data_idx = data_idxs, data_group)
  run_insert_sql(data, "experiment_device_data", con = con, quiet = quiet)
  return(invisible(data));
}

# helper function to start/stop experiment recording
change_experiment_recording <- function(exp_id, recording, group_id, con, quiet) {

  con <- validate_db_connection(enquo(con))

  if (!quiet) {
    glue("\nInfo: trying to {if(recording) 'start' else 'stop'} recording for experiment '{exp_id}' (group '{group_id}')... ") %>%
      message(appendLF = FALSE)
  }

  result <-
    glue("UPDATE experiments SET recording = {if(recording) 'true' else 'false'}, last_recording_change={to_sql(format(now('UTC')))} WHERE group_id = {to_sql(group_id)} AND exp_id = {to_sql(exp_id)}") %>%
    run_sql(con)
  if (!quiet) {
    if (result > 0) glue("{result} record updated.") %>% message()
    else message("no records found, this experiment is not part of this group.")
  }
  return(result)
}

#' Start recording for an experiment
#' @inheritParams ll_add_experiment_devices
ll_experiment_start_recording <- function(exp_id, group_id = default(group_id), con = default(con), quiet = default(quiet)) {
  if (missing(exp_id)) stop("must supply an existing experiment id", call. = FALSE)
  result <- data_frame(exp_id) %>%
    mutate(
      updated = map_int(exp_id, ~change_experiment_recording(.x, TRUE, group_id = group_id, con = con, quiet = quiet)),
      success = updated > 0
    )
  return(invisible(result))
}

#' stop recording for an experiment
#' @inheritParams ll_experiment_stop_recording
ll_experiment_stop_recording <- function(exp_id, group_id = default(group_id), con = default(con), quiet = default(quiet)) {
  if (missing(exp_id)) stop("must supply an existing experiment id", call. = FALSE)
  result <- data_frame(exp_id) %>%
    mutate(
      updated = map_int(exp_id, ~change_experiment_recording(.x, FALSE, group_id = group_id, con = con, quiet = quiet)),
      success = updated > 0
    )
  return(invisible(result))
}
