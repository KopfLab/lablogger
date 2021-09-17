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

#' Add new device type
#' @param device_type_id device type ID, must be unique (will error if not)
#' @param device_type_desc device type description
#' @export
ll_add_device_type <- function(device_type_id, device_type_desc, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) glue("\nInfo: add new device type '{device_type_id}'... ") %>%
    message(appendLF = FALSE)
  data <- tibble(device_type_id = device_type_id, device_type_desc = device_type_desc)
  run_insert_sql(data, "device_types", con, quiet = quiet)
  return(invisible(data));
}

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

# update device particle IDs
# @return the devices with updated particle id and a new column (updated_particle_id)
update_device_particle_id <- function(devices, con, quiet) {

  # safety checks
  con <- validate_db_connection(enquo(con))
  stopifnot(all(c("device_id", "particle_id", "id") %in% names(devices)))

  devices <- mutate(devices, updated_particle_id = !is.na(device_id) & !is.na(id) & (is.na(particle_id) | particle_id != id))
  if (!quiet) {
    glue::glue("Info: updating particle ID for {sum(devices$updated_particle_id)} devices... ") %>%
      message(appendLF = FALSE)
  }
  sql <-
    glue::glue("UPDATE devices AS t SET particle_id = new.particle_id ",
               "FROM ( VALUES {df_to_sql(select(filter(devices, updated_particle_id), device_id, id))} ) ",
               "AS new (device_id, particle_id) ",
               "WHERE new.device_id = t.device_id")
  result <- run_sql(sql, con)
  if (!quiet) {
    glue::glue(
      "{result} records updated.\n",
      " - {paste(with(filter(devices, updated_particle_id), paste(device_id, ':', particle_id, 'to', id)), collapse = '\n - ')}") %>%
      message()
  }

  # return updated devices
  devices %>%
    mutate(particle_id = ifelse(updated_particle_id, id, particle_id))
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
ll_add_experiment <- function(exp_id, exp_desc = NA, exp_notes = NA, group_id = default(group_id), con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (missing(exp_id)) stop("must supply an experiment id", call. = FALSE)
  if (!quiet) glue("\nInfo: add new experiment '{exp_id}' for group '{group_id}'... ") %>%
    message(appendLF = FALSE)
  data <- data_frame(exp_id, exp_desc, exp_notes, group_id, recording = FALSE)
  run_insert_sql(data, "experiments", con, quiet = quiet)
  return(invisible(data));
}

#' Update experiment information
#' @inheritParams ll_add_experiment
#' @return whether the update was successful or not
#' @export
ll_update_experiment_info <- function(exp_id, exp_desc = NULL, exp_notes = NULL, group_id = default(group_id), con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (missing(exp_id)) stop("must supply an experiment id", call. = FALSE)
  if (length(exp_id) != 1) stop("must provide only one exp id", call. = FALSE)
  if (!quiet) glue("\nInfo: updating info for experiment '{exp_id}'... ") %>%
    message(appendLF = FALSE)

  updates <- c(
    exp_desc = if (!is.null(exp_desc)) exp_desc,
    exp_notes = if (!is.null(exp_notes)) exp_notes
  )
  if(is.null(update) || length(update) == 0) stop("nothing to update", call. = FALSE)

  sql <- glue::glue(
    "UPDATE experiments SET {to_sql(updates, named = TRUE)} ",
    "WHERE group_id = {to_sql(group_id)} AND exp_id = {to_sql(exp_id)}")
  result <- run_sql(sql, con)

  if (!quiet) {
    if (result > 0) glue("{result} record updated.") %>% message()
    else message("no records found, this experiment is not part of this group.")
  }

  return(result == 1)
}

# TODO: rename to ll_add_experiment_device_links
#' Link device to experiment
#'
#' Note that for the same device, experiment and index, only one record can exist. If the same one is added again, it will simply update the existing one to be active (in case it is not).
#'
#' @param exp_id the unique identifier of the experiment (usually a few letter code). Has to exist already in the data base.
#' @param device_ids one or multiple device ids to add (by default is inferred from the device names)
#' @param device_names names of the device(s) to link (define alternatively to the device id, will determine device_ids internally)
#' @param data_idxs the data indices to map for the experiment, must be same length as device_names/device_ids
#' @param experiment_devices alternatively provide experiment devices as a data frame right away
ll_add_experiment_devices <- function(
  exp_id,
  device_names,
  data_idxs,
  device_ids = ll_get_device_ids(device_names, quiet = quiet),
  experiment_devices = tibble(exp_id, device_id = device_ids, data_idx = data_idxs),
  group_id = default(group_id),
  con = default(con),
  quiet = default(quiet)) {

  if (!"exp_id" %in% names(experiment_devices)) stop("must supply an existing experiment id", call. = FALSE)
  if (!"device_id" %in% names(experiment_devices)) stop("must supply device_id(s)", call. = FALSE)
  if (!"data_idx" %in% names(experiment_devices) || !is.numeric(experiment_devices$data_idx))
    stop("must supply integer data indices", call. = FALSE)

  # no duplicates
  experiment_devices <- unique(experiment_devices)

  if (!quiet) {
    device_info <- { if(!missing(device_names)) device_names else experiment_devices$device_id } %>%
      str_c(" #", experiment_devices$data_idx) %>% glue::glue_collapse(sep = ", ")
    glue("\nInfo: linking device data ({device_info}) to experiment '{exp_id}'... ") %>%
    message(appendLF = FALSE)
  }
  run_insert_sql(
    experiment_devices, "experiment_device_data",
    # if record already exists, simply update it
    on_conflict_constraint = "experiment_device_data_exp_id_device_id_data_idx_key",
    on_conflict_do = "UPDATE SET active = true",
    con = con, quiet = quiet
  )
  return(invisible(experiment_devices));
}

#' Remove experiment device links
#'
#' Removes/deactivates the specified experiment device links. Removes those that don't already have data records associated with them and deactivates the others.
#'
#' @inheritParams ll_add_experiment_devices
#' @param exp_id experiment id
#' @param exp_device_data_ids exp_device_data_ids
ll_remove_experiment_device_links <- function(exp_id, exp_device_data_ids, con = default(con), quiet = default(quiet)) {

  if (!quiet) {
    glue("\nInfo: trying to remove (if not already used) or else deactivate ",
         "{length(exp_device_data_ids)} link(s) for experiment '{exp_id}'... ") %>%
      message(appendLF = FALSE)
  }

  deactivated <-
    glue::glue(
      "UPDATE experiment_device_data SET active = false WHERE ",
      "exp_id = {to_sql(exp_id)} ",
      "AND exp_device_data_id IN ({to_sql(exp_device_data_ids)})") %>%
    run_sql(con)

  deleted <-
    glue::glue(
      "DELETE FROM experiment_device_data WHERE ",
      "exp_id = {to_sql(exp_id)} ",
      "AND exp_device_data_id IN ({to_sql(exp_device_data_ids)}) ",
      "AND NOT EXISTS (SELECT 1 FROM device_data_logs ",
      "WHERE device_data_logs.exp_device_data_id = experiment_device_data.exp_device_data_id)") %>%
    run_sql(con)

  if (!quiet) {
    glue::glue("{deleted} link(s) deleted, {deactivated - deleted} link(s) deactivated.") %>%
      message()
  }

  return(invisible(NULL))
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
