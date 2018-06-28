#' Add a new owner record
#' @param owner_id the unique identifier/name of the owner. Will error if id already exists.
#' @param desc the description for the device owner (optional)
#' @export
c3_add_owner <- function(owner_id, desc = NA, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) glue("\nInfo: add new owner '{owner_id}'... ") %>% message(appendLF = FALSE)
  data <- data_frame(owner_id = owner_id, owner_desc = desc)
  run_insert_sql(data, "owners", con, quiet = quiet)
  return(invisible(data));
}

#' Add new device
#' @param device_name device name / id, must be unique for each owner (will error if not)
#' @param desc device description
#' @param device_type_id device type name, must exist in database (will error if not)
#' @param owner_id owner name, must exist in database (will error if not)
#' @param particle_id optional, will be automatically filled in the first time the device logs to the database
#' @param in_use whether device is in use (if not, cannot log any data)
#' @export
c3_add_device <- function(device_name, desc = NA, device_type_id = "undefined", owner_id = default(owner_id), particle_id = NA, in_use = TRUE, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) glue("\nInfo: add new device '{device_name}' for owner '{owner_id}'... ") %>%
    message(appendLF = FALSE)
  data <- data_frame(device_name, device_desc = desc, device_type_id, owner_id, particle_id, in_use)
  run_insert_sql(data, "devices", con, quiet = quiet)
  return(invisible(data));
}
