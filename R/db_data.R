# general data retrieval functions
validate_db_connection <- function(con_quo) {
  if(!is_quosure(con_quo)) stop("connection not supplied as quosure", call. = FALSE)
  eval_tidy(resolve_defaults(con_quo))
}

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

#' retrieve devices
#' @export
c3_get_devices <- function(in_use_only = TRUE, con = default(con), quiet = default(quiet)) {
  con <- validate_db_connection(enquo(con))
  if (!quiet) {
    if (in_use_only) cat("\nInfo: retrieving in-use devices... ")
    else cat("\nInfo: retrieving all devices... ")
  }
  df <- tbl(con, "devices") %>%
    filter(in_use | !in_use_only) %>%
    collect()
  if (!quiet) cat(glue("found {nrow(df)}\n\n"))
  return(df)
}

#' Update device names
#'
#' Update device names in the database from the cloud.
#' @return devices with updated device names
#' @export
c3_snyc_device_names_from_cloud <- function(in_use_only = TRUE, con = default(con), access_token = default(access_token), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))

  devices <-
    c3_get_devices(in_use_only = in_use_only, con = con, quiet = quiet) %>%
    mutate(
      device_name = map_chr(device_id, c3_get_device_name_from_cloud, access_token = access_token, quiet = quiet),
      last_name_sync = with_tz(now(), tzone = "UTC"),
      values = map2_chr(device_id, device_name, ~sprintf("('%s', '%s')", .x, str_replace(.y, fixed("'"), "''")))
    )

  sql <-
    glue(
      "update devices as tbl set device_name = map.device_name
      from (values {collapse(devices$values, ',\n')}) as map(device_id, device_name)
      where map.device_id = tbl.device_id")

  if (!quiet) cat(glue("\nInfo: updating device names in DB... "))
  result <- dbExecute(con, sql)
  if (!quiet) cat(glue("{result} updated successfully.\n\n"))

  # return the updated devices invisibly
  return(invisible(devices))
}
