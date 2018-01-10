# functions to interact with the particle cloud --------

#' Get device information
#'
#' Get information from the particle cloud about a device
#' @param device_particle_id the ID of the particle device
#' @param access_token the access token for the accout
#' @return list (from JSON)
c3_get_device_info_from_cloud <- function(device_particle_id, access_token = default(access_token), quiet = default(quiet)) {

  # safety checks
  if (missing(device_particle_id) || nchar(device_particle_id) == 0) stop("missing device_particle_id", call. = FALSE)
  if (nchar(access_token) == 0) stop("missing access token", call. = FALSE)

  # request
  request <- sprintf("https://api.particle.io/v1/devices/%s?access_token=%s", device_particle_id, access_token)

  if (!quiet) cat(glue("\nInfo: making cloud request for device '{device_particle_id}'... "))

  # generate curl handle
  result <- new_handle() %>%
    # make request
    curl_fetch_memory(request, handle = .) %>%
    { rawToChar(.$content) } %>%
    fromJSON()

  if (!is.null(result$error)) {
    if (!quiet) cat(glue("failed.\n\n"))
    warning(glue("encountered the following error for device '{device_particle_id}': {result$error}"), immediate. = TRUE, call. = FALSE)
  } else if (!quiet) {
    cat(glue("successful.\n\n"))
  }
  return(result)
}

#' Get device name
#' @export
c3_get_device_name_from_cloud <- function(device_particle_id, access_token = default(access_token), quiet = default(quiet)) {
  name <- c3_get_device_info_from_cloud(device_particle_id, access_token, quiet)$name
  if (is.null(name)) return(NA_character_)
  else return(name)
}


