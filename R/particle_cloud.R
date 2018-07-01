# functions to interact with the particle cloud --------

#' Get device information
#'
#' Get information from the particle cloud about devices.
#'
#' @param particle_id the ID(s) of the particle device(s)
#' @param access_token the access token for the accout
#' @return nested data frame (converted from JSON)
c3_get_devices_cloud_info <- function(particle_id, access_token = default(access_token), quiet = default(quiet)) {

  # safety checks
  if (missing(particle_id) || length(particle_id) == 0) stop("missing particle_id", call. = FALSE)
  if (nchar(access_token) == 0) stop("missing access token", call. = FALSE)

  # request
  total_n <- length(particle_id)
  info <- map2(particle_id, 1:length(particle_id), function(particle_id, idx) {
    request <- sprintf("https://api.particle.io/v1/devices/%s?access_token=%s", particle_id, access_token)

    if (!quiet) {
      glue("\nInfo: making cloud request for device {idx}/{total_n} ('{particle_id}')... ") %>%
        message(appendLF = FALSE)
    }

    # generate curl handle
    result <- new_handle() %>%
      # make request
      curl_fetch_memory(request, handle = .) %>%
      { rawToChar(.$content) } %>%
      fromJSON()

    if (!is.null(result$error)) {
      if (!quiet) glue("failed.") %>% message()
      glue("encountered the following error for device '{particle_id}': {result$error}") %>%
        warning(immediate. = TRUE, call. = FALSE)
    } else if (!quiet) {
      glue("successful.") %>% message()
    }
    return(result)
  })

  unpack_lists_data_frame(data_frame(lists = info), unnest_single_values = TRUE, unpack_sub_lists = TRUE)
}

#' Get device name
#' @export
c3_get_device_name_from_cloud <- function(particle_id, access_token = default(access_token), quiet = default(quiet)) {
  name <- c3_get_device_info_from_cloud(particle_id, access_token, quiet)$name
  if (is.null(name)) return(NA_character_)
  else return(name)
}


