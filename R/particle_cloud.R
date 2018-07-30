# functions to interact with the particle cloud --------

# helper function to make a particle cloud request
# @param timeout how long to wait for curl request
# @param nr which request this is (purely for info messages)
# @param total total number of requests (purely for info messages)
# @note consider implementing an asynchronious version with curl_fetch_multi (if possible to intet4q53 well into shiny app.)
make_particle_cloud_request <- function(endpoint, nr = NULL, total = NULL, timeout = default(request_timeout), access_token = default(access_token), quiet = default(quiet)) {

  # safety checks
  if (nchar(access_token) == 0) stop("missing access token", call. = FALSE)

  # request
  handle <- new_handle(timeout = timeout)
  request <- sprintf("https://api.particle.io/v1/%s?access_token=%s", endpoint, access_token)

  if (!quiet) {
    glue("\nInfo: making cloud request ",
         "{if(!is.null(nr) && !is.null(total)) str_c(nr, '/', total, ' ') else ''}",
         "('{endpoint}')... ") %>%
      message(appendLF = FALSE)
  }

  # generate curl handle
  result <-
    tryCatch(
      handle %>%
        # make request
        curl_fetch_memory(request, handle = .) %>%
        { rawToChar(.$content) } %>%
        fromJSON(),
      error = function(e) {
        if (str_detect(e$message, ":")) {
          return(list(error =  str_extract(e$message, "^[^:]*"), error_details = str_extract(e$message, "[^:\\s][^:]*$")))
        } else {
          return(list(error = e$message))
        }
      }
    )

  if (!is.null(result$error)) {
    if (!quiet) glue("failed.") %>% message()
    glue("encountered the following error: {result$error}") %>%
      warning(immediate. = TRUE, call. = FALSE)
  } else if (!quiet) {
    glue("successful.") %>% message()
  }

  return(result)
}

#' Get general device information
#'
#' Get information from the particle cloud about devices.
#'
#' @param particle_id the ID(s) of the particle device(s)
#' @param access_token the access token for the accout
#' @return nested data frame (converted from JSON)
# @ note: consider making a function to udpate particle ids in the DB from here (overkill? since state/data logs cause update too)
ll_get_devices_cloud_info <- function(devices = ll_get_devices(group_id = group_id, con = con), group_id = default(group_id), con = default(con), access_token = default(access_token), convert_to_TZ = Sys.getenv("TZ"), quiet = default(quiet)) {

  # safety checks
  if (!is.data.frame(devices) | !all(c("particle_id", "device_name") %in% names(devices)))
    stop("devices needs to be supplied as a data frame with columns (at the least) 'particle_id' and 'device_name'", call. = FALSE)

  # request general info
  info <- make_particle_cloud_request(
    endpoint = "devices",
    access_token = access_token,
    quiet = quiet
  )

  if (nrow(info) > 0) {
    devices <- devices %>% left_join(info, by = c("device_name" = "name"))
    problematic_particle_ids <- filter(devices, particle_id != id)
    if (nrow(problematic_particle_ids) > 0) {
      problematic_particle_ids %>% mutate(problem = str_c(device_name, " (db: ", particle_id, ", cloud: ", id, ")"))
      glue("some devices' particle ids are not yet updated in the database (will happen at the next successful data/state log)") %>%
        warning(immediate. = TRUE, call. = FALSE)
    }
    devices <- devices %>% select(-id)

    # time zone
    devices <- devices %>% mutate(last_heard = ymd_hms(last_heard)) %>%
    {
      if (!is.null(convert_to_TZ)) mutate(., last_heard = with_tz(last_heard, convert_to_TZ))
      else .
    }
  } else
    warning("no information retrieved from cloud", immediate. = TRUE, call. = FALSE)

  return(devices)
}

# helper function for cloud variable request
get_devices_cloud_variable <- function(devices, variable, access_token = default(access_token), quiet = default(quiet)) {
  # safety checks
  if (!is.data.frame(devices) | !all(c("particle_id", "device_name") %in% names(devices)))
    stop("devices needs to be supplied as a data frame with columns (at the least) 'particle_id' and 'device_name'", call. = FALSE)

  # request state
  ..device_variable.. <- variable
  devices %>%
    mutate(
      lists = map2(
        particle_id, row_number(),
        ~make_particle_cloud_request(
          endpoint = sprintf("devices/%s/%s", .x, ..device_variable..),
          nr = .y,
          total = nrow(devices),
          access_token = access_token,
          quiet = quiet
        )
      )
    ) %>%
    unpack_lists_data_frame(unnest_single_values = TRUE, unpack_sub_lists = TRUE, nest_into_data_frame = FALSE)
}

# helper function to unpack cloud variable result
unpack_cloud_variable_result <- function(var_data, data_column, renames = c(), convert_to_TZ = Sys.getenv("TZ"), spread_function = NULL) {

  var_data <- mutate(var_data, ..rowid.. = row_number())
  data_column_quo <- enquo(data_column)

  # unpack state data
  if (nrow(var_data) > 0 && "result" %in% names(var_data)) {
    var_data_unpacked <-
      var_data %>%
      select(..rowid.., result) %>%
      mutate(result = map(result, ~if(!is.na(.x)) {
        tryCatch(fromJSON (.x), error = function(e) { warning("problems parsing JSON - ", e$message, immediate. = TRUE, call. = FALSE); list() })
      } else list())) %>%
      unpack_lists_data_frame(result)

    # check if there is any data
    if (quo_text(data_column_quo) %in% names(var_data_unpacked)) {
      var_data_unpacked <- var_data_unpacked %>% unnest(!!data_column_quo)

      # only rename what exists
      renames <- renames[unname(renames) %in% names(var_data_unpacked)]

      if (nrow(var_data_unpacked) > 0) {
        var_data_unpacked <-
          var_data_unpacked %>%
          rename(!!!renames) %>%
          mutate(datetime = ymd_hms(datetime)) %>%
          {
            if (!is.null(convert_to_TZ)) mutate(., datetime = with_tz(datetime, convert_to_TZ))
            else .
          }

        if (!is.null(spread_function)) {
          var_data_unpacked <- spread_function(var_data_unpacked)
        }

    }

      var_data <- left_join(var_data %>% select(-result), var_data_unpacked, by = "..rowid..")
    }
  }

  return (select(var_data, -..rowid..))
}

#' Get device state
#'
#' Get state from the particle cloud for devices.
#' @inheritParams ll_get_devices_cloud_info
#' @inheritParams ll_get_device_state_logs
#' @param spread whether to convert the state data into wide format (note that this combines value and units columns!)
#' @return nested data frame (converted from JSON)
ll_get_devices_cloud_state <-
  function(devices = ll_get_devices(group_id = group_id, con = con),
           group_id = default(group_id),
           con = default(con),
           access_token = default(access_token),
           convert_to_TZ = Sys.getenv("TZ"),
           spread = FALSE,
           quiet = default(quiet)) {

    if (nrow(devices) == 0) return(data_frame())

    devices %>%
      # request state info
      get_devices_cloud_variable(
        variable = "state",
        access_token = access_token,
        quiet = quiet
      ) %>%
      # unpack state data
      unpack_cloud_variable_result(
        data_column = s,
        renames = c(datetime = "dt", key = "k", value = "v", units = "u"),
        convert_to_TZ = convert_to_TZ,
        spread_function = if (spread) spread_state_columns else NULL
      )
  }

#' Get device data
#'
#' Get latest data from the particle cloud for devices.
#' @inheritParams ll_get_devices_cloud_info
#' @inheritParams ll_get_device_state_logs
#' @param spread whether to convert the state data into wide format (note that this combines key and index, as well as, value and units columns!)
#' @return nested data frame (converted from JSON)
ll_get_devices_cloud_data <-
  function(devices = ll_get_devices(group_id = group_id, con = con),
           group_id = default(group_id),
           con = default(con),
           access_token = default(access_token),
           convert_to_TZ = Sys.getenv("TZ"),
           spread = FALSE,
           quiet = default(quiet)) {

    if (nrow(devices) == 0) return(data_frame())

    devices %>%
      # request state info
      get_devices_cloud_variable(
        variable = "data",
        access_token = access_token,
        quiet = quiet
      ) %>%
      # unpack state data
      unpack_cloud_variable_result(
        data_column = d,
        renames = c(datetime = "dt", idx = "i", key = "k", value = "v", units = "u"),
        convert_to_TZ = convert_to_TZ,
        spread_function = if (spread) spread_data_columns else NULL
      ) %>%
      # add missing
      { if (!"datetime" %in% names(.)) mutate(., datetime = NA_real_) else . } %>%
      { if (!"idx" %in% names(.)) mutate(., idx = NA_integer_) else . } %>%
      { if (!"key" %in% names(.)) mutate(., key = NA_character_) else . } %>%
      { if (!"value" %in% names(.)) mutate(., value = NA_real_) else . } %>%
      { if (!"units" %in% names(.)) mutate(., units = NA_character_) else . } %>%
      # rename raw data
      { if ("r" %in% names(.)) rename(., raw_serial = r) else mutate(., raw_serial = NA_character_) } %>%
      { if ("e" %in% names(.)) rename(., raw_serial_errors = e) else mutate(., raw_serial_errors = NA_character_) }
  }

#' Test which values one gets for a set of experiment devices
#' @param experiment_device_links experiment_device_links records, see \link{ll_get_experiment_device_links}
ll_test_experiment_device_links <- function(experiment_device_links, spread = FALSE, access_token = default(access_token), quiet = default(quiet)) {

  if (!"particle_id" %in% names(experiment_device_links))
    stop("particle_id is a required column in experiment_device_links data frame", call. = FALSE)
  if (!"device_name" %in% names(experiment_device_links))
    stop("device_name is a required column in experiment_device_links data frame", call. = FALSE)
  if (!"data_idx" %in% names(experiment_device_links))
    stop("data_idx is a required column in experiment_device_links data frame", call. = FALSE)

  data <- ll_get_devices_cloud_data(devices = experiment_device_links %>% select(particle_id, device_name) %>% unique(), spread = FALSE)
  if (nrow(data) > 0) {
    data <- select(data, particle_id, device_name, datetime, raw_serial, raw_serial_errors, idx, key, value, units)
    experiment_device_links %>%
      rename(idx = data_idx) %>%
      left_join(data, by = c("particle_id", "device_name", "idx")) %>%
      { if (spread) spread_data_columns(.) else . }
  } else {
    experiment_device_links
  }
}

#' Cloud data / experiment links summary
#'
#' Utility function to combine experimental device links with devices cloud data
#' @export
ll_summarize_cloud_data_experiment_links <- function(cloud_data, experiment_device_links) {

  if (missing(experiment_device_links)) stop("on experiment device links provided", call. = FALSE)
  if (missing(cloud_data)) stop("no cloud data provided")

  experiment_device_links <- experiment_device_links[!names(experiment_device_links) %in% c("data_group", "exp_device_data_id", "particle_id")]

  full_join(
    select(cloud_data, particle_id, device_name, datetime, raw_serial, raw_serial_errors, idx, key, value, units),
    experiment_device_links,
    by = c("device_name", "idx" = "data_idx")) %>%
    filter(active) %>%
    nest(exp_id, recording, .key = ..exp_data..) %>%
    mutate(
      recording_exp_ids = map_chr(..exp_data.., ~filter(.x, recording)$exp_id %>% { if(length(.) > 0) collapse(., sep = ", ") else NA_character_ }),
      non_recording_exp_ids = map_chr(..exp_data.., ~filter(.x, !recording)$exp_id %>% { if(length(.) > 0) collapse(., sep = ", ") else NA_character_ })
    ) %>%
    select(-..exp_data..)
}
