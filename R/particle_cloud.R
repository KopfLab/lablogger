# functions to interact with the particle cloud variables --------

# helper function to make a particle cloud request
# @param endpoint the url endpoint for the request, typically devices/<device_id>/variable or devices/<device_id>/function
# @param arg request argument --> required for function calls! even if just \code{character(0)}
# @param timeout how long to wait for curl request
# @param nr which request this is (purely for info messages)
# @param total total number of requests (purely for info messages)
# @note consider implementing an asynchronious version with curl_fetch_multi (if possible to integrate well into shiny app.)
make_particle_cloud_request <- function(endpoint, arg = NULL, nr = NULL, total = NULL, timeout = default(request_timeout), access_token = default(access_token), quiet = default(quiet)) {

  # safety checks
  if (nchar(access_token) == 0) stop("missing access token", call. = FALSE)

  # request
  handle <- new_handle(timeout = timeout)
  if (is.null(arg)) {
    request <- sprintf("https://api.particle.io/v1/%s?access_token=%s", endpoint, access_token)
  } else if (is.character(arg) && length(arg) <= 1) {
    request <- sprintf("https://api.particle.io/v1/%s", endpoint)
    post <- sprintf("access_token=%s&arg=%s", access_token, utils::URLencode(arg))
  } else {
    stop("something amiss with arg: ", arg, call. = FALSE)
  }

  if (!quiet) {
    glue("\nInfo: making cloud request ",
         if(!is.null(nr) && !is.null(total)) "{nr}/{total} " else "",
         "('{endpoint}'",
         if(!is.null(arg)) " with arg '{arg}'" else "",
         ")... ") %>%
      message(appendLF = FALSE)
  }

  # generate curl handle
  result <-
    tryCatch(
      handle %>%
        {
          # POST?
          if (!is.null(arg)) handle_setopt(., copypostfields = post)
          else  .
        } %>%
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
#' @param devices data frame with devices for which to get cloud info, by default all devices associated with the group
#' @param include_unregistered whether to provide cloud info for devices that are not registered in the database (no by default)
#' @param particle_id the ID(s) of the particle device(s)
#' @param access_token the access token for the accout
#' @return nested data frame (converted from JSON)
# @ note: consider making a function to udpate particle ids in the DB from here (overkill? since state/data logs cause update too)
#' @export
ll_get_devices_cloud_info <- function(devices = ll_get_devices(group_id = group_id, con = con), include_unregistered = FALSE, group_id = default(group_id), con = default(con), access_token = default(access_token), convert_to_TZ = Sys.timezone(), quiet = default(quiet)) {

  # safety checks
  if (!is.data.frame(devices) | !all(c("particle_id", "device_name") %in% names(devices)))
    stop("devices needs to be supplied as a data frame with columns (at the least) 'particle_id' and 'device_name'", call. = FALSE)

  # request general info
  info <- make_particle_cloud_request(
    endpoint = "devices",
    access_token = access_token,
    quiet = quiet
  )

  if (is.data.frame(info) && nrow(info) > 0) {
    if (include_unregistered)
      devices <- devices %>% full_join(info, by = c("device_name" = "name"))
    else
      devices <- devices %>% left_join(info, by = c("device_name" = "name"))

    # check for missing particle IDs
    probs <- filter(devices, !is.na(device_id), !is.na(id), is.na(particle_id) | particle_id != id)
    if (nrow(probs) > 0) {
     glue::glue(
       "some registered devices ({paste(probs$device_name, collapse = ', ')}) ",
       "particle ids are not yet updated in the database and will be updated now...") %>%
        warning(immediate. = TRUE, call. = FALSE)
      # NOTE: this will NOT automatically update the devices in the data manager which
      # requires a "Refresh", but particle_id updates should happen rarely enough that we're
      # just not going to worry about it, even if this means that this update may run
      # multiple times (unnecessarily) until devices are refreshed
      devices <- update_device_particle_id(devices, con = con, quiet = quiet)
    }

    # check for registered devices not listed in the cloud
    probs <- filter(devices, !is.na(device_id), is.na(id))
    if (nrow(probs) > 0) {
      glue::glue(
        "some registered devices ({paste(probs$device_name, collapse = ', ')}) ",
        "do not seem to exist in the cloud") %>%
        warning(immediate. = TRUE, call. = FALSE)
    }

    devices <- devices %>% select(-id) %>%
      mutate(registered = !is.na(device_id))

    # time zone
    devices <- devices %>% mutate(last_heard = ymd_hms(last_heard)) %>%
    {
      if (!is.null(convert_to_TZ)) mutate(., last_heard = with_tz(last_heard, convert_to_TZ))
      else .
    }
  } else {
    warning("no information retrieved from cloud", immediate. = TRUE, call. = FALSE)
  }

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
        particle_id, dplyr::row_number(),
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
unpack_cloud_variable_result <- function(var_data, data_column, renames = c(), convert_to_TZ = Sys.timezone(), spread_function = NULL) {

  var_data <- mutate(var_data, ..rowid.. = dplyr::row_number())
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
           convert_to_TZ = Sys.timezone(),
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
           convert_to_TZ = Sys.timezone(),
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
      # add  missing error
      { if (!"error" %in% names(.)) mutate(., error = NA_character_) else . } %>%
      # add missing datetime
      { if (!"datetime" %in% names(.)) mutate(., datetime = NA_real_) else . } %>%
      # rename raw data
      { if ("r" %in% names(.)) rename(., raw_serial = r) else mutate(., raw_serial = NA_character_) } %>%
      { if ("e" %in% names(.)) rename(., raw_serial_errors = e) else mutate(., raw_serial_errors = NA_character_) } %>%
      # add missing data fields
      { if (!spread && !"idx" %in% names(.)) mutate(., idx = NA_integer_) else . } %>%
      { if (!spread && !"key" %in% names(.)) mutate(., key = NA_character_) else . } %>%
      { if (!spread && !"value" %in% names(.)) mutate(., value = NA_real_) else . } %>%
      { if (!spread && !"units" %in% names(.)) mutate(., units = NA_character_) else . } %>%
      # arrange
      { if (spread) arrange(., device_name) else arrange(., device_name, idx) }
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
#' @param experiment_device_links the links between devices and experiments, see \link{ll_get_experiment_device_links}
#' @param linked whether to include linked data
#' @param unlinked whether to include unlinked data
#' @export
ll_summarize_cloud_data_experiment_links <- function(
  cloud_data,
  experiment_device_links = tibble(),
  linked = TRUE, unlinked = TRUE) {

  if (missing(cloud_data)) stop("no cloud data provided")

  experiment_device_links <- experiment_device_links[!names(experiment_device_links) %in% c("exp_device_data_id", "particle_id")]

  # make sure empty cloud data or device links have the necessary columns
  if (nrow(experiment_device_links) == 0) {
    experiment_device_links <- tibble(
      exp_id = character(), recording = logical(), device_name = character(),
      data_idx = integer(), data_group = character(), active = logical())
  }

  if (nrow(cloud_data) == 0) {
    cloud_data <- tibble(
      particle_id = character(), device_name = character(),
      datetime = as.POSIXct(numeric(), origin = "1960-01-01"),
      error = character(),
      raw_serial = character(), raw_serial_errors = character(),
      idx = integer(), key = character(), value = character(), units = character()
    )
  }

  cloud_data <- cloud_data %>%
    select(particle_id, device_name, datetime, error, raw_serial, raw_serial_errors, idx, key, value, units)

  full_join(
    cloud_data %>% filter(is.na(error)) %>% select(-error),
    experiment_device_links %>% filter(active),
    by = c("device_name", "idx" = "data_idx")) %>%
    # add error info from cloud data to the existing links
    left_join(cloud_data %>% filter(!is.na(error)) %>% select(device_name, error), by = "device_name") %>%
    # add error info from cloud data for devices that have no existing links at all
    {
      bind_rows(., filter(cloud_data, !is.na(error), !device_name %in% .$device_name))
    } %>%
    mutate(
      exp_id_data_group = case_when(
        !is.na(exp_id) & !is.na(data_group) ~ str_c(exp_id, " (", data_group, ")"),
        !is.na(exp_id) ~ exp_id,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(linked & !is.na(exp_id) | unlinked & is.na(exp_id)) %>%
    select(-exp_id, -data_group) %>%
    nest(exp_id_data_group, recording, .key = ..exp_data..) %>%
    mutate(
      recording_exp_ids = map_chr(..exp_data.., ~filter(.x, recording)$exp_id_data_group %>% { if(length(.) > 0) glue::glue_collapse(., sep = ", ") else NA_character_ }),
      non_recording_exp_ids = map_chr(..exp_data.., ~filter(.x, !recording)$exp_id_data_group %>% { if(length(.) > 0) glue::glue_collapse(., sep = ", ") else NA_character_ })
    ) %>%
    select(-..exp_data..) %>%
    select(particle_id, device_name, datetime, error, everything())
}

# functions to interact with particle cloud commands =====

# helper function for cloud function calls
call_devices_cloud_function <- function(devices, func = "device", arg = "", access_token = default(access_token), quiet = default(quiet)) {
  # safety checks
  if (!is.data.frame(devices) | !all(c("particle_id", "device_name") %in% names(devices)))
    stop("devices needs to be supplied as a data frame with columns (at the least) 'particle_id' and 'device_name'", call. = FALSE)

  # request state
  devices %>%
    mutate(
      lists = map2(
        particle_id, dplyr::row_number(),
        ~make_particle_cloud_request(
          endpoint = sprintf("devices/%s/%s", .x, !!func),
          arg = !!arg,
          nr = .y,
          total = nrow(devices),
          access_token = access_token,
          quiet = quiet
        )
      )
    ) %>%
    unpack_lists_data_frame(unnest_single_values = TRUE, unpack_sub_lists = TRUE, nest_into_data_frame = FALSE)
}

#' Send device commands to the cloud
#'
#' Send commands to one or more devices.
#'
#' @param devices data frame with devices for which to issue commands
#' @param command to send
#' @param message message to add to command
#' @param access_token the access token for the accout
#' @return nested data frame (converted from JSON)
# @ note: consider making a function to udpate particle ids in the DB from here (overkill? since state/data logs cause update too)
#' @export
ll_send_devices_command <- function(devices, command, message = "", access_token = default(access_token), quiet = default(quiet)) {

  if (nchar(message) > 0) command <- paste(command, message)

  # return codes
  return_codes <- get_device_command_return_values()

  # call cloud function
  devices %>%
    # add command
    mutate(
      command = !!command,
    ) %>%
    call_devices_cloud_function(
      func = "device", arg = command,
      access_token = access_token, quiet = quiet
    ) %>%
    # add  missing error
    { if (!"error" %in% names(.)) mutate(., error = NA_character_) else . } %>%
    # add  missing return value
    { if (!"return_value" %in% names(.)) mutate(., return_value = NA_integer_) else . } %>%
    # add return message
    mutate(
      return_message = case_when(
        is.na(return_value) ~ error,
        as.character(return_value) %in% names(return_codes) ~ return_codes[as.character(return_value)],
        return_value < 0 ~ "Unknown error",
        return_value > 0 ~ "Unknown warning",
        TRUE ~ "Undefined behaviour",
      ) %>% {
        ifelse(!is.na(return_value), str_c(., " (code ", return_value, ")"), .)
      }
    )
}

# helper function for device return values
get_device_command_return_values <- function() {
  # details at https://github.com/KopfLab/labware_photon/blob/master/DeviceCommands.h
  c(
     `0` = "Success",
    `-1` = "Undefined error",
    `-2` = "Device locked",
    `-3` = "Invalid command",
    `-4` = "Invalid command value",
    `-5` = "Invalid command units",
     `1` = "State already as requested"
  )
}
