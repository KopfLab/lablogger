
dataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values ===
  values <- reactiveValues(
    refresh_experiments = NULL,
    selected_exp_ids = c(),
    refresh_devices = NULL,
    selected_device_ids = c(),
    refresh_experiment_device_links = NULL,
    refresh_device_data_logs = NULL,
    refresh_devices_cloud_state = NULL,
    refresh_devices_cloud_data = NULL,
    refresh_devices_cloud_info = NULL
  )

  # experiments ====
  get_experiments <- reactive({
    req(values$refresh_experiments)
    withProgress(
      message = 'Fetching experiments', detail = "Querying database...", value = 0.5,
      ll_get_experiments(group_id = group_id, con = pool, convert_to_TZ = timezone)
    )
  })

  refresh_experiments <- function(init = FALSE) {
    if (is.null(values$refresh_experiments)) values$refresh_experiments <- 1
    else if (!init) values$refresh_experiments <- values$refresh_experiments + 1
  }

  select_experiments <- function(exp_ids) {
    if (!identical(values$selected_exp_ids, exp_ids)) {
      module_message(ns, "debug", glue("selecting exp ids '{if(!is.null(exp_ids)) collapse(exp_ids, sep = ', ') else 'none'}'"))
      values$selected_exp_ids <- exp_ids
    }
  }

  # devices ====
  get_devices <- reactive({
    req(values$refresh_devices)
    withProgress(
      message = 'Fetching devices', detail = "Querying database...", value = 0.5,
      ll_get_devices(group_id = group_id, con = pool) %>% arrange(device_name)
    )
  })

  refresh_devices <- function(init = FALSE) {
    if (is.null(values$refresh_devices)) values$refresh_devices <- 1
    else if (!init) values$refresh_devices <- values$refresh_devices + 1
  }

  select_devices <- function(device_ids) {
    if (!identical(values$selected_device_ids, device_ids)) {
      module_message(ns, "debug", glue("selecting device ids '{if(!is.null(device_ids)) collapse(device_ids, sep = ', ') else 'none'}'"))
      values$selected_device_ids <- device_ids
    }
  }

  # experiment device links ====
  get_experiment_device_links <- eventReactive(values$refresh_experiment_device_links, {
    if (length(values$selected_device_ids) > 0) {
      withProgress(
        message = 'Fetching experiment device links', detail = "Querying database...", value = 0.5,
        ll_get_experiment_device_links(
          group_id = group_id, con = pool,
          select = c(exp_device_data_id, exp_id, recording, device_name, data_group, data_idx, active),
          filter = device_id %in% c(!!!values$selected_device_ids))
      )
    } else {
      data_frame()
    }
  })

  refresh_experiment_device_links <- function(init = FALSE) {
    if(is.null(values$refresh_experiment_device_links)) values$refresh_experiment_device_links <- 1
    else if (!init) values$refresh_experiment_device_links <- values$refresh_experiment_device_links + 1
  }

  # device data logs ====
  get_device_data_logs <- eventReactive(values$refresh_device_data_logs, {
    if (length(values$selected_exp_ids) > 0) {
      withProgress(
        message = 'Fetching device data logs', detail = "Querying database...", value = 0.5,
        ll_get_exp_device_data_logs(
          exp_id = values$selected_exp_ids,
          group_id = group_id,
          con = pool,
          convert_to_TZ = timezone
        ))
    } else {
      data_frame()
    }
  })

  get_device_data_logs_in_time_interval <- function(logs, from, to) {
    filter(logs, between(datetime, as_datetime(from, tz = timezone), as_datetime(to, tz = timezone)))
  }

  refresh_device_data_logs <- function() {
    values$refresh_device_data_logs <- if(is.null(values$refresh_device_data_logs)) 1 else values$refresh_device_data_logs + 1
  }

  # cloud state =====
  get_devices_cloud_state <- eventReactive(values$refresh_devices_cloud_state, {
    withProgress(
      message = 'Fetching device state', detail = "Querying device cloud...", value = 0.5,
      get_devices() %>%
        filter(device_id %in% values$selected_device_ids) %>%
        ll_get_devices_cloud_state(access_token = access_token, convert_to_TZ = timezone, spread = TRUE)
    )
  })

  refresh_devices_cloud_state <- function() {
    values$refresh_devices_cloud_state <- if(is.null(values$refresh_devices_cloud_state)) 1 else values$refresh_devices_cloud_state + 1
  }

  # cloud data =====
  get_devices_cloud_data <- eventReactive(values$refresh_devices_cloud_data, {
    withProgress(
      message = 'Fetching device data', detail = "Querying device cloud...", value = 0.5,
      get_devices() %>%
        filter(device_id %in% values$selected_device_ids) %>%
        ll_get_devices_cloud_data(access_token = access_token, convert_to_TZ = timezone)
    )
  })

  refresh_devices_cloud_data <- function() {
    values$refresh_devices_cloud_data <- if(is.null(values$refresh_devices_cloud_data)) 1 else values$refresh_devices_cloud_data + 1
  }

  # cloud info ======
  get_devices_cloud_info <- eventReactive(values$refresh_devices_cloud_info, {
    withProgress(
      message = 'Fetching device info', detail = "Querying device cloud...", value = 0.5,
      get_devices() %>%
        filter(device_id %in% values$selected_device_ids) %>%
        ll_get_devices_cloud_info(access_token = access_token, convert_to_TZ = timezone)
    )
  })

  refresh_devices_cloud_info <- function() {
    values$refresh_devices_cloud_info <- if(is.null(values$refresh_devices_cloud_info)) 1 else values$refresh_devices_cloud_info + 1
  }

  # functions ====
  list(
    # experiments
    get_experiments = get_experiments,
    refresh_experiments = refresh_experiments,
    select_experiments = select_experiments,
    get_selected_experiments = reactive({values$selected_exp_ids}),
    # devices
    get_devices = get_devices,
    refresh_devices = refresh_devices,
    select_devices = select_devices,
    get_selected_devices = reactive({values$selected_device_ids}),
    # experiment devices
    get_experiment_device_links = get_experiment_device_links,
    refresh_experiment_device_links = refresh_experiment_device_links,
    # device data logs
    get_device_data_logs = get_device_data_logs,
    get_device_data_logs_in_time_interval = get_device_data_logs_in_time_interval,
    refresh_device_data_logs = refresh_device_data_logs,
    # devices cloud state
    get_devices_cloud_state = get_devices_cloud_state,
    refresh_devices_cloud_state = refresh_devices_cloud_state,
    # devices cloud data
    get_devices_cloud_data = get_devices_cloud_data,
    refresh_devices_cloud_data = refresh_devices_cloud_data,
    # devices cloud info
    get_devices_cloud_info = get_devices_cloud_info,
    refresh_devices_cloud_info = refresh_devices_cloud_info
  )

}
