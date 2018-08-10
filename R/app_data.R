experimentsDataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_experiments = NULL,
    selected_exp_ids = c(), # multi selection experiments
    loaded_exp_id = NULL, # single selection experiments
    loaded_exp_device_links = NULL # device links of single selection experiment
  )

  # experiments
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

  # single experiment
  load_experiment <- function(exp_id) {
    stopifnot(length(exp_id) == 1)
    if (!identical(values$loaded_exp_id, exp_id)) {
      module_message(ns, "debug", glue("loading exp id '{exp_id}'"))
      values$loaded_exp_id <- exp_id

      filter_quo <- quo(exp_id == !!values$loaded_exp_id)
      values$loaded_exp_device_links <-
        withProgress(
          message = 'Loading experiment', detail = "Querying database for experiment devices...", value = 0.5,
          ll_get_experiment_device_links(group_id = group_id, con = pool, select = c(device_id, device_name, particle_id),filter = active && !!filter_quo)
        ) %>% unique()
    }
  }

  start_experiment <- function() {
    withProgress(
      message = 'Starting experiment', detail = "Updating data base...", value = 0.5,
      ll_experiment_start_recording(exp_id = values$loaded_exp_id, group_id = group_id, con = pool)
    )
  }

  stop_experiment <- function() {
    withProgress(
      message = 'Stopping experiment', detail = "Updating data base...", value = 0.5,
      ll_experiment_stop_recording(exp_id = values$loaded_exp_id, group_id = group_id, con = pool)
    )
  }

  # experimentsDataServer functions ====
  list(
    get_experiments = get_experiments,
    refresh_experiments = refresh_experiments,
    select_experiments = select_experiments,
    get_selected_experiments = reactive(values$selected_exp_ids),
    load_experiment = load_experiment,
    get_loaded_experiment = reactive(values$loaded_exp_id),
    is_loaded_experiment_recording = reactive(filter(get_experiments(), exp_id == !!values$loaded_exp_id)$recording),
    start_experiment = start_experiment,
    stop_experiment = stop_experiment
  )

}

devicesDataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_devices = NULL,
    selected_device_ids = c()
  )

  # devices
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

  # devicesDataServer functions ====
  list(
    get_devices = get_devices,
    refresh_devices = refresh_devices,
    select_devices = select_devices,
    get_selected_devices = reactive({values$selected_device_ids})
  )
}

datalogsDataServer <- function(input, output, session, experiments, devices, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_device_data_logs = NULL
  )

  # main getter function for device data logs
  get_device_data_logs <- function(exp_ids, device_ids = NULL) {
    if (length(exp_ids) > 0 && (is.null(device_ids) || length(device_ids) > 0)) {
      data_logs <- withProgress(
        message = 'Fetching device data logs', detail = "Querying database...", value = 0.5,
        ll_get_exp_device_data_logs(
          exp_id = exp_ids,
          group_id = group_id,
          con = pool,
          convert_to_TZ = timezone
        )
      )
      if (!is.null(device_ids)) data_logs <- filter(data_logs, device_id %in% device_ids)
      return(data_logs)
    } else {
      return(data_frame())
    }
  }

  refresh_data_logs <- function() {
    values$refresh_device_data_logs <- if(is.null(values$refresh_device_data_logs)) 1 else values$refresh_device_data_logs + 1
  }

  # datalogsDataServer functions =====
  list(
    get_devices_experiments_data_logs = eventReactive(
      values$refresh_device_data_logs,
      get_device_data_logs(experiments$get_selected_experiments(), devices$get_selected_devices())),
    get_experiments_data_logs = eventReactive(
      values$refresh_device_data_logs,
      get_device_data_logs(experiments$get_selected_experiments())),
    get_experiment_data_logs = eventReactive(
      experiments$get_loaded_experiment(),
      get_device_data_logs(experiments$get_loaded_experiment())),
    refresh_data_logs = refresh_data_logs
  )
}

dataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values ===
  values <- reactiveValues(
    devices = data_frame(),
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
  set_devices <- function(devices) {
    module_message(ns, "debug", "setting devices")
    values$devices = devices
  }

  observe({
    req(values$refresh_devices)
    set_devices(withProgress(
      message = 'Fetching devices', detail = "Querying database...", value = 0.5,
      ll_get_devices(group_id = group_id, con = pool) %>% arrange(device_name)
    ))
  })

  get_devices <- reactive(values$devices)

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

  # list(
  #   # experiments
  #   get_experiments = get_experiments,
  #   refresh_experiments = refresh_experiments,
  #   select_experiments = select_experiments,
  #   get_selected_experiments = reactive({values$selected_exp_ids}),
  #   # devices
  #   set_devices = set_devices,
  #   get_devices = get_devices,
  #   refresh_devices = refresh_devices,
  #   select_devices = select_devices,
  #   get_selected_devices = reactive({values$selected_device_ids}),
  #   # experiment devices
  #   get_experiment_device_links = get_experiment_device_links,
  #   refresh_experiment_device_links = refresh_experiment_device_links,
  #   # device data logs
  #   get_device_data_logs = get_device_data_logs,
  #   refresh_device_data_logs = refresh_device_data_logs,
  #   # devices cloud state
  #   get_devices_cloud_state = get_devices_cloud_state,
  #   refresh_devices_cloud_state = refresh_devices_cloud_state,
  #   # devices cloud data
  #   get_devices_cloud_data = get_devices_cloud_data,
  #   refresh_devices_cloud_data = refresh_devices_cloud_data,
  #   # devices cloud info
  #   get_devices_cloud_info = get_devices_cloud_info,
  #   refresh_devices_cloud_info = refresh_devices_cloud_info
  # )

  list()
}
