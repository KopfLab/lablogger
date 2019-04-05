experimentsDataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_experiments = NULL,
    selected_exp_ids = c(), # multi selection experiments
    loaded_exp_id = NULL, # single selection experiments
    loaded_exp_device_links = NULL, # device links of single selection experiment
    loaded_exp_devices = NULL, # experiment devices,
    selected_loaded_exp_device_ids = c()
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
      module_message(ns, "debug", glue("selecting exp ids '{if(!is.null(exp_ids)) glue::glue_collapse(exp_ids, sep = ', ') else 'none'}'"))
      values$selected_exp_ids <- exp_ids
    }
  }

  # single experiment
  load_experiment <- function(exp_id) {
    stopifnot(length(exp_id) == 1)
    if (!identical(values$loaded_exp_id, exp_id)) {
      module_message(ns, "debug", glue("loading exp id '{exp_id}'"))
      values$loaded_exp_id <- exp_id
      load_experiment_device_links()
    }
  }

  load_experiment_device_links <- function(...) {
    # ... = init: for compatibility with other reload functions but is not used here
    module_message(ns, "debug", glue("(re)loading exp device links for exp '{values$loaded_exp_id}'"))
    filter_quo <- quo(exp_id == !!values$loaded_exp_id)
    values$loaded_exp_device_links <-
      withProgress(
        message = 'Fetching experiment data', detail = "Querying database for experiment device links...", value = 0.5,
        ll_get_experiment_device_links(group_id = group_id, con = pool, select = c(device_id, device_name, device_type_desc, particle_id, data_idx, active),filter = !!filter_quo)
      ) %>% unique()

    values$loaded_exp_devices <-
      values$loaded_exp_device_links %>%
      filter(active) %>%
      select(device_id, device_name, particle_id, device_type_desc) %>%
      unique()
  }

  select_loaded_experiment_devices <- function(device_ids) {
    if (!identical(values$selected_loaded_exp_device_ids, device_ids)) {
      module_message(ns, "debug", glue("selecting experiment device ids '{if(!is.null(device_ids)) glue::glue_collapse(device_ids, sep = ', ') else 'none'}'"))
      values$selected_loaded_exp_device_ids <- device_ids
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
    load_experiment_device_links = load_experiment_device_links,
    get_loaded_experiment_device_links = reactive(values$loaded_exp_device_links),
    get_loaded_experiment_devices = reactive(values$loaded_exp_devices),
    select_loaded_experiment_devices = select_loaded_experiment_devices,
    get_selected_loaded_experiment_devices = reactive(values$selected_loaded_exp_device_ids),
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
    selected_device_ids = c(),
    refresh_devices_experiments_links = NULL
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
      module_message(ns, "debug", glue("selecting device ids '{if(!is.null(device_ids)) glue::glue_collapse(device_ids, sep = ', ') else 'none'}'"))
      values$selected_device_ids <- device_ids
    }
  }

  # device experiment links (from the device perspective)
  get_devices_experiments_links <- eventReactive(values$refresh_devices_experiments_links, {
    if (length(values$selected_device_ids) > 0) {
      withProgress(
        message = 'Fetching device experiment links', detail = "Querying database...", value = 0.5,
        ll_get_experiment_device_links(
          group_id = group_id, con = pool,
          select = c(exp_device_data_id, exp_id, recording, device_name, data_group, data_idx, active),
          filter = device_id %in% !!values$selected_device_ids)
      )
    } else {
      data_frame()
    }
  })

  refresh_devices_experiments_links <- function(init = FALSE) {
    if(is.null(values$refresh_devices_experiments_links)) values$refresh_devices_experiments_links <- 1
    else if (!init) values$refresh_devices_experiments_links <- values$refresh_devices_experiments_links + 1
  }

  # devicesDataServer functions ====
  list(
    get_devices = get_devices,
    refresh_devices = refresh_devices,
    select_devices = select_devices,
    get_selected_devices = reactive({values$selected_device_ids}),
    # device experiment links
    get_devices_experiments_links = get_devices_experiments_links,
    refresh_devices_experiments_links = refresh_devices_experiments_links
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

cloudInfoDataServer <- function(input, output, session, experiments, devices, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_devices_experiments_links = NULL,
    refresh_cloud_state = NULL,
    refresh_cloud_data = NULL,
    refresh_cloud_info = NULL
  )

  # cloud state
  get_cloud_state <- function(devices, device_ids) {
    module_message(ns, "debug", "fetching cloud state")
    withProgress(
      message = 'Fetching device state', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_state(access_token = access_token, convert_to_TZ = timezone, spread = TRUE)
    )
  }

  get_devices_cloud_state <- eventReactive(values$refresh_cloud_state, {
    get_cloud_state(devices$get_devices(), devices$get_selected_devices())
  })

  get_exp_devices_cloud_state <- eventReactive(values$refresh_cloud_state, {
    get_cloud_state(experiments$get_loaded_experiment_devices(), experiments$get_selected_loaded_experiment_devices())
  })

  refresh_cloud_state <- function() {
    values$refresh_cloud_state <- if(is.null(values$refresh_cloud_state)) 1 else values$refresh_cloud_state + 1
  }

  # cloud data for devices
  get_cloud_data <- function(devices, device_ids, links, linked, unlinked) {
    module_message(ns, "debug", "fetching cloud data")
    # data from cloud
    data <- withProgress(
      message = 'Fetching device data', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_data(access_token = access_token, convert_to_TZ = timezone)

    )

    # safety check
    if (nrow(data) == 0) return(data)

    # device links from data base
    ll_summarize_cloud_data_experiment_links(
      cloud_data = data, experiment_device_links = links,
      linked = linked, unlinked = unlinked)
  }

  get_devices_cloud_data <- eventReactive(values$refresh_cloud_data, {
    get_cloud_data(devices$get_devices(), devices$get_selected_devices(), devices$get_devices_experiments_links(), TRUE, TRUE) %>%
      arrange(device_name, idx)
  })

  # cloud data for experiment
  get_exp_devices_cloud_data <- eventReactive(values$refresh_cloud_data, {

    exp_links <- experiments$get_loaded_experiment_device_links() %>% filter(active)
    device_ids <- experiments$get_loaded_experiment_devices()$device_id
    all_links <- withProgress(
      message = 'Fetching device data', detail = "Querying database for experiment device links...", value = 0.5,
      ll_get_experiment_device_links(
        group_id = group_id, con = pool,
        select = c(exp_device_data_id, exp_id, recording, device_name, data_group, data_idx, active),
        filter = device_id %in% !!device_ids)
    )

    considered_links <- all_links %>% semi_join(exp_links, by = c("device_name", "data_idx"))

    # fetch actual cloud data
    get_cloud_data(experiments$get_loaded_experiment_devices(),
                   device_ids, considered_links, TRUE, FALSE) %>%
      arrange(recording_exp_ids, device_name, idx)
  })

  refresh_cloud_data <- function() {
    values$refresh_cloud_data <- if(is.null(values$refresh_cloud_data)) 1 else values$refresh_cloud_data + 1
  }

  # cloud info
  get_cloud_info <- function(devices, device_ids) {
    module_message(ns, "debug", "fetching cloud info")
    withProgress(
      message = 'Fetching device info', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_info(access_token = access_token, convert_to_TZ = timezone)
    )
  }

  get_devices_cloud_info <- eventReactive(values$refresh_cloud_info, {
    get_cloud_info(devices$get_devices(), devices$get_selected_devices())
  })

  get_exp_devices_cloud_info <- eventReactive(values$refresh_cloud_info, {
    get_cloud_info(experiments$get_loaded_experiment_devices(), experiments$get_selected_loaded_experiment_devices())
  })

  refresh_cloud_info <- function() {
    values$refresh_cloud_info <- if(is.null(values$refresh_cloud_info)) 1 else values$refresh_cloud_info + 1
  }

  # cloudInfoDataServer functions ======
  list(
    # devices cloud state
    get_devices_cloud_state = get_devices_cloud_state,
    get_exp_devices_cloud_state = get_exp_devices_cloud_state,
    refresh_cloud_state = refresh_cloud_state,
    # devices cloud data
    get_devices_cloud_data = get_devices_cloud_data,
    get_exp_devices_cloud_data = get_exp_devices_cloud_data,
    refresh_cloud_data = refresh_cloud_data,
    # devices cloud info
    get_devices_cloud_info = get_devices_cloud_info,
    get_exp_devices_cloud_info = get_exp_devices_cloud_info,
    refresh_cloud_info = refresh_cloud_info
  )


}
