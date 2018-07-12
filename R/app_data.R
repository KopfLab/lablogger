
dataServer <- function(input, output, session, group_id, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values ===
  values <- reactiveValues(
    refresh_experiments = NULL,
    selected_exp_ids = c(),
    refresh_devices = NULL,
    selected_device_ids = c(),
    refresh_device_data_logs = NULL
  )

  # experiments ====
  get_experiments <- reactive({
    req(values$refresh_experiments)
    withProgress(
      message = 'Fetching experiments', detail = "Querying database...", value = 0.5,
      c3_get_experiments(group_id = group_id, con = pool)
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
      c3_get_devices(group_id = group_id, con = pool) %>% arrange(device_name)
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

  # device data logs ====
  get_device_data_logs <- eventReactive(values$refresh_device_data_logs, {
    if (length(values$selected_exp_ids) > 0) {
      withProgress(
        message = 'Fetching device data logs', detail = "Querying database...", value = 0.5,
        c3_get_exp_device_data_logs(
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
    # device data logs
    get_device_data_logs = get_device_data_logs,
    get_device_data_logs_in_time_interval = get_device_data_logs_in_time_interval,
    refresh_device_data_logs = refresh_device_data_logs
  )

}
