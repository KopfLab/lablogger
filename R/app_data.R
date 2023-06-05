# LINKS =====

experimentDeviceLinksDataServer <- function(input, output, session, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_links = NULL
  )

  # get links
  get_links <- reactive({
    req(values$refresh_links)
    withProgress(
      message = 'Fetching data links', detail = "Querying database for experiment device links...", value = 0.5,
      ll_get_experiment_device_links(
        group_id = group_id, con = pool,
        select = c(
          exp_device_data_id, data_idx, active, # link info
          exp_id, recording, # exp info
          device_id, device_name, device_type_desc, particle_id # device info
        ),
        filter = active)
    ) %>%
      # introduce a unique id for the link combinations
      mutate(..id.. = paste(device_id, data_idx, sep = "_")) %>%
      # arrange by device name and index
      arrange(device_name, data_idx)
  })

  # initialize links
  init_links <- function() {
    if (is.null(values$refresh_links))  {
      module_message(ns, "debug", "initializing links")
      values$refresh_links <- 1
    }
  }

  # refresh links (only refreshes if links are already initalized!)
  refresh_links <- function() {
    if (!is.null(values$refresh_links)) {
      module_message(ns, "debug", "setting links refresh flag")
      values$refresh_links <- values$refresh_links + 1
    }
  }

  #  LINKS functions ====
  list(
    get_links = get_links,
    refresh_links = refresh_links,
    init_links = init_links
  )
}

# EXPERIMENTS ====

experimentsDataServer <- function(input, output, session, links, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values ----
  values <- reactiveValues(
    refresh_experiments = NULL,
    selected_exp_ids = c(), # multi selection experiments
    loaded_exp_id = NULL # single selection experiments
  )

  # LOAD =====

  # get_experiments ----
  get_experiments <- reactive({
    req(values$refresh_experiments)
    withProgress(
      message = 'Fetching experiments', detail = "Querying database...", value = 0.5,
      ll_get_experiments(group_id = group_id, con = pool, convert_to_TZ = timezone)
    )
  })

  init_experiments <- function() {
    if (is.null(values$refresh_experiments)) {
      module_message(ns, "debug", "initializing experiments")
      values$refresh_experiments <- 1
    }
  }

  refresh_experiments <- function() {
    if (!is.null(values$refresh_experiments)) {
      module_message(ns, "debug", "setting experiments refresh flag")
      values$refresh_experiments <- values$refresh_experiments + 1
      links$refresh_links()
    }
  }

  # single experiment
  load_experiment <- function(exp_id) {
    stopifnot(length(exp_id) == 1)
    if (!identical(values$loaded_exp_id, exp_id)) {
      module_message(ns, "debug", glue("loading exp id '{exp_id}'"))
      values$loaded_exp_id <- exp_id
      if (is_loaded_experiment_archived()) {
        # archived
        module_message(ns, "debug", glue("this experiment ('{values$loaded_exp_id}') is archived"))
      }
    }
  }

  # SELECT =====

  select_experiments <- function(exp_ids) {
    if (!identical(values$selected_exp_ids, exp_ids)) {
      module_message(
        ns, "debug",
        glue::glue("selecting exp ids '",
        if(!is.null(exp_ids)) paste(exp_ids, collapse = "', '") else 'none',
        "'")
      )
      values$selected_exp_ids <- exp_ids
    }
  }

  # ADD =====

  # create new experiment
  add_experiment <- function(exp_id) {
    stopifnot(length(exp_id) == 1)
    withProgress(
      message = 'Creating experiment', detail = glue::glue("Setting up new experiment '{exp_id}'..."), value = 0.5,
      ll_add_experiment(exp_id = exp_id, exp_desc = "", group_id = group_id, con = pool)
    )
  }

  # LINKS ====

  # get_experiment_device_links -------
  get_experiment_device_links <- reactive({
    if (!is.null(values$loaded_exp_id)) {
      links$init_links() # make sure links are initialized
      if (nrow(links$get_links() > 0)) {
        experiment_links <- links$get_links() %>%
          filter(exp_id == values$loaded_exp_id)
        module_message(
          ns, "debug",
          glue::glue("found {nrow(experiment_links)} device links for experiment ",
                     "'{values$loaded_exp_id}'")
        )
        return(experiment_links)
      }
    }
    return(tibble(device_id = integer(0), data_idx = integer(0)))
  })

  # get_experiment_device_links_all -----
  # all experiment device links that overlap with this experiment
  get_experiment_device_links_all <- reactive({
    links$init_links() # make sure links are initialized
    semi_join(
      links$get_links(),
      get_experiment_device_links(),
      by = c("device_id", "data_idx")
    )
  })

  # get_experiment_devices -----
  get_experiment_devices <- reactive({
    if (nrow(get_experiment_device_links()) > 0) {
      get_experiment_device_links() %>%
        select(device_id, device_name, particle_id, device_type_desc) %>%
        unique()
    } else {
      tibble(device_id = integer(0), device_name = character(0),
             particle_id = character(0), device_type_desc = character(0))
    }
  })

  # add new links
  add_experiment_device_links <- function(device_links) {
    ll_add_experiment_devices(
      exp_id = values$loaded_exp_id,
      experiment_devices = mutate(device_links, exp_id = values$loaded_exp_id),
      group_id = group_id,
      con = pool)
  }

  # delte links
  delete_experiment_device_links <- function(device_link_ids) {
    ll_remove_experiment_device_links(
      exp_id = values$loaded_exp_id,
      exp_device_data_ids = device_link_ids,
      con = pool)
  }

  # INFO =======
  # get_loaded_experiment_info ----
  get_loaded_experiment_info <- reactive({
    filter(get_experiments(), exp_id == !!values$loaded_exp_id)
  })

  update_loaded_experiment_info <- function(exp_desc, exp_notes) {
    ll_update_experiment_info(exp_id = values$loaded_exp_id, exp_desc = exp_desc,
                              exp_notes = exp_notes, group_id = group_id, con = pool)
  }

  # is_loaded_experiment_archived -----
  is_loaded_experiment_archived <- reactive({
    filter(get_experiments(), exp_id == !!values$loaded_exp_id)$archived
  })

  # START / STOP ======

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

  # FUNCTIONS ====
  list(
    get_experiments = get_experiments,
    init_experiments = init_experiments,
    refresh_experiments = refresh_experiments,
    select_experiments = select_experiments,
    get_selected_experiments = reactive(values$selected_exp_ids),
    add_experiment = add_experiment,
    load_experiment = load_experiment,
    get_loaded_experiment = reactive(values$loaded_exp_id),
    get_loaded_experiment_info = get_loaded_experiment_info,
    updated_loaded_experiment_info = update_loaded_experiment_info,

    get_loaded_experiment_device_links = get_experiment_device_links,
    get_loaded_experiment_device_links_all = get_experiment_device_links_all,
    get_loaded_experiment_devices = get_experiment_devices,
    get_loaded_experiment_device_ids = reactive(get_experiment_devices()$device_id),
    add_experiment_device_links = add_experiment_device_links,
    delete_experiment_device_links = delete_experiment_device_links,

    is_loaded_experiment_recording = reactive(filter(get_experiments(), exp_id == !!values$loaded_exp_id)$recording),
    is_loaded_experiment_archived = is_loaded_experiment_archived,
    start_experiment = start_experiment,
    stop_experiment = stop_experiment
  )

}

# DEVICES ======

devicesDataServer <- function(input, output, session, links, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_devices = NULL,
    selected_device_ids = c()
  )

  # get_devices -----
  get_devices <- reactive({
    req(values$refresh_devices)
    withProgress(
      message = 'Fetching devices', detail = "Querying database...", value = 0.5,
      ll_get_devices(group_id = group_id, con = pool) %>% arrange(device_name)
    )
  })

  init_devices <- function() {
    if (is.null(values$refresh_devices)) {
      module_message(ns, "debug", "initializing devices")
      values$refresh_devices <- 1
    }
  }

  refresh_devices <- function() {
    if (!is.null(values$refresh_devices)) {
      module_message(ns, "debug", "setting devices refresh flag")
      values$refresh_devices <- values$refresh_devices + 1
      links$refresh_links()
    }
  }

  select_devices <- function(device_ids) {
    if (!identical(values$selected_device_ids, device_ids)) {
      module_message(
        ns, "debug",
        glue::glue(
          "selecting device ids ",
          if(!is.null(device_ids)) paste(device_ids, collapse = ', ')
          else "'none'")
      )
      values$selected_device_ids <- device_ids
    }
  }

  # get_devices_experiment_links -----
  get_devices_experiments_links <- reactive({
    if (length(values$selected_device_ids) > 0) {
      # make sure links are initialized
      links$init_links()
      if (nrow(links$get_links() > 0)) {
        device_links <- links$get_links() %>%
          filter(device_id %in% !!values$selected_device_ids & active)
        module_message(
          ns, "debug",
          glue::glue("found {nrow(device_links)} device links for device ids ",
                     paste(values$selected_device_ids, collapse = ", "))
        )
        return(device_links)
      }
    }
    return(tibble())
  })

  # devicesDataServer functions ====
  list(
    get_devices = get_devices,
    init_devices = init_devices,
    refresh_devices = refresh_devices,
    select_devices = select_devices,
    get_selected_devices = reactive({values$selected_device_ids}),
    get_devices_experiments_links = get_devices_experiments_links
  )
}

# LOGS ======

# FIXME: this should just be logsDataServer because it handles data and state logs
datalogsDataServer <- function(input, output, session, experiments, devices, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_device_state_logs = NULL,
    refresh_experiment_device_state_logs = NULL,
    refresh_experiments_data_logs = NULL,
    refresh_experiment_data_logs = NULL
  )

  # main getter function for device state logs
  get_device_state_logs <- function(device_ids) {
    if (length(device_ids) > 0) {
      filter_quo <- quo(device_id %in% !!device_ids)
      state_logs <- withProgress(
        message = 'Fetching device state logs', detail = "Querying database...", value = 0.5,
        ll_get_device_state_logs(
          group_id = group_id,
          filter = !!filter_quo,
          con = pool,
          convert_to_TZ = timezone
        )
      )
      return(state_logs)
    } else {
      return(tibble())
    }
  }

  refresh_state_logs <- function() {
    values$refresh_device_state_logs <-
      if(is.null(values$refresh_device_state_logs)) 1
    else values$refresh_device_state_logs + 1
  }

  refresh_experiment_state_logs <- function() {
    values$refresh_experiment_device_state_logs <-
      if(is.null(values$refresh_experiment_device_state_logs)) 1
    else values$refresh_experiment_device_state_logs + 1
  }

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
      return(tibble())
    }
  }

  refresh_data_logs <- function() {
    values$refresh_experiments_data_logs <-
      if(is.null(values$refresh_experiments_data_logs)) 1
      else values$refresh_experiments_data_logs + 1
  }

  refresh_experiment_data_logs <- function() {
    values$refresh_experiment_data_logs <-
      if(is.null(values$refresh_experiment_data_logs)) 1
    else values$refresh_experiment_data_logs + 1
  }

  # datalogsDataServer functions =====
  list(
    # state logs
    get_devices_state_logs = eventReactive(
      values$refresh_device_state_logs,
      get_device_state_logs(devices$get_selected_devices())
    ),
    refresh_state_logs = refresh_state_logs,
    get_experiment_devices_state_logs = eventReactive(
      values$refresh_experiment_device_state_logs,
      get_device_state_logs(experiments$get_loaded_experiment_device_ids())
    ),
    refresh_experiment_state_logs = refresh_experiment_state_logs,
    # multi experiments data logs
    get_devices_experiments_data_logs = eventReactive( # Q: is this function used anywhere?
      values$refresh_experiments_data_logs,
      get_device_data_logs(experiments$get_selected_experiments(), devices$get_selected_devices())),
    get_experiments_data_logs = eventReactive(
      values$refresh_experiments_data_logs,
      get_device_data_logs(experiments$get_selected_experiments())),
    refresh_data_logs = refresh_data_logs,
    # single experiment data logs
    get_experiment_data_logs = eventReactive(
      {
        # trigger either if new experiment is loaded or the refresh event happens
        #experiments$get_loaded_experiment()
        values$refresh_experiment_data_logs
      },
      get_device_data_logs(experiments$get_loaded_experiment())),
    refresh_experiment_data_logs = refresh_experiment_data_logs
  )
}

# CLOUD =====

cloudInfoDataServer <- function(input, output, session, experiments, devices, links, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    refresh_cloud_state = NULL,
    refresh_cloud_data = NULL,
    refresh_cloud_info = NULL
  )

  # CLOUD STATE ----
  get_cloud_state <- function(devices, device_ids = devices$device_id) {
    withProgress(
      message = 'Fetching device cloud state', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_state(access_token = access_token, convert_to_TZ = timezone, spread = TRUE)
    )
  }

  # get_devices_cloud_state -----
  get_devices_cloud_state <- eventReactive(values$refresh_cloud_state, {
    validate(
      need(!is.null(devices$get_devices()), "No devices available.") %then%
        need(length(devices$get_selected_devices()) > 0, "No device selected.")
    )
    module_message(ns, "debug", "fetching devices cloud state")
    get_cloud_state(devices$get_devices(), devices$get_selected_devices())
  })

  # get_exp_devices_cloud_state ----
  get_exp_devices_cloud_state <- eventReactive(values$refresh_cloud_state, {
    validate(
      need(length(experiments$get_loaded_experiment_device_ids()) > 0, "This experiment has no associated devices yet.")
    )
    module_message(ns, "debug", "fetching experiment devices cloud state")
    get_cloud_state(experiments$get_loaded_experiment_devices())
  })

  refresh_cloud_state <- function() {
    values$refresh_cloud_state <- if(is.null(values$refresh_cloud_state)) 1 else values$refresh_cloud_state + 1
  }

  # CLOUD DATA =====
  get_cloud_data <- function(devices, device_ids = devices$device_id, links, linked, unlinked) {
    data <- withProgress(
      message = 'Fetching device cloud data', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_data(access_token = access_token, convert_to_TZ = timezone)
    )
    ll_summarize_cloud_data_experiment_links(
      cloud_data = data, experiment_device_links = links,
      linked = linked, unlinked = unlinked) %>%
      mutate(..id.. = paste(device_id, idx, sep = "_")) %>%
      arrange(device_name, idx)
  }

  # get_devices_cloud_data ----
  get_devices_cloud_data <- eventReactive(values$refresh_cloud_data, {
    validate(
      need(!is.null(devices$get_devices()), "No devices available.") %then%
        need(length(devices$get_selected_devices()) > 0, "No device selected.")
    )
    # get device cloud data
    module_message(ns, "debug", "fetching device cloud data")
    get_cloud_data(
      devices = devices$get_devices(),
      device_ids = devices$get_selected_devices(),
      links = devices$get_devices_experiments_links(),
      linked = TRUE, unlinked = TRUE
    )
  })

  # get_exp_devices_cloud_data ----
  get_exp_devices_cloud_data <- eventReactive(values$refresh_cloud_data, {
    validate(
      need(!is.null(experiments$get_loaded_experiment_devices()), "No devices available.") %then%
        need(!is.null(experiments$get_loaded_experiment_device_links_all()), "No device links available.")
    )
    module_message(ns, "debug", "fetching experiment devices cloud data")
    get_cloud_data(
      devices = experiments$get_loaded_experiment_devices(),
      links = experiments$get_loaded_experiment_device_links_all(),
      linked = TRUE, unlinked = FALSE
    )
  })

  # has_cloud_data ----
  has_cloud_data <- reactive({
    return(!is.null(values$refresh_cloud_data))
  })

  refresh_cloud_data <- function() {
    values$refresh_cloud_data <- if(is.null(values$refresh_cloud_data)) 1 else values$refresh_cloud_data + 1
  }

  reset_cloud_data <- function() {
    values$refresh_cloud_data <- NULL
  }



  # CLOUD INFO ====
  get_cloud_info <- function(devices, device_ids = devices$device_id) {
    withProgress(
      message = 'Fetching device cloud info', detail = "Querying device cloud...", value = 0.5,
      devices %>%
        filter(device_id %in% !!device_ids) %>%
        ll_get_devices_cloud_info(access_token = access_token, convert_to_TZ = timezone, include_unregistered = TRUE)
    )
  }

  # get_all_devices_cloud_info ----
  get_all_devices_cloud_info <- eventReactive(values$refresh_cloud_info, {
    validate(
      need(!is.null(devices$get_devices()), "No devices available.")
    )
    module_message(ns, "debug", "fetching cloud info for all devices")
    get_cloud_info(devices$get_devices())
  })

  # get_devices_cloud_info ----
  get_devices_cloud_info <- eventReactive(values$refresh_cloud_info, {
    validate(
      need(!is.null(devices$get_devices()), "No devices available.") %then%
        need(length(devices$get_selected_devices()) > 0, "No device selected.")
    )
    module_message(ns, "debug", "fetching cloud info for devices")
    get_cloud_info(devices$get_devices(), devices$get_selected_devices())
  })

  # get_exp_devices_cloud_info -----
  get_exp_devices_cloud_info <- eventReactive(values$refresh_cloud_info, {
    validate(
      need(length(experiments$get_loaded_experiment_device_ids()) > 0, "This experiment has no associated devices yet.")
    )
    module_message(ns, "debug", "fetching cloud info experiment devices")
    get_cloud_info(experiments$get_loaded_experiment_devices())
  })

  refresh_cloud_info <- function() {
    values$refresh_cloud_info <- if(is.null(values$refresh_cloud_info)) 1 else values$refresh_cloud_info + 1
  }

  # ALL CLOUD FUNCTIONS ======
  list(
    # devices cloud state
    get_devices_cloud_state = get_devices_cloud_state,
    get_exp_devices_cloud_state = get_exp_devices_cloud_state,
    refresh_cloud_state = refresh_cloud_state,
    # devices cloud data
    get_cloud_data = get_cloud_data,
    get_devices_cloud_data = get_devices_cloud_data,
    get_exp_devices_cloud_data = get_exp_devices_cloud_data,
    refresh_cloud_data = refresh_cloud_data,
    reset_cloud_data = reset_cloud_data,
    has_cloud_data = has_cloud_data,
    # devices cloud info
    get_all_devices_cloud_info = get_all_devices_cloud_info,
    get_devices_cloud_info = get_devices_cloud_info,
    get_exp_devices_cloud_info = get_exp_devices_cloud_info,
    refresh_cloud_info = refresh_cloud_info
  )


}
