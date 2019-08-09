
experimentManagerServer <- function(input, output, session, dm_experiments, dm_cloudinfo, dm_datalogs, timezone, access_token) {

  # namespace
  ns <- session$ns

  # select experiment ====
  get_experiments_for_dropdown <- reactive({
    exps <- dm_experiments$get_experiments()
    if (nrow(exps) == 0) return ("Choose an experiment" = "")
    exps <- exps %>% mutate(label = sprintf("%s: %s", exp_id, exp_desc))
    c("Choose an experiment" = "",
      list(
        `Recording` = exps %>% filter(!archived, recording) %>% select(label, exp_id) %>% deframe(),
        `Not recording` = exps %>% filter(!archived, !recording) %>% select(label, exp_id) %>% deframe(),
        `Archived` = exps %>% filter(archived) %>% select(label, exp_id) %>% deframe()
      ))
  })

  # render dropdown
  output$experiment <- renderUI(selectInput(ns("experiment"), label = NULL, choices = get_experiments_for_dropdown()))

  # trigger refresh
  observeEvent(input$experiment_refresh, dm_experiments$refresh_experiments())

  # update dropdown
  observe({
    updateSelectInput(session, "experiment", choices = get_experiments_for_dropdown(), selected = isolate(dm_experiments$get_loaded_experiment()))
    module_message(ns, "debug", "updating experiments dropdown")
  })

  # load experiment ===
  observeEvent(input$experiment, {
    req(input$experiment)
    if (is.null(dm_experiments$get_loaded_experiment()) || input$experiment != dm_experiments$get_loaded_experiment()) {
      load_experiment(input$experiment)
    }
  })

  load_experiment <- function(exp_id) {
    dm_experiments$load_experiment(exp_id)

    # archived
    if (dm_experiments$is_loaded_experiment_archived()) {

      # archived exp - FIXME: allow access to data here?
      hide("tabs")
      show("archived_msg")

    } else {

      # recording
      recording <- dm_experiments$is_loaded_experiment_recording()
      toggle("start_recording", condition = !recording)
      toggle("stop_recording", condition = recording)

      # info
      if (!is.na(dm_experiments$get_loaded_experiment_info()$exp_desc))
        updateTextInput(session, "exp_desc", value = dm_experiments$get_loaded_experiment_info()$exp_desc)
      else
        updateTextInput(session, "exp_desc", value = "", placeholder = "Add a succint description for this experiment.")

      if (!is.na(dm_experiments$get_loaded_experiment_info()$exp_notes))
        updateTextInput(session, "exp_notes", value = dm_experiments$get_loaded_experiment_info()$exp_notes)
      else
        updateTextInput(session, "exp_notes", value = "", placeholder = "Keep notes about this experiment.")

      # devices - select all by default
      dm_experiments$select_loaded_experiment_devices(dm_experiments$get_loaded_experiment_devices()$device_id)

      # cloud data - fetch right away? (alternatively implement a reset for the device data/state/info tables)
      dm_cloudinfo$refresh_cloud_data()

      # show tabs
      hide("archived_msg")
      show("tabs")
    }
  }

  output$exp_ID <- renderText({
    validate(need(dm_experiments$get_loaded_experiment(), "no experiment selected"))
    dm_experiments$get_loaded_experiment()
  })

  # save exp info ====

  observeEvent(input$save_info, {
    success <- dm_experiments$updated_loaded_experiment_info(exp_desc = input$exp_desc, exp_notes = input$exp_notes)
    if (success) {
      showModal(modalDialog(
        title = "Success",
        h4(sprintf("Experiment '%s' was updated succesfully.", dm_experiments$get_loaded_experiment())),
        footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(
        title = "A problem occurred",
        h4(sprintf("Something went wrong updating experiment '%s'.", dm_experiments$get_loaded_experiment())),
        footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    dm_experiments$refresh_experiments()
  })

  # start/stop recording ====

  observeEvent(input$start_recording, {
    result <- dm_experiments$start_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success",
                            p(strong(sprintf("Experiment %s is now recording.", dm_experiments$get_loaded_experiment()))),
                            p("Please note that only data from linked devices that have 'data-log' turned on will actually be recorded. Check the 'Live State' on the 'Devices' tab for an overview of your devices' status."), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not start recording.", dm_experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = FALSE)
    toggle("stop_recording", condition = TRUE)
  })

  observeEvent(input$stop_recording, {
    result <- dm_experiments$stop_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success", p(sprintf("Experiment %s is now no longer recording", dm_experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not stop recording.", dm_experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = TRUE)
    toggle("stop_recording", condition = FALSE)
  })

  # experiment devices =====
  # # NOTE: always show info for all experiment devices to simplify use of this screen
  # callModule(
  #   deviceSelectorServer, "exp_devices",
  #   get_devices = dm_experiments$get_loaded_experiment_devices,
  #   get_selected_devices = dm_experiments$get_selected_loaded_experiment_devices,
  #   refresh_devices = dm_experiments$load_experiment_device_links,
  #   select_devices = dm_experiments$select_loaded_experiment_devices,
  #   access_token = access_token)

  # device links
  output$data_table <- renderTable({
    data <- dm_cloudinfo$get_exp_devices_cloud_data()
    validate(need(nrow(data) > 0, "No live data available."))
    data <- data %>%
      mutate(datetime = ifelse(!is.na(datetime), format(datetime), error)) %>%
      select(Name = device_name, `Live data posted at` = datetime,
             `Exp IDs (recording)` = recording_exp_ids, `Exp IDs (not recording)` = non_recording_exp_ids,
             idx, key, value, units,
             raw_serial, raw_serial_errors)
    module_message(ns, "debug", "rendering cloud data table")

    if (!"serial" %in% input$data_table_options)
      data <- select(data, -raw_serial, -raw_serial_errors)
    if (!"r_exps" %in% input$data_table_options)
      data <- select(data, -`Exp IDs (recording)`)
    if (!"nr_exps" %in% input$data_table_options)
      data <- select(data, -`Exp IDs (not recording)`)

    return(data)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # device control =====
  control <- callModule(deviceCommandsServer, "control", get_devices = dm_experiments$get_loaded_experiment_devices, access_token = access_token)

  # devices info =====
  callModule(
    deviceInfoServer, "devices_info",
    get_cloud_state = dm_cloudinfo$get_exp_devices_cloud_state,
    refresh_cloud_state = dm_cloudinfo$refresh_cloud_state,
    get_cloud_data = dm_cloudinfo$get_exp_devices_cloud_data,
    refresh_cloud_data = dm_cloudinfo$refresh_cloud_data,
    refresh_experiment_device_links = dm_experiments$load_experiment_device_links,
    get_cloud_info = dm_cloudinfo$get_exp_devices_cloud_info,
    refresh_cloud_info = dm_cloudinfo$refresh_cloud_info,
    get_devices = dm_experiments$get_loaded_experiment_devices, #dm_experiments$get_selected_loaded_experiment_devices,
    get_state_logs = dm_datalogs$get_experiment_devices_state_logs,
    refresh_state_logs = dm_datalogs$refresh_experiment_state_logs
  )

  # experiment data ======

  callModule(
    dataPlotServer, "exp_data_plot", timezone = timezone,
    get_experiments = dm_experiments$get_loaded_experiment,
    get_data_logs = dm_datalogs$get_experiment_data_logs,
    refresh_data_logs = dm_datalogs$refresh_experiment_data_logs,
    reset_plot = eventReactive(dm_experiments$get_loaded_experiment(), runif(1))
  )

}

experimentManagerUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    default_box(
      # selection ====
      title = "Experiments", width = width,
      uiOutput(ns("experiment")),
      footer = div(
        tooltipInput(actionButton, ns("experiment_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh experiments."),
        spaces(1),
        # FIXME
        tooltipInput(actionButton, ns("experiment_new"), label = "New experiment", icon = icon("plus"), tooltip = "Add new experiment. NOT IMPLEMENTED YET"),
        spaces(1),
        deviceControlButton(ns("control"), label = "Control Devices"),
        spaces(1),
        actionButton(ns("start_recording"), label = "Start Recording",
                     icon = icon("play"), style="color: #fff; background-color: #007f1f; border-color: #2e6da4") %>% hidden(),
        actionButton(ns("stop_recording"), label = "Stop Recording",
                     icon = icon("stop"), style="color: #fff; background-color: #f22e10; border-color: #2e6da4") %>% hidden()
      )
    ),

    div(id = ns("archived_msg"),
        h2("Sorry, this experiment is archived and can not be reconfigured. Please use the 'Data' menu on the left to view the data.")
        ) %>% hidden(),

    div(id = ns("tabs"),
    tabsetPanel(
      type = "tabs", selected = "devices",
      # data ===
      tabPanel(
        value = "data",
        "Data", br(),
        dataPlotUI(ns("exp_data_plot"))
      ),
      # info =====
      tabPanel(
        value = "info",
        "Info",
        br(),
        default_box(
          title = "Experiment Information", width = 12,
          h4("ID:", textOutput(ns("exp_ID"), inline = TRUE)),
          h4("Description:"),
          textAreaInput(ns("exp_desc"), NULL, cols = 50, rows = 5, resize = "none"),
          h4("Notes:"),
          textAreaInput(ns("exp_notes"), NULL, width = "100%", rows = 10, resize = "both"),
          footer = actionButton(ns("save_info"), label = "Save", icon = icon("save"))
        )
      ),
      # devices ====
      tabPanel(
        value = "devices",
        "Devices",
        br(),
        spaces(3),
        tooltipInput(actionButton, ns("add_devices"), label = "Configure Device Links", icon = icon("microchip"), tooltip = "Add additional device links. NOT IMPLEMENETED YET"),
        spaces(1),
        deviceFetchAllUI(ns("devices_info")),
        br(), br(),
        #deviceDataUI(ns("devices_info"), selected_options = "r_exps", include_fetch_all = FALSE),
        deviceLinksUI(ns(NULL), selected_options = "r_exps"),
        deviceStateUI(ns("devices_info"), include_fetch_all = FALSE),
        deviceInfoUI(ns("devices_info"), include_fetch_all = FALSE),
        deviceLogsUI(ns("devices_info"), include_fetch_all = FALSE)
      )
    )) %>% hidden()

  )

}

# note this is an adapted version of the app_module_device_info.R deviceDataUI function
# easier to adapt than to make the other function serve both purposes
deviceLinksUI <- function(id, width = 12, selected_options = c("r_exps", "serial")) {

  ns <- NS(id)

  tagList(
    # live data
    default_box(
      title = "Device Links & Live Data", width = width,
      style = paste0("min-height: 130px;"),
      checkboxGroupInput(ns("data_table_options"), NULL,
                         c("Experiment Links (recording)" = "r_exps",
                           "Experiment Links (not recording)" = "nr_exps",
                           "Raw Serial Data" = "serial"),
                         selected = selected_options,
                         inline = TRUE),

      tableOutput(ns("data_table")) %>% withSpinner(type = 5, proxy.height = "130px"),
      footer = div(
        tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent live data and experiment links from the cloud.")
      )
    )
  )
}
