
experimentOverviewServer <- function(input, output, session, data_manager, group_id, access_token, pool, timezone) {

  # namespace
  ns <- session$ns

  # data manager
  exp_data_manager <- callModule(dataServer, "exp_data", group_id = group_id, access_token = access_token, pool = pool, timezone = timezone)

  # select experiment ====
  get_experiments_for_dropdown <- reactive({
    experiments <- data_manager$get_experiments() %>% mutate(label = sprintf("%s: %s", exp_id, exp_desc))
    c("Choose an experiment" = "",
      list(
        `Recording` = experiments %>% filter(recording) %>% select(label, exp_id) %>% deframe(),
        `Not recording` = experiments %>% filter(!recording) %>% select(label, exp_id) %>% deframe()
      ))
  })

  # render dropdown
  output$experiment <- renderUI(selectInput(ns("experiment"), label = NULL, choices = get_experiments_for_dropdown()))

  # trigger refresh
  observeEvent(input$experiment_refresh, data_manager$refresh_experiments())

  # update dropdown
  observe({
    updateSelectInput(session, "experiment", choices = get_experiments_for_dropdown(), selected = isolate(exp_data_manager$get_selected_experiments()))
    module_message(ns, "debug", "updating experiments dropdown")
  })

  # load experiment ===
  observeEvent(input$experiment, {
    req(input$experiment)
    if (is.null(data_manager$get_loaded_experiment()) || input$experiment != data_manager$get_loaded_experiment()) {
      load_experiment(input$experiment)
    }
  })

  load_experiment <- function(exp_id) {
    data_manager$load_experiment(exp_id)

    # recording
    recording <- data_manager$is_loaded_experiment_recording()
    toggle("start_recording", condition = !recording)
    toggle("stop_recording", condition = recording)

    # find devices

    # refresh everything
    # exp_data_manager$set_devices(exp_device_links)
    # exp_data_manager$select_devices(exp_device_links$device_id)
    # exp_data_manager$refresh_devices_cloud_state()
    # exp_data_manager$refresh_devices_cloud_data()
    # exp_data_manager$refresh_devices_cloud_info()

    # show tabs
    show("tabs")
  }

  # start/stop recording ====

  observeEvent(input$start_recording, {
    result <- data_manager$start_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success", p(sprintf("Experiment %s is now recording", exp_data_manager$get_selected_experiments())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not start recording.", exp_data_manager$get_selected_experiments())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = FALSE)
    toggle("stop_recording", condition = TRUE)
  })

  observeEvent(input$stop_recording, {
    result <- data_manager$stop_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success", p(sprintf("Experiment %s is now no longer recording", exp_data_manager$get_selected_experiments())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not stop recording.", exp_data_manager$get_selected_experiments())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = TRUE)
    toggle("stop_recording", condition = FALSE)
  })

  # experiment device info
  devices_info <- callModule(deviceInfoServer, "exp_devices_info", exp_data_manager)


}

experimentOverviewUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    default_box(
      title = "Experiments", width = width,
      uiOutput(ns("experiment")),
      footer = div(
        tooltipInput(actionButton, ns("experiment_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh experiments."),
        spaces(1),
        tooltipInput(actionButton, ns("experiment_new"), label = "New experiment", icon = icon("plus"), tooltip = "Add new experiment.")
      )
    ),

    div(id = ns("tabs"),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Configuration",
        h2("Recording"),
        tooltipInput(actionButton, ns("start_recording"), label = "Start Recording",
                     icon = icon("play"), style="color: #fff; background-color: #007f1f; border-color: #2e6da4"),
        tooltipInput(actionButton, ns("stop_recording"), label = "Stop Recording",
                     icon = icon("stop"), style="color: #fff; background-color: #f22e10; border-color: #2e6da4"),
        tooltipInput(actionButton, ns("add_devices"), label = "Add devices", icon = icon("microchip")),
        h2("Devices"),
        deviceInfoUI(ns("exp_devices_info"))
      ),
      tabPanel(
        "Data", br()
      ),
      tabPanel(
        "Logs", br()
      )
    )) %>% hidden()

  )

}
