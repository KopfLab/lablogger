
experimentOverviewServer <- function(input, output, session, experiments) {

  # namespace
  ns <- session$ns

  # select experiment ====
  get_experiments_for_dropdown <- reactive({
    exps <- experiments$get_experiments() %>% mutate(label = sprintf("%s: %s", exp_id, exp_desc))
    c("Choose an experiment" = "",
      list(
        `Recording` = exps %>% filter(recording) %>% select(label, exp_id) %>% deframe(),
        `Not recording` = exps %>% filter(!recording) %>% select(label, exp_id) %>% deframe()
      ))
  })

  # render dropdown
  output$experiment <- renderUI(selectInput(ns("experiment"), label = NULL, choices = get_experiments_for_dropdown()))

  # trigger refresh
  observeEvent(input$experiment_refresh, experiments$refresh_experiments())

  # update dropdown
  observe({
    updateSelectInput(session, "experiment", choices = get_experiments_for_dropdown(), selected = isolate(experiments$get_loaded_experiment()))
    module_message(ns, "debug", "updating experiments dropdown")
  })

  # load experiment ===
  observeEvent(input$experiment, {
    req(input$experiment)
    if (is.null(experiments$get_loaded_experiment()) || input$experiment != experiments$get_loaded_experiment()) {
      load_experiment(input$experiment)
    }
  })

  load_experiment <- function(exp_id) {
    experiments$load_experiment(exp_id)

    # recording
    recording <- experiments$is_loaded_experiment_recording()
    toggle("start_recording", condition = !recording)
    toggle("stop_recording", condition = recording)

    # show tabs
    show("tabs")
  }

  # start/stop recording ====

  observeEvent(input$start_recording, {
    result <- experiments$start_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success", p(sprintf("Experiment %s is now recording", experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not start recording.", experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = FALSE)
    toggle("stop_recording", condition = TRUE)
  })

  observeEvent(input$stop_recording, {
    result <- experiments$stop_experiment()
    if (result$success) {
      showModal(modalDialog(title = "Success", p(sprintf("Experiment %s is now no longer recording", experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Error", p(sprintf("Experiment %s could not stop recording.", experiments$get_loaded_experiment())), footer = modalButton("Close"), fade = FALSE, easyClose = TRUE))
    }
    toggle("start_recording", condition = TRUE)
    toggle("stop_recording", condition = FALSE)
  })

  # experiment device info
  #FIXME: also include device selector (default is having all devices selected)
  #devices_info <- callModule(deviceInfoServer, "exp_devices_info", exp_data_manager)


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
        # FIXME
        tooltipInput(actionButton, ns("experiment_new"), label = "New experiment", icon = icon("plus"), tooltip = "Add new experiment. NOT IMPLEMENTED YET")
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
        # FIXME
        tooltipInput(actionButton, ns("add_devices"), label = "Add device links", icon = icon("microchip"), tooltip = "Add additional device links. NOT IMPLEMENETED YET")
        #h2("Devices")
        #FIXME: also include device selector
        #deviceInfoUI(ns("exp_devices_info"))
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
