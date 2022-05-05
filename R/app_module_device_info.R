
#' Device Info Server
deviceInfoServer <- function(input, output, session, get_cloud_state, refresh_cloud_state, get_cloud_data, refresh_cloud_data, get_cloud_info, refresh_cloud_info, get_device_ids, get_state_logs, refresh_state_logs) {


  # namespace
  ns <- session$ns

  # fetches =======

  # fetch state
  observeEvent(input$fetch_state, refresh_cloud_state())

  # fetch data
  observeEvent(input$fetch_data, refresh_cloud_data())

  # fetch info
  observeEvent(input$fetch_info, refresh_cloud_info())

  # fetch logs
  observeEvent(input$fetch_logs, refresh_state_logs())

  # fetch all
  observe({
    fetches <-
      (input$fetch_state_all %>% { if(is.null(.)) 0L else . }) +
      (input$fetch_data_all %>% { if(is.null(.)) 0L else . }) +
      (input$fetch_info_all %>% { if(is.null(.)) 0L else . }) +
      (input$fetch_logs_all %>% { if(is.null(.)) 0L else . })
    req(fetches > 0)
    module_message(ns, "debug", "fetching all cloud and database info")
    isolate({
      refresh_cloud_state()
      refresh_cloud_data()
      refresh_cloud_info()
      refresh_state_logs()
    })
  })

  # state logs table =====
  is_device_selected <- reactive(length(get_device_ids()) > 0)
  generate_state_logs_table <- eventReactive(get_state_logs(), {
    logs <- get_state_logs()
    validate(need(nrow(logs) > 0, "No state logs available."))
    logs <- logs %>%
      select(log_datetime, everything()) %>%
      select(-device_id) %>%
      mutate(log_datetime = format(log_datetime, "%Y-%m-%d %H:%M:%S"))
    return(logs)
  })
  output$logs_table <- DT::renderDataTable({
    DT::datatable(
      generate_state_logs_table(),
      options = list(orderClasses = TRUE, order = list(1, "desc"),
                     lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10),
      filter = "bottom"
    )
  })

  # live state table ====
  output$state_table <- renderTable({
    state <- get_cloud_state()
    validate(need(nrow(state) > 0 && "datetime" %in% names(state), "No live state information available."))
    module_message(ns, "debug", "rendering cloud state table")
    vars_start <- which(names(state) == "version")
    state %>% arrange(device_name) %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Live state posted at` = datetime, Version = version, vars_start:ncol(state))
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # live data table ======
  # this is only really used on devices screen, should probably be part of device_manager
  # like the experiment counterpart in experiment_manager!
  output$data_table <- renderTable({
    data <- get_cloud_data()
    validate(need(nrow(data) > 0, "No live data available."))
    data <- data %>%
      mutate(datetime = ifelse(!is.na(datetime), format(datetime), error)) %>%
      select(Name = device_name, `Live data posted at` = datetime,
             `Exp IDs (recording)` = recording_exp_ids, `Exp IDs (not recording)` = non_recording_exp_ids,
             idx, key, value, units,
             raw_serial, raw_serial_errors)
    module_message(ns, "debug", "rendering cloud data table")
    return(apply_live_data_table_options(data))
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # column options for live data table
  apply_live_data_table_options <- function(data) {
    if (!"serial" %in% input$data_table_options)
      data <- select(data, -raw_serial, -raw_serial_errors)
    if (!"r_exps" %in% input$data_table_options)
      data <- select(data, -`Exp IDs (recording)`)
    if (!"nr_exps" %in% input$data_table_options)
      data <- select(data, -`Exp IDs (not recording)`)
    return(data)
  }

  # live info table =====
  output$info_table <- renderTable({
    info <- get_cloud_info()
    validate(need(nrow(info) > 0, "No device information available."))
    module_message(ns, "debug", "rendering cloud info table")
    info %>%
      # only show db-registered devices' cloud info
      filter(registered) %>%
      arrange(device_name) %>%
      mutate(last_heard = format(last_heard)) %>%
      select(Name = device_name, `Last heard from` = last_heard, Connected = connected, Status = status, Firmware = system_firmware_version)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # return functions
  list(
    trigger_live_data_table_options = reactive(input$data_table_options),
    apply_live_data_table_options = apply_live_data_table_options
  )

}

deviceLogsUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    default_box(
      title = "Device State Logs", width = width,
      style = paste0("min-height: 300px;"),
      DT::dataTableOutput(ns("logs_table")) %>% withSpinner(type = 5, proxy.height = "300px"),
      footer = div(
        tooltipInput(actionButton, ns("fetch_logs"), "Fetch Logs", icon = icon("cloud-download-alt"),
                     tooltip = "Fetch the most recent state logs from the data base.")
      )
    )

  )

}


deviceInfoUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    # live info
    default_box(
      title = "Live Device Info", width = width,
      style = paste0("min-height: 130px;"),
      tableOutput(ns("info_table")) %>% withSpinner(type = 5, proxy.height = "130px"),
      footer = div(
        tooltipInput(actionButton, ns("fetch_info"), "Fetch Info", icon = icon("cloud-download-alt"),
                     tooltip = "Fetch the most recent device information from the cloud.")
      )
    )

  )

}

deviceStateUI <- function(id, width = 12) {
  ns <- NS(id)

  tagList(

    # live state
    default_box(
      style = paste0("min-height: 130px;"),
      title = "Live Device State", width = width,
      tableOutput(ns("state_table")) %>% withSpinner(type = 5, proxy.height = "130px"),
      footer =
        div(
          tooltipInput(actionButton, ns("fetch_state"), "Fetch State", icon = icon("cloud-download-alt"),
                       tooltip = "Fetch the most recent state information from the cloud.")
        )
    )
  )
}

# allow output as parameter to re-use this function for the experiment device data links
deviceDataUI <- function(
  id, width = 12, selected_options = c("r_exps", "serial"),
  ns = NS(id),
  title = "Live Device Data",
  output = withSpinner(tableOutput(ns("data_table")), type = 5, proxy.height = "130px"),
  add_footer = tagList()) {

  tagList(
    # live data
    default_box(
      title = title, width = width,
      style = paste0("min-height: 130px;"),
      checkboxGroupInput(ns("data_table_options"), NULL,
                         c("Experiment Links (recording)" = "r_exps",
                           "Experiment Links (not recording)" = "nr_exps",
                           "Raw Serial Data" = "serial"),
                         selected = selected_options,
                         inline = TRUE),
      output,
      footer = div(
        tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download-alt"),
                     tooltip = "Fetch the most recent live data and experiment links from the cloud."),
        spaces(1),
        add_footer
      )
    )
  )
}

deviceFetchAllUI <- function(id) {
  ns <- NS(id)
  tooltipInput(actionButton, ns("fetch_data_all"), "Fetch All (Data, State, Info, Logs)", icon = icon("cloud-download-alt"),
               tooltip = "Fetch all device information from the cloud and database.")
}


