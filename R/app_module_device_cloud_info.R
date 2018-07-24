
deviceCloudInfoServer <- function(input, output, session, data_manager) {


  # namespace
  ns <- session$ns

  # fetch state
  observeEvent(input$fetch_state, data_manager$refresh_devices_cloud_state())

  # fetch data
  observeEvent(input$fetch_data, data_manager$refresh_devices_cloud_data())

  # fetch info
  observeEvent(input$fetch_info, data_manager$refresh_devices_cloud_info())

  # fetch all
  observe({
    input$fetch_state_all
    input$fetch_data_all
    input$fetch_info_all
    isolate({
      data_manager$refresh_devices_cloud_state()
      data_manager$refresh_devices_cloud_data()
      data_manager$refresh_devices_cloud_info()
    })
  })

  # state table
  output$state_table <- renderTable({
    state <- data_manager$get_devices_cloud_state()
    validate(need(nrow(state) > 0, "No state information available."))
    module_message(ns, "debug", "rendering cloud state table")
    vars_start <- which(names(state) == "version")
    state %>% arrange(device_name) %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Last state change` = datetime, Version = version, vars_start:ncol(state))
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # data table
  output$data_table <- renderTable({
    data <- data_manager$get_devices_cloud_data()
    validate(need(nrow(data) > 0, "No live device data available."))
    module_message(ns, "debug", "rendering cloud data table")
    data %>% arrange(device_name) %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Last data update` = datetime, idx, key, value, units, raw_serial, raw_serial_errors)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)


  # info table
  output$info_table <- renderTable({
    info <- data_manager$get_devices_cloud_info()
    validate(need(nrow(info) > 0, "No device information available."))
    module_message(ns, "debug", "rendering cloud info table")
    info %>% arrange(device_name) %>%
      mutate(last_heard = format(last_heard)) %>%
      select(Name = device_name, `Last software update` = last_heard, Connected = connected, Status = status, Firmware = system_firmware_version)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

}



deviceCloudInfoUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    # live state
    default_box(
      title = "Live State", width = width,
      tableOutput(ns("state_table")),
      footer =
        div(
          tooltipInput(actionButton, ns("fetch_state"), "Fetch State", icon = icon("cloud-download"),
                            tooltip = "Fetch the most recent state information from the cloud."),
          spaces(1),
          tooltipInput(actionButton, ns("fetch_state_all"), "Fetch All", icon = icon("cloud-download"),
                       tooltip = "Fetch the most recent state, data and information from the cloud.")
        )
    ),

    # live data
    default_box(
      title = "Live Data", width = width,
      tableOutput(ns("data_table")),
      footer = div(
        tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent live data from the cloud."),
        spaces(1),
        tooltipInput(actionButton, ns("fetch_data_all"), "Fetch All", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent state, data and information from the cloud.")
      )
    ),

    # live info
    default_box(
      title = "Live Info", width = width,
      tableOutput(ns("info_table")),
      footer = div(
        tooltipInput(actionButton, ns("fetch_info"), "Fetch Info", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent device information from the cloud."),
        spaces(1),
        tooltipInput(actionButton, ns("fetch_info_all"), "Fetch All", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent state, data and information from the cloud.")
      )
    )

  )

}
