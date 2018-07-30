
deviceInfoServer <- function(input, output, session, data_manager) {


  # namespace
  ns <- session$ns

  # fetch cloud data - experiment device links
  observeEvent(input$fetch_cloud_data_experiment_links, {
    data_manager$refresh_experiment_device_links()
    data_manager$refresh_devices_cloud_data()
  })

  # fetch state
  observeEvent(input$fetch_state, data_manager$refresh_devices_cloud_state())

  # fetch data
  observeEvent(input$fetch_data, {
    data_manager$refresh_devices_cloud_data()
    # only trigger this if currently showing exp links info
    data_manager$refresh_experiment_device_links(
      init = !any(c("r_exps", "nr_exps") %in% input$data_table_options)
    )
  })

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
      data_manager$refresh_experiment_device_links() # always trigger in this case
      data_manager$refresh_devices_cloud_info()
    })
  })

  # experiment devices
  output$cloud_data_experiment_links_table <- renderTable({
    exp_devices <- data_manager$get_cloud_data_experiment_links()
    validate(need(nrow(exp_devices) > 0, "No active linked experiments."))
    module_message(ns, "debug", "rendering experiment devices table")
    exp_devices %>% arrange(device_name, data_idx) %>%
      select(Name = device_name, idx = data_idx, `Exp ID` = exp_id, `Data Group` = data_group)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

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
    links <- data_manager$get_experiment_device_links()
    validate(need(nrow(data) > 0, "No live device data available."))
    data_links <- ll_summarize_cloud_data_experiment_links(cloud_data = data, experiment_device_links = links) %>%
      arrange(device_name, idx) %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Last data update` = datetime,
             `Exp IDs (recording)` = recording_exp_ids, `Exp IDs (not recording)` = non_recording_exp_ids,
             idx, key, value, units,
             raw_serial, raw_serial_errors)
    module_message(ns, "debug", "rendering cloud data table")

    if (!"serial" %in% input$data_table_options)
      data_links <- select(data_links, -raw_serial, -raw_serial_errors)
    if (!"r_exps" %in% input$data_table_options)
      data_links <- select(data_links, -`Exp IDs (recording)`)
    if (!"nr_exps" %in% input$data_table_options)
      data_links <- select(data_links, -`Exp IDs (not recording)`)

    return(data_links)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)


  # info table
  output$info_table <- renderTable({
    info <- data_manager$get_devices_cloud_info()
    validate(need(nrow(info) > 0, "No device information available."))
    module_message(ns, "debug", "rendering cloud info table")
    info %>% arrange(device_name) %>%
      mutate(last_heard = format(last_heard)) %>%
      select(Name = device_name, `Last restart` = last_heard, Connected = connected, Status = status, Firmware = system_firmware_version)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

}



deviceInfoUI <- function(id, width = 12) {

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
                       tooltip = "Fetch all device information from the cloud and database.")
        )
    ),

    # live data
    default_box(
      title = "Live Data", width = width,

      checkboxGroupInput(ns("data_table_options"), NULL,
                         c("Experiment Links (recording)" = "r_exps",
                           "Experiment Links (not recording)" = "nr_exps",
                           "Raw Serial Data" = "serial"),
                         selected = c("r_exps", "serial"),
                         inline = TRUE),

      tableOutput(ns("data_table")),
      footer = div(
        tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent live data and experiment links from the cloud."),
        spaces(1),
        tooltipInput(actionButton, ns("fetch_data_all"), "Fetch All", icon = icon("cloud-download"),
                     tooltip = "Fetch all device information from the cloud and database.")
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
                     tooltip = "Fetch all device information from the cloud and database.")
      )
    )

  )

}
