
#' Device Info Server
#' @param refresh_experiment_device_links to refresh the database queried list of links between experiments and devices (either from the device or experiment perspective)
deviceInfoServer <- function(input, output, session, get_cloud_state, refresh_cloud_state, get_cloud_data, refresh_cloud_data, refresh_experiment_device_links, get_cloud_info, refresh_cloud_info) {


  # namespace
  ns <- session$ns

  # fetch cloud data - experiment device links
  observeEvent(input$fetch_cloud_data_experiment_links, {
    refresh_experiment_device_links()
    refresh_cloud_data()
  })

  # fetch state
  observeEvent(input$fetch_state, refresh_cloud_state())

  # fetch data
  observeEvent(input$fetch_data, {
    refresh_cloud_data()
    # only trigger this if currently showing exp links info
    refresh_experiment_device_links(
      init = !any(c("r_exps", "nr_exps") %in% input$data_table_options)
    )
  })

  # fetch info
  observeEvent(input$fetch_info, refresh_cloud_info())

  # fetch all
  observe({
    req(input$fetch_state_all + input$fetch_data_all + input$fetch_info_all > 0)
    module_message(ns, "debug", "fetching all cloud info")
    isolate({
      refresh_cloud_state()
      refresh_cloud_data()
      refresh_experiment_device_links() # always trigger in this case
      refresh_cloud_info()
    })
  })

  # state table
  output$state_table <- renderTable({
    state <- get_cloud_state()
    validate(need(nrow(state) > 0, "No state information available."))
    module_message(ns, "debug", "rendering cloud state table")
    vars_start <- which(names(state) == "version")
    state %>% arrange(device_name) %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Last state change` = datetime, Version = version, vars_start:ncol(state))
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # data table
  output$data_table <- renderTable({
    data <- get_cloud_data()
    validate(need(nrow(data) > 0, "No live data available."))
    data <- data %>%
      mutate(datetime = format(datetime)) %>%
      select(Name = device_name, `Last data update` = datetime,
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


  # info table
  output$info_table <- renderTable({
    info <- get_cloud_info()
    validate(need(nrow(info) > 0, "No device information available."))
    module_message(ns, "debug", "rendering cloud info table")
    info %>% arrange(device_name) %>%
      mutate(last_heard = format(last_heard)) %>%
      select(Name = device_name, `Last restart` = last_heard, Connected = connected, Status = status, Firmware = system_firmware_version)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

}

deviceDataUI <- function(id, width = 12, selected_options = c("r_exps", "serial"), include_fetch_all = TRUE) {

  ns <- NS(id)

  tagList(
    # live data
    default_box(
      title = "Live Device Data", width = width,
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
                     tooltip = "Fetch the most recent live data and experiment links from the cloud."),
        spaces(1),
        if (include_fetch_all)
          tooltipInput(actionButton, ns("fetch_data_all"), "Fetch All", icon = icon("cloud-download"),
                       tooltip = "Fetch all device information from the cloud and database.")
      )
    )
  )
}

deviceInfoUI <- function(id, width = 12, include_fetch_all = TRUE) {

  ns <- NS(id)

  tagList(

    # live state
    default_box(
      style = paste0("min-height: 130px;"),
      title = "Live Device State", width = width,
      tableOutput(ns("state_table")) %>% withSpinner(type = 5, proxy.height = "130px"),
      footer =
        div(
          tooltipInput(actionButton, ns("fetch_state"), "Fetch State", icon = icon("cloud-download"),
                            tooltip = "Fetch the most recent state information from the cloud."),
          spaces(1),
          if (include_fetch_all)
            tooltipInput(actionButton, ns("fetch_state_all"), "Fetch All", icon = icon("cloud-download"),
                         tooltip = "Fetch all device information from the cloud and database.")
        )
    ),

    # live info
    default_box(
      title = "Live Device Info", width = width,
      style = paste0("min-height: 130px;"),
      tableOutput(ns("info_table")) %>% withSpinner(type = 5, proxy.height = "130px"),
      footer = div(
        tooltipInput(actionButton, ns("fetch_info"), "Fetch Info", icon = icon("cloud-download"),
                     tooltip = "Fetch the most recent device information from the cloud."),
        spaces(1),
        if (include_fetch_all)
          tooltipInput(actionButton, ns("fetch_info_all"), "Fetch All", icon = icon("cloud-download"),
                       tooltip = "Fetch all device information from the cloud and database.")
      )
    )

  )

}
