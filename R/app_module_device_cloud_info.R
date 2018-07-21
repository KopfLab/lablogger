
deviceCloudInfoServer <- function(input, output, session, data_manager) {


  # namespace
  ns <- session$ns

  # fetch state
  observeEvent(input$fetch_state, data_manager$refresh_devices_cloud_state())

  # fetch data
  observeEvent(input$fetch_data, {})

  # state table
  output$state_table <- renderTable({
    state <- data_manager$get_devices_cloud_state()
    validate(need(nrow(state) > 0, "No state information available."))
    module_message(ns, "debug", "rendering cloud state table")
    state %>% filter(in_use) %>% arrange(device_name) %>%
      select(Name = device_name, Description = device_desc, `Last seen` = datetime, Version = version)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

}



deviceCloudInfoUI <- function(id) {

  ns <- NS(id)

  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Live State", br(),
      tooltipInput(actionButton, ns("fetch_state"), "Fetch State", icon = icon("cloud-download"),
                   tooltip = "Fetch the most recent state information from the cloud."),
      div(style = 'overflow-x: scroll; height: 400px;',
          tableOutput(ns("state_table")) %>% withSpinner(type = 5, proxy.height = "400px;"))
    ),
    tabPanel(
      "Live Data", br(),
      tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                   tooltip = "Fetch the most recent live data from the cloud.")
    )
  )

}
