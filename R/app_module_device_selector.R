
deviceSelectorServer <- function(input, output, session, data_manager) {

  # namespace
  ns <- session$ns

  # selector
  selector <- callModule(selectorTableServer, "selector", id_column = "device_id", col_headers = c("ID", "Name", "Description"))

  # update data
  observe({
    req(df <- data_manager$get_devices())
    isolate({
      if (nrow(df) > 0) {
        df <- select(df, device_id, device_name, device_desc)
        selector$set_table(df)
        data_manager$select_devices(df$device_id)
      }
    })
  })

  # update selected
  observe({
    selected_devices <- data_manager$get_selected_devices()
    selector$set_selected(selected_devices)
  })

  # trigger refresh
  observeEvent(input$device_refresh, data_manager$refresh_devices())

  # trigger select
  observe(data_manager$select_devices(selector$get_selected()))

}

deviceSelectorUI <- function(id, width = 12, selector_height = 150) {
  ns <- NS(id)
  default_box(
    title = "Devices", width = width,
    selectorTableUI(ns("selector"), height = selector_height),
    footer = div(
      tooltipInput(actionButton, ns("device_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh devices."),
      spaces(1),
      selectorTableButtons(ns("selector"))
    )
  )
}
