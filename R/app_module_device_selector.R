# FIXME: replace data_manager with get_devices, refresh_devices, etc. functions
deviceSelectorServer <- function(input, output, session, get_devices, get_selected_devices, refresh_devices, select_devices) {

  # namespace
  ns <- session$ns

  # selector
  selector <- callModule(selectorTableServer, "selector", id_column = "device_id", col_headers = c("ID", "Name", "Type"))

  # update data
  observe({
    req(df <- get_devices())
    isolate({
      if (nrow(df) > 0) {
        module_message(ns, "debug", "setting device selection table")
        df <- select(df, device_id, device_name, device_type_desc)
        selector$set_table(df)
        # selet all by default
        #select_devices(df$device_id)
      }
    })
  })

  # update selected
  observe({
    selected_devices <- get_selected_devices()
    selector$set_selected(selected_devices)
  })

  # trigger refresh
  observeEvent(input$device_refresh, refresh_devices())

  # trigger select
  observe(select_devices(selector$get_selected()))

}

deviceSelectorUI <- function(id, width = 12, selector_height = 150, add_footer = tagList()) {
  ns <- NS(id)
  default_box(
    title = "Devices", width = width,
    selectorTableUI(ns("selector"), height = selector_height),
    footer = div(
      tooltipInput(actionButton, ns("device_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh devices."),
      spaces(1),
      selectorTableButtons(ns("selector")),
      add_footer
    )
  )
}
