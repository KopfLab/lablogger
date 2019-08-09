deviceSelectorServer <- function(input, output, session, get_devices, get_selected_devices, refresh_devices, select_devices, access_token) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    devices = NULL
  )

  # selector
  selector <- callModule(selectorTableServer, "selector",
                         id_column = "device_id",
                         column_select = c(Name = device_name, Type = device_type_desc))

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

  # device control ====
  control <- callModule(deviceCommandsServer, "control", get_devices = get_devices, access_token = access_token)

}

deviceSelectorUI <- function(id, width = 12, add_footer = tagList()) {
  ns <- NS(id)
  default_box(
    title = "Devices", width = width,
    selectorTableUI(ns("selector")),
    footer = div(
      tooltipInput(actionButton, ns("device_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh devices."),
      spaces(1),
      selectorTableButtons(ns("selector")),
      spaces(1),
      deviceControlButton(ns("control")),
      add_footer
    )
  )
}
