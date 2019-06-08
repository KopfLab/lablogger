deviceSelectorServer <- function(input, output, session, get_devices, get_selected_devices, refresh_devices, select_devices, access_token) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    devices = NULL
  )

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
    toggleState("device_control", length(selected_devices) > 0)
  })

  # trigger refresh
  observeEvent(input$device_refresh, refresh_devices())

  # trigger select
  observe(select_devices(selector$get_selected()))

  # device control ====
  # FIXME: should this be in a separate module? should the actual command be managed by the app_data module?

  # trigger control dialog
  observeEvent(input$device_control, showModal(control_dialog()))

  # dialog
  control_dialog <- reactive({
    modalDialog(
      title = h3("Send a command to the following devices"),
      fade = FALSE, easyClose = TRUE, size = "m",
      tableOutput(ns("devices")),
      h4("Command"),
      textInput(ns("command"), NULL),
      h4("Message for State Log (optional)"),
      textInput(ns("message"), NULL),
      h5("Are you sure you want to send this command to all the above devices?"),
      footer =
        tagList(
          tooltipInput(actionButton, ns("send_command"),
                       label = "Send", icon = icon("paper-plane"), tooltip = "Send command."),
          modalButton("Close")
        )
    )})

  # update table
  observe({
    devices <- get_devices()
    selected_devices <- get_selected_devices()
    if (nrow(devices) > 0 && length(selected_devices) > 0) {
      values$devices <- filter(devices, device_id %in% selected_devices) %>%
        select(ID = device_id, Name = device_name)
    }
  })

  # render table
  output$devices <- renderTable({
    validate(need(values$devices, "no devices available"))
    values$devices
  })

  # send command
  observeEvent(input$send_command, {
    module_message(ns, "debug", "sending device command '", input$command, "'...")
    result <-
      withProgress(
        message = 'Sending command', detail = "Contacting device cloud...", value = 0.5,
        get_devices() %>%
          filter(device_id %in% values$devices$ID) %>%
          ll_send_devices_command(
            command = input$command,
            message = input$message,
            access_token = access_token
          )
      )

    values$devices <- result %>%
      select(ID = device_id, Name = device_name, `Command Message` = return_message)
  })

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
      spaces(1),
      tooltipInput(actionButton, ns("device_control"), label = "Control", icon = icon("gamepad"), tooltip = "Control selected devices.") %>% disabled(),
      add_footer
    )
  )
}
