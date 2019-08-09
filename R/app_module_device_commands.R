deviceCommandsServer <- function(input, output, session, get_devices, access_token) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    selected_devices = data_frame()
  )

  # trigger control dialog
  observeEvent(input$device_control, showModal(control_dialog()))

  # dialog
  control_dialog <- reactive({
    modalDialog(
      title = h3("Send a command to one or multiple devices", align = "center"),
      fade = FALSE, easyClose = TRUE, size = "l",
      fluidRow(
        column(6, selectorTableUI(ns("devices"))),
        column(6, h4("Selected Devices"), tableOutput(ns("selected_devices")))
      ),
      fluidRow(
        column(6,
          h4("Command"),
          h5("For a full list of available commands, follow ", a("this link", href="https://github.com/KopfLab/labware_commands#devicecontroller-commands-cmd-options", target = "_blank"), "."),
          textInput(ns("command"), NULL)
        ),
        column(6,
          h4("Message for State Log"),
          h5("To add an optional custom note to the devices' state log."),
          textInput(ns("message"), NULL)
        )
      ),
      h4("CHECK: are you absolutely sure you want to send this command to the selected devices?", style = "color: red;", align = "center"),
      footer =
        tagList(
          tooltipInput(actionButton, ns("send_command"),
                       label = "Send", icon = icon("paper-plane"),
                       tooltip = "Send command.",
                       disabled = TRUE),
          modalButton("Close")
        )
    )})

  # selection table
  selector <- callModule(
    selectorTableServer, "devices",
    id_column = "device_id",
    column_select = c(Name = device_name, Type = device_type_desc),
    page_lengths = c(10, 20, 50)
  )

  # update devices table
  observe({
    req(df <- get_devices())
    if (nrow(df) > 0) selector$set_table(df)
  })

  # react to device selection
  observe({
    values$selected_devices <- selector$get_selected_items()
    shinyjs::toggleState("send_command", condition = nrow(values$selected_devices) > 0)
  })

  # show summary table of selected devices
  output$selected_devices = renderTable({
    validate(need(nrow(values$selected_devices) > 0, "None"))
    if ("return_message" %in% names(values$selected_devices))
      select(values$selected_devices, Name = device_name, `Command Message` = return_message)
    else
      select(values$selected_devices, Name = device_name)
  })

    # send command
  observeEvent(input$send_command, {
    module_message(ns, "debug", "sending device command '", input$command, "'...")
    values$selected_devices <-
      withProgress(
        message = 'Sending command', detail = "Contacting device cloud...", value = 0.5,
        values$selected_devices %>%
          ll_send_devices_command(
            command = input$command,
            message = input$message,
            access_token = access_token
          )
      )
  })
}

# Device Control Button
deviceControlButton <- function(id, label = "Control",  tooltip = "Send device commands.") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("device_control"), label = label, icon = icon("gamepad"), tooltip = tooltip)
}

