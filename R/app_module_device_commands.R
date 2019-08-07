deviceCommandsServer <- function(input, output, session, get_devices, access_token) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    devices = data_frame()
  )

  # trigger control dialog
  observeEvent(input$device_control, showModal(control_dialog()))

  # dialog
  control_dialog <- reactive({
    modalDialog(
      title = h3("Send a command to one or multiple devices", align = "center"),
      fade = FALSE, easyClose = TRUE, size = "l",
      fluidRow(
        column(6, DT::dataTableOutput(ns("devices"))),
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
                       label = "Send", icon = icon("paper-plane"), tooltip = "Send command."),
          modalButton("Close")
        )
    )})

  # render table
  output$devices <- DT::renderDataTable(
    {
      devices <- get_devices()
      validate(need(nrow(devices) > 0, "no devices available"))
      df <- select(devices, Name = device_name, Type = device_type_desc) %>% as.data.frame()
      rownames(df) <- devices$device_id
      df
    },
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 20, 50),
      searchDelay = 100
    ),
    server = FALSE
  )

  # detect selected devices
  observe({
    selected_devices = input$devices_rows_selected
    isolate({
      if (length(selected_devices) > 0) {
        shinyjs::enable("send_command")
        values$devices <- get_devices()[selected_devices, ] %>%
          select(device_id, Name = device_name)
      } else {
        shinyjs::disable("send_command")
        values$devices <- data_frame()
      }
    })
  })

  # show summary table of selected devices
  output$selected_devices = renderTable({
    validate(need(nrow(values$devices) > 0, "None"))
    select(values$devices, -device_id)
  })

    # send command
  observeEvent(input$send_command, {
    module_message(ns, "debug", "sending device command '", input$command, "'...")
    result <-
      withProgress(
        message = 'Sending command', detail = "Contacting device cloud...", value = 0.5,
        get_devices() %>%
          filter(device_id %in% values$devices$device_id) %>%
          ll_send_devices_command(
            command = input$command,
            message = input$message,
            access_token = access_token
          )
      )

    values$devices <- result %>%
      select(device_id, Name = device_name, `Command Message` = return_message)
  })
}

# Device Control Button
deviceControlButton <- function(id, label = "Control",  tooltip = "Send device commands.") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("device_control"), label = label, icon = icon("gamepad"), tooltip = tooltip)
}

