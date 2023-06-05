
experimentManagerServer <- function(input, output, session, dm_links, dm_experiments, dm_devices, dm_cloudinfo, dm_datalogs, timezone, access_token) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    in_edit_exp_device_links = NULL
  )

  # new experiment =====
  observeEvent(input$experiment_new, showModal(new_dialog()))
  new_dialog <- reactive({
    modalDialog(
      title = h3("Create new experiment", align = "center"),
      fade = FALSE, easyClose = TRUE, size = "m",
      h4("New Experiment ID"),
      h5("This is your experiment's unique identifier, best to keep it as short as possible (4-8 characters). Initials and an experiment number are a popular choice, for example 'ABC01'. ", span("Keep in mind that you will not be able to change this ID afterwards.", style = "color: red;"), "However, you will be able to add an experiment description and experiment notes that you can continue to change/update as you see fit."),
      textInput(ns("new_experiment_id"), label = NULL),
      footer =
        tagList(
          actionButton(ns("create_experiment"), label = "Create", icon = icon("save")) %>%
            shinyjs::disabled(),
          modalButton("Close")
        )
    )
  })
  observe({ toggleState("create_experiment", nchar(input$new_experiment_id) > 3) })
  observeEvent(input$create_experiment, {
    toggleState("create_experiment", FALSE) # disable button
    new_id <- input$new_experiment_id
    module_message(ns, "debug", "creating new experiment ", new_id)
    tryCatch(
      {
        dm_experiments$add_experiment(new_id)
        dm_experiments$refresh_experiments()
        load_experiment(new_id, loading_tab = "info")
        removeModal()
      },
      error = function(e) {
        removeModal()
        error_modal(
          title = glue::glue("Could not create experiment '{new_id}'"),
          h4(e$message)
        )
      }
    )
  })

  # select experiment ====
  get_experiments_for_dropdown <- reactive({
    exps <- dm_experiments$get_experiments()
    if (nrow(exps) == 0) return ("Choose an experiment" = "")
    exps <- exps %>% mutate(label = sprintf("%s: %s", exp_id, exp_desc))
    c("Choose an experiment" = "",
      list(
        `Recording` = exps %>% filter(!archived, recording) %>% select(label, exp_id) %>% deframe(),
        `Not recording` = exps %>% filter(!archived, !recording) %>% select(label, exp_id) %>% deframe(),
        `Archived` = exps %>% filter(archived) %>% select(label, exp_id) %>% deframe()
      ))
  })

  # render dropdown
  output$experiment <- renderUI(selectInput(ns("experiment"), label = NULL, choices = get_experiments_for_dropdown()))

  # trigger refresh
  observeEvent(input$experiment_refresh, {
    dm_experiments$refresh_experiments()
    load_links_table()
  })

  # update dropdown
  observe({
    updateSelectInput(session, "experiment", choices = get_experiments_for_dropdown(), selected = isolate(dm_experiments$get_loaded_experiment()))
    module_message(ns, "debug", "updating experiments dropdown")
  })

  # load experiment ===
  observeEvent(input$experiment, {
    req(input$experiment)
    if (is.null(dm_experiments$get_loaded_experiment()) || input$experiment != dm_experiments$get_loaded_experiment()) {
      load_experiment(input$experiment)
    }
  })

  load_experiment <- function(exp_id, loading_tab = NULL) {
    dm_experiments$load_experiment(exp_id)

    # archived
    if (dm_experiments$is_loaded_experiment_archived()) {

      # archived exp - FIXME: allow access to data here?
      hide("tabs")
      show("archived_msg")

    } else {

      # recording
      recording <- dm_experiments$is_loaded_experiment_recording()
      toggle("start_recording", condition = !recording)
      toggle("stop_recording", condition = recording)

      # info
      if (!is.na(dm_experiments$get_loaded_experiment_info()$exp_desc))
        updateTextInput(session, "exp_desc", value = dm_experiments$get_loaded_experiment_info()$exp_desc)
      else
        updateTextInput(session, "exp_desc", value = "", placeholder = "Add a succinct description for this experiment.")

      if (!is.na(dm_experiments$get_loaded_experiment_info()$exp_notes))
        updateTextInput(session, "exp_notes", value = dm_experiments$get_loaded_experiment_info()$exp_notes)
      else
        updateTextInput(session, "exp_notes", value = "", placeholder = "Keep notes about this experiment.")

      # reset cloud data
      dm_cloudinfo$reset_cloud_data()

      # select tab
      if (!is.null(loading_tab)) {
        module_message(ns, "debug", glue::glue("selecting exp. details tab '{loading_tab}'..."))
        updateTabsetPanel(session, "tabset", selected = loading_tab)
      }

      # show tabs
      hide("archived_msg")
      show("tabs")
    }
  }

  # info tab
  output$exp_ID <- renderText({
    validate(need(dm_experiments$get_loaded_experiment(), "no experiment selected"))
    dm_experiments$get_loaded_experiment()
  })

  # save exp info ====

  observeEvent(input$save_info, {
    success <- dm_experiments$updated_loaded_experiment_info(exp_desc = input$exp_desc, exp_notes = input$exp_notes)
    if (success) {
      success_modal(
        h4(sprintf("Experiment '%s' was updated succesfully.",
                   dm_experiments$get_loaded_experiment()))
      )
    } else {
      error_modal(
        h4(sprintf("Something went wrong updating experiment '%s'.",
                   dm_experiments$get_loaded_experiment()))
      )
    }
    dm_experiments$refresh_experiments()
  })

  # start/stop recording ====

  observeEvent(input$start_recording, {
    result <- dm_experiments$start_experiment()
    if (result$success) {
      success_modal(
        h4(strong(sprintf("Experiment %s is now recording.", dm_experiments$get_loaded_experiment()))),
        h4("Please note that only data from linked devices that have 'data-log' turned on will actually be recorded. Check the 'Live State' on the 'Devices' tab for an overview of your devices' status.")
      )
    } else {
      error_modal(
        h4(sprintf("Experiment %s could not start recording.", dm_experiments$get_loaded_experiment()))
      )
    }
    toggle("start_recording", condition = FALSE)
    toggle("stop_recording", condition = TRUE)
  })

  observeEvent(input$stop_recording, {
    result <- dm_experiments$stop_experiment()
    if (result$success) {
      success_modal(
        h4(sprintf("Experiment %s is now no longer recording.", dm_experiments$get_loaded_experiment()))
      )
    } else {
      error_modal(
        h4(sprintf("Experiment %s could not stop recording.", dm_experiments$get_loaded_experiment()))
      )
    }
    toggle("start_recording", condition = TRUE)
    toggle("stop_recording", condition = FALSE)
  })

  # experiment links =====

  # device links table
  device_links <- callModule(
    selectorTableServer, "device_links",
    id_column = "..id..", row_column = "..row..",
    initial_page_length = -1 # show All
  )

  # load experiment links table
  load_links_table <- function() {

    # retrieve links
    exp_links <- dm_experiments$get_loaded_experiment_device_links_all()

    # no links available
    if (is.null(exp_links) || nrow(exp_links) == 0) {
      module_message(ns, "debug", "no device links available")
      device_links$set_table(NULL)
    } else {

      if (dm_cloudinfo$has_cloud_data()) {
        # fetch cloud data
        module_message(ns, "debug", "preparing device links data table with live cloud data")
        data <- dm_cloudinfo$get_exp_devices_cloud_data()
      } else {
        # only links, no cloud data
        module_message(ns, "debug", "preparing device links data table without cloud data")
        data <-
          ll_summarize_cloud_data_experiment_links(
            cloud_data = tibble(), experiment_device_links = exp_links,
            linked = TRUE, unlinked = FALSE
          ) %>% mutate(error = "please click <Fetch Data>")
      }

      # format data
      data <- data %>%
        mutate(datetime = ifelse(!is.na(datetime), format(datetime), error)) %>%
        mutate(..row.. = dplyr::row_number()) %>%
        select(
          ..id.., ..row..,
          Name = device_name, `Live data posted at` = datetime,
          `Exp IDs (recording)` = recording_exp_ids, `Exp IDs (not recording)` = non_recording_exp_ids,
          idx, key, value, units,
          raw_serial, raw_serial_errors
        )

      data <- devices_info$apply_live_data_table_options(data)
      device_links$set_table(data)
    }
  }

  # watch for new cloud data
  observe({

  })

  # enable/disable delete button
  observe({
    toggleState("device_links_delete", length(device_links$get_selected()) > 0)
  })

  # add links =====
  observeEvent(input$device_links_add, showModal(add_dialog()))
  add_dialog <- reactive({
    modalDialog(
      title = h3("Add device links", align = "center"),
      fade = FALSE, easyClose = TRUE, size = "l",
      div(id = ns("add_links_step1_div"),
          h5(
            align = "center",
            tooltipInput(
              actionButton, ns("add_by_device"),
              label = "Add links by device", icon = icon("cogs"), tooltip = "Select specific devices to link some or all of their data to the experiment."),
            tooltipInput(
              actionButton, ns("add_by_experiment"),
              label = "Copy links from experiment", icon = icon("flask"), tooltip = "Copy some or all of the device data links from another experiment.")
          ),
          div(id = ns("devices_div"), selectorTableUI(ns("add_links_devices"))) %>% shinyjs::hidden(),
          div(id = ns("experiments_div"), selectorTableUI(ns("add_links_experiments"))) %>% shinyjs::hidden()
      ),
      div(id = ns("add_links_step2_div"),
          span("Select which links to add (only links not already part of your experiment are displayed).", align = "center"),
          selectorTableUI(ns("add_links_links"))
      ) %>% shinyjs::hidden(),
      footer =
        tagList(
          actionButton(ns("add_by_device_next"), label = "Next", icon = icon("forward")) %>%
            shinyjs::hidden() %>% shinyjs::disabled(),
          actionButton(ns("add_by_experiment_next"), label = "Next", icon = icon("forward")) %>%
            shinyjs::hidden() %>% shinyjs::disabled(),
          actionButton(ns("add_links_save"), label = "Add Links", icon = icon("save"))%>%
            shinyjs::hidden() %>% shinyjs::disabled(),
          modalButton("Close")
        )
    )
  })
  observeEvent(input$add_by_device, {
    # make sure to initialize devices
    dm_devices$init_devices()
    shinyjs::hide("experiments_div")
    shinyjs::hide("add_by_experiment_next")
    shinyjs::show("devices_div")
    shinyjs::show("add_by_device_next")
  })
  observeEvent(input$add_by_experiment, {
    shinyjs::hide("devices_div")
    shinyjs::hide("add_by_device_next")
    shinyjs::show("experiments_div")
    shinyjs::show("add_by_experiment_next")
  })
  observe({
    toggleState("add_by_device_next", length(add_links_devices$get_selected()) > 0)
    toggleState("add_by_experiment_next", length(add_links_experiments$get_selected()) > 0)
  })

  # add links - device selection
  add_links_devices <- callModule(
    selectorTableServer, "add_links_devices",
    id_column = "device_id",
    column_select = c(Name = device_name, Type = device_type_desc),
    initial_page_length = 5
  )
  observe({
    req(df <- dm_devices$get_devices())
    req(dm_experiments$get_loaded_experiment())
    if (nrow(df) > 0) add_links_devices$set_table(df)
  })

  # add links - experiment selection
  add_links_experiments <- callModule(
    selectorTableServer, "add_links_experiments",
    id_column = "exp_id",
    column_select = c(Description = exp_desc, Recording = recording),
    initial_page_length = 5
  )
  observe({
    req(df <- dm_experiments$get_experiments())
    req(dm_experiments$get_loaded_experiment())
    if (nrow(df) > 0) {
      df <- df %>%
        dplyr::mutate(recording = ifelse(recording, "yes", "no")) %>%
        dplyr::filter(exp_id != dm_experiments$get_loaded_experiment())
      add_links_experiments$set_table(df)
    }
  })

  # select links to add =====
  add_links_links <- callModule(
    selectorTableServer, "add_links_links",
    id_column = "..id..", row_column = "..row..",
    initial_page_length = 10
  )
  set_add_links_table <- function(links_cloud_data) {
    # only links that are not part of the experiment yet
    exp_links <- dm_experiments$get_loaded_experiment_device_links()
    if (nrow(exp_links) > 0) {
      links_cloud_data <- links_cloud_data %>%
        filter(!..id.. %in% exp_links$..id..)
    }

    # table data
    table_data <- links_cloud_data %>%
      mutate(datetime = ifelse(!is.na(datetime), format(datetime), error)) %>%
      mutate(..row.. = dplyr::row_number()) %>%
      select(
        ..id.., ..row..,
        Name = device_name, `Live data posted at` = datetime,
        `Exp IDs (recording)` = recording_exp_ids, `Exp IDs (not recording)` = non_recording_exp_ids,
        idx, key, value, units
      )
    add_links_links$set_table(table_data)
  }

  # links by device ======
  observeEvent(input$add_by_device_next, {
    # relevant device links
    links <- dm_links$get_links() %>%
      filter(device_id %in% add_links_devices$get_selected())

    # fetch cloud data
    links_cloud_data <-
      dm_cloudinfo$get_cloud_data(
        devices = add_links_devices$get_selected_items(),
        links = links,
        linked = TRUE,
        unlinked = TRUE
      )

    # set data
    set_add_links_table(links_cloud_data)

    # hide/show divs
    shinyjs::hide("add_links_step1_div")
    shinyjs::hide("add_by_device_next")
    shinyjs::show("add_links_step2_div")
    shinyjs::show("add_links_save")
  })

  # links by experiment ====
  observeEvent(input$add_by_experiment_next, {

    # relevant device links
    links <- dm_links$get_links() %>%
      filter(exp_id %in% add_links_experiments$get_selected())

    # devices
    devices <- links %>%
      select(device_id, device_name, particle_id, device_type_desc) %>%
      unique()

    # fetch cloud data
    links_cloud_data <-
      dm_cloudinfo$get_cloud_data(
        devices = devices,
        links = links,
        linked = TRUE,
        unlinked = FALSE
      )

    # set data
    set_add_links_table(links_cloud_data)

    # hide/show divs
    shinyjs::hide("add_links_step1_div")
    shinyjs::hide("add_by_experiment_next")
    shinyjs::show("add_links_step2_div")
    shinyjs::show("add_links_save")
  })

  # FIXME: easier to get the device_id and idx elsewhere?
  # parsing seems a bit silly as a solution
  parse_link_id <- function(value) {
    matches <- stringr::str_match(value, "(\\d+)_(\\d+|NA)")
    tibble(
      device_id = readr::parse_number(matches[,2]),
      data_idx = readr::parse_number(matches[,3])
    )
  }

  observe({
    link_ids <- parse_link_id(add_links_links$get_selected())
    shinyjs::toggleState("add_links_save", length(na.omit(link_ids$data_idx)) > 0)
  })

  # save new links ======
  observeEvent(input$add_links_save, {
    add_links <- parse_link_id(add_links_links$get_selected())

    module_message(
      ns, "debug", "adding device links ",
      with(add_links, paste0(device_id, " #", data_idx) %>% paste(collapse = ", ")),
      "..."
    )
    dm_experiments$add_experiment_device_links(add_links)

    # close modal
    removeModal()
    # refresh device links
    dm_links$refresh_links()
    device_links$set_selected(c())
    dm_cloudinfo$reset_cloud_data()
  })

  # delete links ====
  observeEvent(input$device_links_delete, showModal(delete_dialog()))
  delete_dialog <- reactive({
    modalDialog(
      title = h3("Delete device links", align = "center"),
      fade = FALSE, easyClose = TRUE, size = "m",
      tableOutput(ns("selected_links")),
      h4("Are you sure that you want to delete these device links? They will no longer record information as part of this experiment.",
         style = "color: red;", align = "center"),
      footer =
        tagList(
          tooltipInput(
            actionButton, ns("device_links_delete_confirm"),
            label = "Delete", icon = icon("trash"), tooltip = "Delete these links."),
          modalButton("Close")
        )
    )
  })
  output$selected_links = renderTable({
    validate(need(length(device_links$get_selected()) > 0, "None selected."))
    device_links$get_selected_items() %>% select(-..id.., -..row..)
  })

  # delete confirmation
  observeEvent(input$device_links_delete_confirm, {

    to_delete <-
      filter(
        dm_experiments$get_loaded_experiment_device_links(),
        ..id.. %in% device_links$get_selected()
      )$exp_device_data_id

    # FIXME: immplement delete
    module_message(ns, "debug", "deleting device links ", paste(to_delete, collapse = ", "), "...")
    dm_experiments$delete_experiment_device_links(to_delete)

    # close modal
    removeModal()
    # refresh device links
    dm_links$refresh_links()
    device_links$set_selected(c())
    dm_cloudinfo$reset_cloud_data()
  })

  # device control / commands =====
  control <- callModule(deviceCommandsServer, "control", get_devices = dm_experiments$get_loaded_experiment_devices, access_token = access_token)

  # devices info =====
  devices_info <- callModule(
    deviceInfoServer, "devices_info",
    get_cloud_state = dm_cloudinfo$get_exp_devices_cloud_state,
    refresh_cloud_state = dm_cloudinfo$refresh_cloud_state,
    get_cloud_data = dm_cloudinfo$get_exp_devices_cloud_data,
    refresh_cloud_data = function(){
      dm_cloudinfo$refresh_cloud_data()
      load_links_table() # update links table whenever cloud data is refreshed
    },
    get_cloud_info = dm_cloudinfo$get_exp_devices_cloud_info,
    refresh_cloud_info = dm_cloudinfo$refresh_cloud_info,
    get_device_ids = dm_experiments$get_loaded_experiment_device_ids,
    get_state_logs = dm_datalogs$get_experiment_devices_state_logs,
    refresh_state_logs = dm_datalogs$refresh_experiment_state_logs
  )
  observe({
    devices_info$trigger_live_data_table_options()
    load_links_table()
  })

  # experiment data ======

  callModule(
    dataPlotServer, "exp_data_plot", timezone = timezone,
    get_experiments = dm_experiments$get_loaded_experiment,
    get_data_logs = dm_datalogs$get_experiment_data_logs,
    refresh_data_logs = dm_datalogs$refresh_experiment_data_logs,
    reset_plot = eventReactive(dm_experiments$get_loaded_experiment(), runif(1))
  )

}

experimentManagerUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(

    default_box(
      # selection ====
      title = "Experiments", width = width,
      uiOutput(ns("experiment")),
      footer = div(
        tooltipInput(actionButton, ns("experiment_refresh"), label = "Refresh", icon = icon("sync"), tooltip = "Refresh experiments."),
        spaces(1),
        # FIXME
        tooltipInput(actionButton, ns("experiment_new"), label = "New experiment", icon = icon("plus"), tooltip = "Add new experiment."),
        spaces(1),
        deviceControlButton(ns("control"), label = "Control Devices"),
        spaces(1),
        actionButton(ns("start_recording"), label = "Start Recording",
                     icon = icon("play"), style="color: #fff; background-color: #007f1f; border-color: #2e6da4") %>% hidden(),
        actionButton(ns("stop_recording"), label = "Stop Recording",
                     icon = icon("stop"), style="color: #fff; background-color: #f22e10; border-color: #2e6da4") %>% hidden()
      )
    ),

    div(id = ns("archived_msg"),
        h2("Sorry, this experiment is archived and can not be reconfigured. Please use the 'Data' menu on the left to view the data.")
        ) %>% hidden(),

    div(id = ns("tabs"),
    tabsetPanel(
      id = ns("tabset"),
      type = "tabs", selected = "data",
      # data ===
      tabPanel(
        value = "data",
        "Data", br(),
        dataPlotUI(ns("exp_data_plot"))
      ),
      # info =====
      tabPanel(
        value = "info",
        "Info",
        br(),
        default_box(
          title = "Experiment Information", width = 12,
          h4("ID:", textOutput(ns("exp_ID"), inline = TRUE)),
          h4("Description:"),
          textAreaInput(ns("exp_desc"), NULL, cols = 50, rows = 5, resize = "none"),
          h4("Notes:"),
          textAreaInput(ns("exp_notes"), NULL, width = "100%", rows = 10, resize = "both"),
          footer = actionButton(ns("save_info"), label = "Save", icon = icon("save"))
        )
      ),
      # devices ====
      tabPanel(
        value = "devices",
        "Devices",
        br(),
        spaces(3),
        deviceFetchAllUI(ns("devices_info")),
        br(), br(),
        deviceDataUI(
          ns("devices_info"), selected_options = "r_exps",
          title = "Device Links & Live Data",
          output = selectorTableUI(ns("device_links")),
          add_footer = tagList(
            tooltipInput(
              actionButton,
              ns("device_links_add"),
              label = "Add Links",
              icon = icon("link"),
              tooltip = "Add additional device links."
            ),
            tooltipInput(
              actionButton,
              ns("device_links_delete"),
              label = "Delete Links",
              icon = icon("unlink"),
              tooltip = "Delete the selected device link(s).",
              disabled = TRUE
            )
          )
        ),
        deviceStateUI(ns("devices_info")),
        deviceInfoUI(ns("devices_info")),
        deviceLogsUI(ns("devices_info"))
      )
    )) %>% hidden()

  )

}
