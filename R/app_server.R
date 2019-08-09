#' Chemostat Control Center Server
#'
#' Generates the server part of the isoviewer app
app_server <- function(group_id, access_token, pool, app_pwd, timezone, start_screen = "devices") {
  shinyServer(function(input, output, session) {

    message("\n\nINFO: Loading GUI instance ...")

    # DATA MANAGERS =====
    dm_experiments <- callModule(experimentsDataServer, "dm_experiments", group_id, access_token, pool, timezone)
    dm_devices <- callModule(devicesDataServer, "dm_devices", group_id, access_token, pool, timezone)
    dm_datalogs <- callModule(datalogsDataServer, "dm_datalogs", dm_experiments, dm_devices, group_id, access_token, pool, timezone)
    dm_cloudinfo <- callModule(cloudInfoDataServer, "dm_cloudinfo", dm_experiments, dm_devices, group_id, access_token, pool, timezone)

    # LOGIN SCREEN =====
    login_manager <- callModule(loginServer, "login", app_pwd = app_pwd, group = group_id, timezone = timezone)
    observeEvent(input$menu, {
      if (!login_manager$is_logged_in()) {
          module_message(NULL, "debug", "not logged in yet, jumping back to login screen")
          updateTabItems(session, "menu", selected = "login")
      }
    })
    observeEvent(login_manager$is_logged_in(), {
      if (login_manager$is_logged_in()) {
        updateTabItems(session, "menu", start_screen)
        dm_experiments$refresh_experiments(init = TRUE)
        dm_devices$refresh_devices(init = TRUE)
      }
    })

    # DATA SCREEN ====
    callModule(experimentSelectorServer, "data_exps", dm_experiments)
    data_plot <- callModule(
      dataPlotServer, "data_plot", timezone = timezone,
      get_experiments = dm_experiments$get_selected_experiments,
      get_data_logs = dm_datalogs$get_experiments_data_logs,
      refresh_data_logs = dm_datalogs$refresh_data_logs,
      reset_plot = eventReactive(length(dm_experiments$get_selected_experiments()), runif(1))
    )
    output$data <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      isolate({
        message("INFO: Generating 'data' screen")
        tagList(
          experimentSelectorUI("data_exps"),
          dataPlotUI("data_plot")
        )
      })
    })

    # EXPERIMENTS SCREEN ====

    callModule(
      experimentManagerServer, "experiments",
      dm_experiments = dm_experiments,
      dm_cloudinfo = dm_cloudinfo,
      dm_datalogs = dm_datalogs,
      timezone = timezone,
      access_token = access_token
    )

    output$experiments <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'experiments' screen")
      experimentManagerUI("experiments")
    })

    # DEVICES SCREEN ====

    callModule(
      deviceManagerServer, "devices",
      dm_devices = dm_devices,
      dm_cloudinfo = dm_cloudinfo,
      dm_datalogs = dm_datalogs,
      access_token = access_token
    )

    output$devices <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'devices' screen")
      deviceManagerUI("devices")
    })

    # WEBCAMS SCREEN ====
    output$live <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'webcams' screen")
      tagList(h3("Coming soon..."))
    })

    # HELP LINK ====
    output$help <- renderUI({
      link <- "https://github.com/KopfLab/lablogger/wiki"
      links <- c(experiments = "Experiments", devices = "Devices", data = "All-Data")
      if (input$menu %in% names(links)) link <- paste0(link, "/", links[input$menu])
      a(tags$i(class="fa fa-question-circle"), "Help", href = link, target = "_blank") %>%
        as.character() %>%
        HTML()
    })

  })
}
