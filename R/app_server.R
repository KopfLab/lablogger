#' Chemostat Control Center Server
#'
#' Generates the server part of the isoviewer app
app_server <- function(group_id, access_token, pool, app_pwd, timezone, start_screen = "data") {
  shinyServer(function(input, output, session) {

    message("\n\nINFO: Loading GUI instance ...")

    # data managers
    dm_experiments <- callModule(experimentsDataServer, "dm_experiments", group_id, access_token, pool, timezone)
    dm_devices <- callModule(devicesDataServer, "dm_devices", group_id, access_token, pool, timezone)
    dm_datalogs <- callModule(datalogsDataServer, "dm_datalogs", dm_experiments, dm_devices, group_id, access_token, pool, timezone)
    dm_cloudinfo <- callModule(cloudInfoDataServer, "dm_cloudinfo", dm_experiments, dm_devices, group_id, access_token, pool, timezone)

    # login server
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

    experiments <- callModule(
      experimentManagerServer, "experiments",
      dm_experiments, dm_cloudinfo, dm_datalogs,
      timezone = timezone)
    output$experiments <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'experiments' screen")
      experimentManagerUI("experiments")
    })

    # DEVICES SCREEN ====

    callModule(
      deviceSelectorServer, "devices",
      get_devices = dm_devices$get_devices,
      get_selected_devices = dm_devices$get_selected_devices,
      refresh_devices = dm_devices$refresh_devices,
      select_devices = dm_devices$select_devices
    )
    callModule(
      deviceInfoServer, "devices_info",
      get_cloud_state = dm_cloudinfo$get_devices_cloud_state,
      refresh_cloud_state = dm_cloudinfo$refresh_cloud_state,
      get_cloud_data = dm_cloudinfo$get_devices_cloud_data,
      refresh_cloud_data = dm_cloudinfo$refresh_cloud_data,
      refresh_experiment_device_links = dm_devices$refresh_devices_experiments_links,
      get_cloud_info = dm_cloudinfo$get_devices_cloud_info,
      refresh_cloud_info = dm_cloudinfo$refresh_cloud_info,
      get_devices = dm_devices$get_selected_devices,
      get_state_logs = dm_datalogs$get_devices_state_logs,
      refresh_state_logs = dm_datalogs$refresh_state_logs
    )

    output$devices <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'devices' screen")
      tagList(
        deviceSelectorUI("devices", width = 12, selector_height = 200,


                         ),
        deviceDataUI("devices_info"),
        deviceLogsUI("devices_info"),
        deviceInfoUI("devices_info")
      )
    })

    # WEBCAMS SCREEN ====
    output$live <- renderUI({
      if (!login_manager$is_logged_in()) return(NULL)
      message("INFO: Generating 'webcams' screen")
      tagList(h3("Coming soon..."))
    })




    # # google spreadsheet authentication
    # options(googlesheets.httr_oauth_cache = FALSE)
    # if (file.exists(file.path(.base_dir, "gs_token.rds"))) {
    #   message("INFO: Google spreadsheet token found, authenticating...")
    #   gs_auth(token = file.path(.base_dir, "gs_token.rds"))
    # } else {
    #   stop("No googlespreadsheet authentication information found. Please run generate_gs_token() function from interactive environment once first (e.g. from RStudio) and indicate the target_directory for the GUI data.", call. = FALSE)
    # }

    # # SETTINGS =======
    # global <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "global")
    # configs <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "configurations")
    # channels <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "channels")
    # data_types <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "data_types")
    # live <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "live")
    # message("INFO: Authenticated and all settings loaded.")
    # last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
    # if (length(last_update) == 0) stop("Cannot find log google spreadsheet ", global$gs_title, call. = F)
    # message("INFO: Last update of google spreadsheet ", format(last_update, usetz = TRUE))
    #

    # # retrieve logger data
    # get_logger_data <- reactivePoll(
    #   30000, session,
    #   # checks every few seconds if the google spreadsheet online was updated
    #   checkFunc = function() {
    #     # check for updated tag on the logger spreadsheet
    #     last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
    #     message("INFO: Checking google spreadsheet, last update: ", format(last_update, usetz = TRUE))
    #     isolate({
    #       if (is.null(values$last_gs_update) || values$last_gs_update != last_update)
    #         values$last_gs_update <- last_update
    #     })
    #     return(last_update)
    #   },
    #   # reads the log files
    #   valueFunc = function() {
    #
    #     # older RData chached data
    #     raw <- data_frame()
    #     for (file in list.files(path = file.path(base_dir, DATA_DIR), pattern = "*data_log\\.RData", full.names = TRUE)) {
    #       load(file = file)
    #       if (exists("cached_data")) {
    #         message("INFO: Loading cached RData ", basename(file))
    #         raw <- bind_rows(raw, cached_data)
    #         rm("cached_data")
    #       } else {
    #         message("INFO: Failed to load cached RData from ", basename(file))
    #       }
    #     }
    #
    #     # newest data
    #     last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
    #     last_log_file <- file.path(base_dir, DATA_DIR, format(last_update, "%Y%m%d_%H%M%S_gs_log.csv"))
    #
    #     if (file.exists(last_log_file)) {
    #       # most recent data available locally
    #       message("INFO: Last log file of google spreadsheet data already cached --> read local csv")
    #       raw <- bind_rows(raw, read.csv(last_log_file))
    #     } else {
    #       # go from spread sheet instead
    #       message("INFO: Loading google spreadsheet data")
    #       withProgress(
    #         message = 'Retrieving new data from google spreadsheet',
    #         detail = 'This may take a moment...', value = 0.25, {
    #           raw.gs <- gs_title(global$gs_title) %>% gs_read_csv(ws = 1)
    #           raw <- bind_rows(raw, raw.gs)
    #           write.table(raw.gs, file = file.path(base_dir, DATA_DIR, format(last_update, "%Y%m%d_%H%M%S_gs_log.csv")), row.names = FALSE, sep = ",", col.names = TRUE)
    #           old_logs <- list.files(path = file.path(base_dir, DATA_DIR), pattern = "*gs_log\\.csv", full.names = TRUE) %>% sort()  %>% tail(-5)
    #           if (length(old_logs) > 0) {
    #             message("INFO: Data loaded, deleting ", length(old_logs), " old log files (before last 5)")
    #             file.remove(old_logs)
    #           }
    #         })
    #     }
    #     return(raw)
    #   }
    # )

    # # retrieve processed logger data
    # processed_logger_data <- reactive({
    #   raw_data <- get_logger_data()
    #   message("INFO: ", nrow(raw_data), " entries of M800 data loaded")
    #
    #   # datetime
    #   data <- raw_data %>%
    #     mutate(
    #       datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S"),
    #       configuration = NA
    #     )
    #
    #   # assign data to configurations
    #   for (i in 1:nrow(configs)) {
    #     start_date <- configs$first_tp[i]
    #     end_date <- configs$last_tp[i]
    #     in_config <-
    #       (is.na(start_date) | data$datetime >= start_date) &
    #       (is.na(end_date) | data$datetime <= end_date)
    #     data <- data %>% mutate(configuration = ifelse(in_config, configs$configuration[i], configuration))
    #   }
    #   message("INFO: ", nrow(filter(data, is.na(configuration))), " entries of M800 data discarded because they do not belong to any configuration")
    #
    #   # walk through configurations to regexp check the data and transform into long format
    #   number_regexp <- "(-?[0-9]+\\.?[0-9]*|----|\\*{4})"
    #   config_channels <- channels %>%
    #     group_by(configuration) %>%
    #     summarize(regexp = paste0(number_regexp, "\\s*,", trace, ",") %>% paste(collapse=""))
    #   data_gathered <- data_frame()
    #   for (i in 1:nrow(config_channels)) {
    #     config_id <- config_channels$configuration[i]
    #     config_data <- data %>%
    #       filter(configuration == config_id) %>%
    #       mutate(data_err = !grepl(config_channels$regexp[i], data))
    #     message("INFO: ", config_data %>% filter(data_err) %>% nrow(),
    #             "/", config_data %>% nrow(),
    #             " entries of M800 data in configuration ", config_id," discarded because of data errors")
    #     message("EXAMPLES of discards:")
    #     if ( (error_rows <- config_data %>% filter(data_err) %>% nrow()) > 0) {
    #       print(config_data %>% filter(data_err) %>% sample_n(min(5, error_rows)) %>% select(data))
    #     }
    #     message("\n")
    #     # extract and combine with other configurations
    #     if (nrow(config_data) > 0) {
    #       parsed_config_data <-
    #         suppressWarnings(
    #           config_data %>%
    #             select(datetime, device, type, data, configuration) %>%
    #             extract(data, into=as.character(filter(channels, configuration == config_id)$output), config_channels$regexp[i], remove = T, convert = T) %>% # extract data
    #             gather("output", "value", -c(datetime, device, type, configuration)) %>% # melt data
    #             left_join(channels, by = c("configuration", "output")) %>% # add the setup information of the probes
    #             as_data_frame() %>%
    #             mutate(value = as.numeric(value)) %>%
    #             filter(!is.na(value)) %>%
    #             # include time.hrs column
    #             group_by(vessel) %>% mutate(time.hrs = difftime(datetime, min(datetime)[1]) %>% as.double(units = "hours")) %>% ungroup()
    #         )
    #       data_gathered <- bind_rows(data_gathered, parsed_config_data)
    #     }
    #   }
    #
    #   # return result
    #   if (nrow(data_gathered) == 0) return(NULL)
    #
    #   data_gathered %>%
    #     left_join(data_types, by = "trace")
    # })
    #
    # #--- OUTPUTS
    # output$last_gs_update <- renderText({
    #   req(values$last_gs_update)
    #   return(values$last_gs_update %>% format("Latest data recorded at %H:%M:%S (%d %b %Y %Z)"))
    # })
    #
    # # filters
    # observe({
    #   data <- processed_logger_data()
    #   time_range <- range(data$datetime)
    #   message("INFO: update filters")
    #   isolate({
    #     if (is.null(values$date_filter)) {
    #       # no start and end date set yet
    #       values$date_filter <- as.Date(time_range)
    #       updateDateRangeInput(session, "date_range",
    #                            min = time_range[1], max = time_range[2] + 60*60*24,
    #                            start = time_range[1], end = time_range[2])
    #     } else if (!setequal(values$date_filter, as.Date(time_range))) {
    #       # just update the date range limits
    #       values$date_filter <- as.Date(time_range)
    #       updateDateRangeInput(session, "date_range",
    #                            min = time_range[1], max = time_range[2] + 60*60*24)
    #     }
    #
    #     vessels <- (data %>% filter(!is.na(vessel), vessel != ""))$vessel %>% unique()
    #     if (!setequal(vessels, values$vessels)) {
    #       values$vessels <- vessels
    #       updateCheckboxGroupInput(session, "vessels", choices = vessels, sel = c())
    #     }
    #
    #     traces <- data$trace %>% unique()
    #     if (!setequal(traces, values$traces)) {
    #       values$traces <- traces
    #       updateCheckboxGroupInput(session, "traces", choices = traces, sel = c())
    #     }
    #
    #   })
    # })

    # # get data for plot
    # get_plot_data <- reactive({
    #   validate(
    #     need(input$vessels, message = "no experiments selected"),
    #     need(input$traces, message = "no data traces selected"),
    #     need(input$date_range, message = "no dates selected"))
    #
    #   m800 <- processed_logger_data()
    #
    #   isolate({
    #     if (is.null(m800)) return(NULL)
    #     df <- m800 %>%
    #       filter(vessel %in% input$vessels, trace %in% input$traces) %>%
    #       filter(as.Date(datetime) >= input$date_range[1] & as.Date(datetime) <= input$date_range[2])
    #     if (nrow(df) == 0) return(NULL)
    #     return(as_data_frame(df))
    #   })
    # })
    #
    # # main plot
    # make_main_plot <- reactive({
    #   validate(need(get_plot_data(), message = "no data selected"))
    #   message("INFO: generating main plot")
    #   withProgress(message = 'Rendering plot...', value = 0.2, {
    #     setProgress(detail = 'Retrieving plot data ...', value = 0.3)
    #     data <- get_plot_data()
    #     print(head(data))
    #     setProgress(detail = 'Generating graph ...', value = 0.5)
    #     if (input$time_format == "time.hrs") {
    #       p <- ggplot(data) +
    #         aes(time.hrs, value, color = trace)
    #     } else {
    #       p <- ggplot(data) +
    #         aes(datetime, value, color = trace)
    #     }
    #     p <- p +
    #       geom_line() +
    #       facet_grid(data_type~vessel, scales = "free") +
    #       theme_bw() +
    #       labs(color = "Trace")
    #     # if (isolate(input$legend) == "below")
    #     #   p <- p + theme(legend.position = "bottom")
    #     return(p)
    #   })
    # })
    #
    # # get data for export
    # get_export_data <- reactive({
    #   data <- get_plot_data()
    #   print(names(data))
    #   return(data %>% head())
    # })


    # # downloads
    # observe({
    #   if (!is.null(input$vessels) & !is.null(input$traces) && !is.null(input$date_range) && !is.null(get_plot_data()) ) {
    #     shinyjs::show("dl_actions")
    #   } else {
    #     shinyjs::hide("dl_actions")
    #   }
    # })
    #
    # output$dl_plot <- downloadHandler(
    #   filename = function() {paste0(format(Sys.time(), format = "%Y%m%d_%H%M"), "_chemostat_overview.pdf")},
    #   content = function(file) {
    #     device <- function(..., version="1.4") grDevices::pdf(..., version=version)
    #     ggsave(file = file, plot = make_main_plot(), width = 10, height = 8, device = device)
    #   })
    #
    # output$dl_excel <- downloadHandler(
    #   filename = function() {paste0(format(Sys.time(), format = "%Y%m%d_%H%M"), "_chemostat_data.xlsx")},
    #   content = function(file) {
    #     get_plot_data() %>%
    #       select(datetime, time.hrs, vessel, trace, value) %>%
    #       arrange(vessel, datetime) %>% distinct() %>% # make sure there are no replicate issues (might discard some replicate timepoint data)
    #       spread(trace, value) %>%
    #       openxlsx::write.xlsx(file = file)
    #   })
    #
    # output$dl_data <- downloadHandler(
    #   filename = function() {paste0(format(Sys.time(), format = "%Y%m%d_%H%M"), "_chemostat_data.RData")},
    #   content = function(file) {
    #     data <- get_plot_data()
    #     save(data, file = file)
    #   })

    # # plots
    # output$main_plot <- renderPlotly({
    #   ggplotly(make_main_plot(), tooltip = "all")
    # })



  })
}
