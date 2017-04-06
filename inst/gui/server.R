

server <- function(input, output, session) {

  # STARTUP =======
  base_dir <- .GlobalEnv$.base_dir
  if (!dir.exists(base_dir)) dir.create(base_dir)

  message("\n***************************************************************",
          "\nINFO: Launching GUI ...",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", base_dir,
          "\nINFO: Settings file: ", file.path(base_dir, SETTINGS_FILE))

  # google spreadsheet authentication
  options(googlesheets.httr_oauth_cache = FALSE)
  if (file.exists(file.path(.base_dir, "gs_token.rds"))) {
    message("INFO: Google spreadsheet token found, authenticating...")
    gs_auth(token = file.path(.base_dir, "gs_token.rds"))
  } else {
    stop("No googlespreadsheet authentication information found. Please run generate_gs_token() function from interactive environment once first (e.g. from RStudio) and indicate the target_directory for the GUI data.", call. = FALSE)
  }

  # SETTINGS =======
  global <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "global")
  configs <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "configurations")
  channels <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "channels")
  data_types <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "data_types")
  live <- read_excel(file.path(base_dir, SETTINGS_FILE), sheet = "live")
  message("INFO: Authenticated and all settings loaded.")
  last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
  if (length(last_update) == 0) stop("Cannot find log google spreadsheet ", global$gs_title, call. = F)
  message("INFO: Last update of google spreadsheet ", format(last_update, usetz = TRUE))

  # REACTIVE VALUES ----
  values <- reactiveValues(
    last_gs_update = NULL,
    vessels = c(),
    traces = c(),
    date_filter = NULL
  )

  # retrieve logger data
  get_logger_data <- reactivePoll(
    30000, session,
    # checks every few seconds if the google spreadsheet online was updated
    checkFunc = function() {
      # check for updated tag on the logger spreadsheet
      last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
      message("INFO: Checking google spreadsheet, last update: ", format(last_update, usetz = TRUE))
      isolate({
        if (is.null(values$last_gs_update) || values$last_gs_update != last_update)
          values$last_gs_update <- last_update
      })
      return(last_update)
    },
    # reads the log files
    valueFunc = function() {

      # older RData chached data
      raw <- data_frame()
      for (file in list.files(path = file.path(base_dir, DATA_DIR), pattern = "*data_log\\.RData", full.names = TRUE)) {
        load(file = file)
        if (exists("cached_data")) {
          message("INFO: Loading cached RData ", basename(file))
          raw <- bind_rows(raw, cached_data)
          rm("cached_data")
        } else {
          message("INFO: Failed to load cached RData from ", basename(file))
        }
      }

      # newest data
      last_update <- subset(gs_ls(), sheet_title == global$gs_title)$updated
      last_log_file <- file.path(base_dir, DATA_DIR, format(last_update, "%Y%m%d_%H%M%S_gs_log.csv"))

      if (file.exists(last_log_file)) {
        # most recent data available locally
        message("INFO: Last log file of google spreadsheet data already cached --> read local csv")
        raw <- bind_rows(raw, read.csv(last_log_file))
      } else {
        # go from spread sheet instead
        message("INFO: Loading google spreadsheet data")
        withProgress(
          message = 'Retrieving new data from google spreadsheet',
          detail = 'This may take a moment...', value = 0.25, {
            raw.gs <- gs_title(global$gs_title) %>% gs_read_csv(ws = 1)
            raw <- bind_rows(raw, raw.gs)
            write.table(raw.gs, file = file.path(base_dir, DATA_DIR, format(last_update, "%Y%m%d_%H%M%S_gs_log.csv")), row.names = FALSE, sep = ",", col.names = TRUE)
            old_logs <- list.files(path = file.path(base_dir, DATA_DIR), pattern = "*gs_log\\.csv", full.names = TRUE) %>% sort()  %>% tail(-5)
            if (length(old_logs) > 0) {
              message("INFO: Data loaded, deleting ", length(old_logs), " old log files (before last 5)")
              file.remove(old_logs)
            }
          })
      }
      return(raw)
    }
  )

  # retrieve processed logger data
  processed_logger_data <- reactive({
    raw_data <- get_logger_data()
    message("INFO: ", nrow(raw_data), " entries of M800 data loaded")

    # datetime
    data <- raw_data %>%
      mutate(
        datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S"),
        configuration = NA
      )

    # assign data to configurations
    for (i in 1:nrow(configs)) {
      start_date <- configs$first_tp[i]
      end_date <- configs$last_tp[i]
      in_config <-
        (is.na(start_date) | data$datetime >= start_date) &
        (is.na(end_date) | data$datetime <= end_date)
      data <- data %>% mutate(configuration = ifelse(in_config, configs$configuration[i], configuration))
    }
    message("INFO: ", nrow(filter(data, is.na(configuration))), " entries of M800 data discarded because they do not belong to any configuration")

    # walk through configurations to regexp check the data and transform into long format
    number_regexp <- "(-?[0-9]+\\.?[0-9]*|----|\\*{4})"
    config_channels <- channels %>%
      group_by(configuration) %>%
      summarize(regexp = paste0(number_regexp, "\\s*,", trace, ",") %>% paste(collapse=""))
    data_gathered <- data_frame()
    for (i in 1:nrow(config_channels)) {
      config_id <- config_channels$configuration[i]
      config_data <- data %>%
        filter(configuration == config_id) %>%
        mutate(data_err = !grepl(config_channels$regexp[i], data))
      message("INFO: ", config_data %>% filter(data_err) %>% nrow(),
              "/", config_data %>% nrow(),
              " entries of M800 data in configuration ", config_id," discarded because of data errors")
      message("EXAMPLES of discards:")
      if ( (error_rows <- config_data %>% filter(data_err) %>% nrow()) > 0) {
        print(config_data %>% filter(data_err) %>% sample_n(min(5, error_rows)) %>% select(data))
      }
      message("\n")
      # extract and combine with other configurations
      if (nrow(config_data) > 0) {
        parsed_config_data <-
          suppressWarnings(
            config_data %>%
              select(datetime, device, type, data, configuration) %>%
              extract(data, into=as.character(filter(channels, configuration == config_id)$output), config_channels$regexp[i], remove = T, convert = T) %>% # extract data
              gather("output", "value", -c(datetime, device, type, configuration)) %>% # melt data
              left_join(channels, by = c("configuration", "output")) %>% # add the setup information of the probes
              as_data_frame() %>%
              mutate(value = as.numeric(value)) %>%
              filter(!is.na(value)))
        data_gathered <- bind_rows(data_gathered, parsed_config_data)
      }
    }

    # return result
    if (nrow(data_gathered) == 0) return(NULL)

    data_gathered %>%
      left_join(data_types, by = "trace")
  })

  #--- OUTPUTS
  output$last_gs_update <- renderText({
    req(values$last_gs_update)
    return(values$last_gs_update %>% format("Latest data recorded at %H:%M:%S (%d %b %Y %Z)"))
  })

  # filters
  observe({
    data <- processed_logger_data()
    time_range <- range(data$datetime)
    message("INFO: update filters")
    isolate({
      if (is.null(values$date_filter)) {
        # no start and end date set yet
        values$date_filter <- as.Date(time_range)
        updateDateRangeInput(session, "date_range",
                             min = time_range[1], max = time_range[2] + 60*60*24,
                             start = time_range[1], end = time_range[2])
      } else if (!setequal(values$date_filter, as.Date(time_range))) {
        # just update the date range limits
        values$date_filter <- as.Date(time_range)
        updateDateRangeInput(session, "date_range",
                             min = time_range[1], max = time_range[2] + 60*60*24)
      }

      vessels <- (data %>% filter(!is.na(vessel), vessel != ""))$vessel %>% unique()
      if (!setequal(vessels, values$vessels)) {
        values$vessels <- vessels
        updateCheckboxGroupInput(session, "vessels", choices = vessels, sel = c())
      }

      traces <- data$trace %>% unique()
      if (!setequal(traces, values$traces)) {
        values$traces <- traces
        updateCheckboxGroupInput(session, "traces", choices = traces, sel = traces)
      }

    })
  })

  # get data for plot
  get_plot_data <- reactive({
    validate(
      need(input$vessels, message = "no experiments selected"),
      need(input$traces, message = "no data traces selected"),
      need(input$date_range, message = "no dates selected"))

    m800 <- processed_logger_data()

    isolate({
      if (is.null(m800)) return(NULL)
      df <- m800 %>%
        filter(vessel %in% input$vessels, trace %in% input$traces) %>%
        filter(as.Date(datetime) >= input$date_range[1] & as.Date(datetime) <= input$date_range[2])
      if (nrow(df) == 0) return(NULL)
      return(df)
    })
  })

  # main plot
  make_main_plot <- reactive({
    validate(need(get_plot_data(), message = "no data selected"))
    message("INFO: generating main plot")
    withProgress(message = 'Rendering plot...', value = 0.2, {
      setProgress(detail = 'Retrieving plot data ...', value = 0.3)
      data <- get_plot_data()
      print(head(data))
      setProgress(detail = 'Generating graph ...', value = 0.5)
      p <- ggplot(data) +
        aes(datetime, value, color = trace) +
        geom_line() +
        facet_grid(data_type~vessel, scales = "free") +
        theme_bw() +
        labs(color = "Trace")
      # if (isolate(input$legend) == "below")
      #   p <- p + theme(legend.position = "bottom")
      return(p)
    })
  })

  output$save <- downloadHandler(
    filename = function() {paste0(format(Sys.time(), format = "%Y%m%d_%H%M"), "_chemostat_overview.pdf")},
    content = function(file) {
      device <- function(..., version="1.4") grDevices::pdf(..., version=version)
      ggsave(file = file, plot = make_main_plot(), width = 10, height = 8, device = device)
    })

  output$main_plot <- renderPlotly({
    ggplotly(make_main_plot(), tooltip = "all")
  })


  #' RASPI CAMS ====

  output$raspicams <- renderUI({

    # retrieve raspi camp ip addresses
    raspicams <- gs_title(global$gs_title) %>% gs_read_csv(ws = "picams")

    boxes <- list()

    for (i in 1:nrow(raspicams)) {
      camera_box <- box(title = paste(raspicams[i,"camera"], "camera"), collapsible = TRUE, collapsed = TRUE,
                        width = 12, status = "success", solidHeader = TRUE,
                        tags$iframe(src=paste0("http://", raspicams[i, "ip"], ":8081"), width = "640px", height = "480px", frameborder="0"))

      boxes <- c(boxes, list(column(width = 12, camera_box)))
    }

    return(tagAppendChildren(fluidRow(), list = boxes))
  })

}
