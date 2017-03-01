

server <- function(input, output, session) {

  # STARTUP =======
  data_dir <- .GlobalEnv$.base_dir
  if (!dir.exists(data_dir)) dir.create(data_dir)

  message("\n***************************************************************",
          "\nINFO: Launching GUI ...",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", data_dir,
          "\nINFO: Settings file: ", file.path(data_dir, SETTINGS_FILE))

  # google spreadsheet authentication
  options(googlesheets.httr_oauth_cache = FALSE)
  if (file.exists(file.path(.base_dir, "gs_token.rds"))) {
    message("INFO: Google spreadsheet token found, authenticating...")
    gs_auth(token = file.path(.base_dir, "gs_token.rds"))
  } else {
    stop("No googlespreadsheet authentication information found. Please run generate_gs_token() function from interactive environment once first (e.g. from RStudio) and indicate the target_directory for the GUI data.", call. = FALSE)
  }

  # SETTINGS =======
  global <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "global")
  configs <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "configurations")
  channels <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "channels")
  data_types <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "data_types")
  live <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "live")
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
      # read the data
      message("INFO: Loading google spreadsheet data")
      withProgress(
        message = 'Retrieving new data from google spreadsheet',
        detail = 'This may take a moment...', value = 0.25, {
          raw <- gs_title(global$gs_title) %>% gs_read_csv(ws = 1)
          write.table(raw, file = file.path(data_dir, "gs_log.csv"),
                      row.names = FALSE, sep = ",", col.names = TRUE)
          #raw <- read.csv(file = file.path(data_dir, "gs_log.csv"))
        })
      return(raw)
    }
  )

  # retrieve processed logger data
  processed_logger_data <- reactive({
    raw_data <- get_logger_data()
    message("INFO: ", nrow(raw_data), " entries of M800 data loaded")

    datetime_regexp <- "[0-9]{1,2}/[a-zA-z]{3}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}"
    number_regexp <- "(-?[0-9]+\\.?[0-9]*|----|\\*{4})"
    # number_regexp <- "(?:[+-]?)(?:(?=[.]?[0-9])(?:[0-9]*)(?:(?:[.])(?:[0-9]{0,}))?)" ## for full scale

    # remove problematic data entries because of datetime issues
    data <- raw_data %>% mutate(
      datetime_err = !grepl(datetime_regexp, datetime))
    message("INFO: ", nrow(subset(data, datetime_err)), " entries of M800 data discarded because of datetime errors")

    # assign data to configurations
    data <- data %>%
      filter(!datetime_err)
      mutate(
        datetime = as.POSIXct(datetime, format = "%d/%b/%Y %H:%M:%S"),
        configuration = NA) %>%
        filter(!is.na(datetime), c(0,diff(datetime)) >= 0 & c(diff(datetime),0) >= 0)  # remove entries with correctly formatted but incorrectly transmitted datetimes
    for (i in 1:nrow(configs)) {
      start_date <- configs$first_tp[i]
      end_date <- configs$last_tp[i]
      in_config <-
        (rep(is.na(start_date), nrow(data)) | data$datetime >= rep(start_date, nrow(data))) &
        (rep(is.na(end_date), nrow(data)) | data$datetime <= rep(end_date, nrow(data)))
      data <- data %>% mutate(configuration = ifelse(in_config, configs$configuration[i], configuration))
    }
    message("INFO: ", nrow(filter(data, is.na(configuration))), " entries of M800 data discarded because they do not belong to any configuration")

    # walk through configurations to regexp check the data and transform into long format
    config_channels <- channels %>%
      group_by(configuration) %>%
      summarize(regexp = paste0(number_regexp, "\\s*,", trace, ",") %>% paste(collapse=""))
    data_gathered <- data_frame()
    for (i in 1:nrow(config_channels)) {
      config_id <- config_channels$configuration[i]
      #print(config_channels$regexp[i]) # FIXME
      config_data <- data %>%
        filter(configuration == config_id) %>%
        mutate(data_err = !grepl(config_channels$regexp[i], data))
      message("INFO: ", config_data %>% filter(data_err) %>% nrow(),
              "/", config_data %>% nrow(),
              " entries of M800 data in configuration ", config_id," discarded because of data errors")
      message("EXAMPLES of discards:")
      print(config_data %>% filter(data_err) %>% sample_n(5) %>% select(datetime, data))
      message("\n")
      # extract and combine with other configurations
      data_gathered <- bind_rows(
        data_gathered,
        suppressWarnings(config_data %>%
          select(datetime, device, type, data, configuration) %>%
          extract(data, into=as.character(filter(channels, configuration == config_id)$output), config_channels$regexp[i], remove = T, convert = T) %>% # extract data
          gather("output", "value", -c(datetime, device, type, configuration)) %>% # melt data
          left_join(channels, by = c("configuration", "output")) %>% # add the setup information of the probes
          as_data_frame() %>%
          mutate(value = as.numeric(value)) %>%
          filter(!is.na(value)))
      )
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
      } else if (values$date_filter != as.Date(time_range)) {
        # just update the date range limits
        values$date_filter <- as.Date(time_range)
        updateDateRangeInput(session, "date_range",
                             min = time_range[1], max = time_range[2] + 60*60*24)
      }

      vessels <- (data %>% filter(!is.na(vessel), vessel != ""))$vessel %>% unique()
      if (!setequal(vessels, values$vessels)) {
        values$vessels <- vessels
        updateCheckboxGroupInput(session, "vessels", choices = vessels, sel = vessels)
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
      need(input$vessels, message = "no vessels selected"),
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

}
