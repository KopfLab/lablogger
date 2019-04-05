
dataPlotServer <- function(input, output, session, timezone, get_experiments, get_data_logs, refresh_data_logs, reset_plot) {

  # namespace
  ns <- session$ns

  # reactive values  ====
  zoom_factor <- 2 # zoom in and out factor with each click
  zoom_move <- 0.5 # sideways move interval
  values <- reactiveValues(
    valid_fetch = FALSE,
    valid_plot = FALSE,
    selected_traces = c(),
    selected_data_groups = c(),
    refresh_data_plot = NULL,
    zoom_stack = list(list(zoom = NULL, x_min = NULL, x_max = NULL))
  )

  # experiment selected
  is_experiment_selected <- reactive(length(get_experiments()) > 0)

  # reset =====
  observeEvent(reset_plot(), {
    module_message(ns, "debug", "resetting data plot - exp selected: ", is_experiment_selected())
    values$valid_fetch <- FALSE
    values$valid_plot <- FALSE
    values$zoom_stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
    toggle("data_plot_div", condition = FALSE)
    toggle("traces_box", condition = FALSE)
    toggle("groups_box", condition = FALSE)
    toggle("options_box", condition = FALSE)
    toggle("summary_box", condition = FALSE)
  })

  # plot buttons ====
  observeEvent(is_experiment_selected(), {
    toggleState("fetch_data", condition = is_experiment_selected())
    toggleState("reset_cache", condition = is_experiment_selected())
  })

  observeEvent(values$valid_plot, {
    toggleState("zoom_all", condition = values$valid_plot)
    toggleState("zoom_move_left", condition = values$valid_plot)
    toggleState("zoom_move_right", condition = values$valid_plot)
    toggleState("zoom_back", condition = values$valid_plot)
    toggleState("plot_download-download_dialog", condition = values$valid_plot)
    toggleState("data_download", condition = values$valid_plot)
  })

  observe({
    refresh_available <- values$valid_fetch && length(traces_selector$get_selected()) > 0 && length(groups_selector$get_selected() > 0)
    toggleState("plot_refresh", condition = refresh_available)
    toggleState("traces_refresh", condition = refresh_available)
    toggleState("groups_refresh", condition = refresh_available)
    toggleState("options_refresh", condition = refresh_available)
  })

  # plot messages ====
  output$data_plot_message <- renderText({
    validate(
      need(is_experiment_selected(), "Please select an experiment.") %then%
        need(values$valid_fetch, "Please 'Fetch Data' to query the database.") %then%
          need(traces_selector$get_selected(), "Please select at least one data trace.") %then%
            need(groups_selector$get_selected(), "Please select at least one data group.") %then%
              need(values$valid_plot, "Please press any 're-plot' button to render the plot.")
    )
    NULL
  })

  # traces ====

  # selector
  traces_selector <- callModule(selectorTableServer, "traces_selector", id_column = "data_trace", col_headers = c("Trace"))

  # update data
  observe({
    df <- get_data_logs() %>% prepare_data_for_plotting()
    isolate({
      if (nrow(df) > 0) {
        traces_selector$set_table(df %>% select(data_trace) %>% unique() %>% arrange(data_trace))
      } else {
        traces_selector$set_table(data_frame(data_trace = character(0)))
      }
    })
  })

  # groups ====

  # selector
  groups_selector <- callModule(selectorTableServer, "groups_selector", id_column = "data_group", col_headers = c("Group"))

  # update data
  NO_DATA_GROUP <- "-- no data group --"
  observe({
    df <- get_data_logs() %>% prepare_data_for_plotting()
    isolate({
      if (nrow(df) > 0) {
        df <- df %>% select(data_group) %>% unique() %>% arrange(data_group) %>%
          mutate(data_group = ifelse(is.na(data_group), NO_DATA_GROUP, data_group))
        groups_selector$set_table(df)
        # select all by default if plot doesn't exist yet
        if (!values$valid_plot) groups_selector$set_selected(df$data_group)
      } else {
        groups_selector$set_table(data_frame(data_group = character(0)))
      }
    })
  })

  # zooming ====

  get_data_logs_in_time_interval <- function(logs, from, to) {
    filter(logs, between(datetime, as_datetime(from, tz = timezone), as_datetime(to, tz = timezone)))
  }

  # add to zoom stack
  add_to_zoom_stack <- function(zoom, x_min, x_max, update = TRUE, only_add_if_new = TRUE) {
    if (missing(zoom)) zoom <- get_last_zoom()$zoom
    if (missing(x_min)) x_min <- get_last_zoom()$x_min
    if (missing(x_max)) x_max <- get_last_zoom()$x_max
    new_zoom <- list(zoom = zoom, x_min = x_min, x_max = x_max)
    if (only_add_if_new && identical(get_last_zoom(), new_zoom)) return(NULL)
    module_message(ns, "debug", "adding to zoom stack: ", zoom, " time: ", x_min, " to ", x_max)
    values$zoom_stack <- c(values$zoom_stack, list(new_zoom))
    if (update) refresh_data_plot()
  }

  # load last zoom
  load_last_zoom <- function(update = TRUE) {
    last_element <- length(values$zoom_stack)
    if (last_element > 1) values$zoom_stack[last_element] <- NULL
    if (update) refresh_data_plot()
  }

  # get current zoom
  get_last_zoom <- function() {
    values$zoom_stack[[length(values$zoom_stack)]]
  }

  # zoom back
  observeEvent(input$zoom_back, load_last_zoom())
  observeEvent(input$data_plot_dblclick, load_last_zoom())
  # zoom whole data set
  observeEvent(input$zoom_all, {
    add_to_zoom_stack(zoom = NULL, x_min = NULL, x_max = NULL)
  })
  # # zoom fit
  # observeEvent(input$zoom_fit, {
  #   add_to_zoom_stack(zoom = NULL)
  # })
  # # zoom in
  # observeEvent(input$zoom_in, {
  #   if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = zoom_factor)
  #   else add_to_zoom_stack(zoom = get_last_zoom()$zoom * zoom_factor)
  # })
  # # zoom out
  # observeEvent(input$zoom_out, {
  #   if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = 1/zoom_factor)
  #   else add_to_zoom_stack(zoom = get_last_zoom()$zoom/zoom_factor)
  # })
  # time zoom
  observeEvent(input$data_plot_brush, {
    brush <- input$data_plot_brush
    if (!is.null(brush$xmin) && !is.null(brush$xmax)) {
      # convert to seconds
      add_to_zoom_stack(x_min = brush$xmin, x_max = brush$xmax)
    }
  })
  # left right movening
  move_zoom <- function(direction) {
    if ( !is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max) ) {
      add_to_zoom_stack(
        x_min = get_last_zoom()$x_min + direction * zoom_move * (get_last_zoom()$x_max - get_last_zoom()$x_min),
        x_max = get_last_zoom()$x_max + direction * zoom_move * (get_last_zoom()$x_max - get_last_zoom()$x_min)
      )
    }
  }
  observeEvent(input$zoom_move_left, move_zoom(-1))
  observeEvent(input$zoom_move_right, move_zoom(+1))

  # fetch data ====
  observeEvent(input$fetch_data, {
    values$valid_fetch <- TRUE
    refresh_data_logs()
    get_data_logs()
    toggle("traces_box", condition = TRUE)
    toggle("groups_box", condition = TRUE)
    toggle("options_box", condition = TRUE)
    toggle("summary_box", condition = TRUE)
    #refresh_data_plot() # NOTE consider not triggering this here
  })

  # reset cache ====

  observeEvent(input$reset_cache, {
    values$valid_fetch <- FALSE

    withProgress(
      message = 'Resetting data logs cache', detail = "Accessing file system...", value = 0.5,
      ll_reset_exp_device_data_logs_cache(get_experiments())
    )
  })

  # generate data plot ====

  refresh_data_plot <- function() {
    if (is.null(values$refresh_data_plot)) values$refresh_data_plot <- 1
    else values$refresh_data_plot <- values$refresh_data_plot + 1
    toggle("data_plot_div", condition = TRUE)
  }

  observeEvent(input$plot_refresh, refresh_data_plot())
  observeEvent(input$traces_refresh, refresh_data_plot())
  observeEvent(input$groups_refresh, refresh_data_plot())
  observeEvent(input$options_refresh, refresh_data_plot())

  get_plot_data_logs <- reactive({
    logs <- get_data_logs() %>% prepare_data_for_plotting()

    # zoom
    if (!is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)) {
      logs <- get_data_logs_in_time_interval(logs, get_last_zoom()$x_min, get_last_zoom()$x_max)
    }

    # traces and groups filter
    traces <- traces_selector$get_selected()
    groups <- groups_selector$get_selected()
    groups[groups == NO_DATA_GROUP] <- NA_character_
    logs %>% filter(data_trace %in% traces, data_group %in% groups)
  })

  generate_data_plot <- eventReactive(values$refresh_data_plot, {

    # logs
    logs <- get_plot_data_logs()

    # plot
    if (nrow(logs) == 0) {
      p <- ggplot() + annotate(
        "text", x = 0, y = 0,
        label = glue("no data available for the\nselected filters and time interval\n",
                     "experiment(s): {paste(get_experiments(), collapse = ', ')}\n",
                     "trace(s): {paste(traces_selector$get_selected(), collapse = ', ')}\n",
                     "group(s): {paste(groups_selector$get_selected(), collapse = ', ')}"),
        vjust = 0.5, hjust = 0.5, size = 10) + theme_void()
    } else {

      # date breaks
      if (input$time_intervals_unit != "default" && !is.null(input$time_intervals_number) && input$time_intervals_number > 0) {
        date_breaks <- str_c(input$time_intervals_number, " ", input$time_intervals_unit)
      } else {
        date_breaks <- NULL
      }
      p <- ll_plot_device_data_logs(logs, date_breaks = date_breaks, show_error_range = input$show_errors, exclude_outliers = !input$show_outliers)

      # legend position
      if (input$legend_position == "bottom") {
        p <- p + theme(legend.position = "bottom", legend.direction="vertical")
      } else if (input$legend_position == "hide") {
        p <- p + theme(legend.position = "none")
      }

      # font size
      if (!is.null(input$font_size) && input$font_size > 0)
        p <- p + theme(text = element_text(size = input$font_size))

    }

    values$valid_plot <- TRUE
    return(p)
  })

  # generate data summary =====

  generate_data_summary <- eventReactive(values$refresh_data_plot, {
    logs <- get_plot_data_logs()
    if (nrow(logs) > 0) {
      logs <- logs %>% ll_summarize_data_logs(slope_denom_units = "day", exclude_outliers = !input$show_outliers)
    }
    return(logs)
  })

  # data plot output ====

  output$data_plot <- renderPlot(generate_data_plot(), height = eventReactive(values$refresh_data_plot, input$plot_height))

  # table output ====

  output$summary_table <- renderTable({
    summary <- generate_data_summary()
    module_message(ns, "debug", "rendering plot data summary table")
    if (nrow(summary) > 0) summary
    else data_frame(` ` = "No data.")
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL, digits = reactive(input$digits))

  # plot download ====
  download_handler <- callModule(
    plotDownloadServer, "plot_download",
    plot_func = generate_data_plot,
    filename_func = reactive({
      exps <- get_data_logs()$exp_id %>% unique()
      glue("{format(now(), '%Y_%m_%d')}-",
           "{glue::glue_collapse(exps, sep = '_')}",
           ".pdf")
    }))

}


dataPlotUI <- function(id, plot_height = 650) {
  ns <- NS(id)

  tagList(
    # plot box ------
    default_box(
      title = "Data Plot", width = 8,
      div(style = paste0("min-height: ", plot_height, "px;"),
          div(id = ns("data_plot_actions"),
              fluidRow(
                column(width = 4,
                       tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                                    tooltip = "Fetch the most recent data from the data base.") %>% disabled(),
                       spaces(1),
                       tooltipInput(actionButton, ns("reset_cache"), "Reset Cache", icon = icon("unlink"),
                                    tooltip = "Reset local cache (only use if experiment configuration changed).") %>% disabled()
                ),
                column(width = 4, align = "center",
                       tooltipInput(actionButton, ns("zoom_all"), "", icon = icon("resize-full", lib = "glyphicon"),
                                    tooltip = "Show all data") %>% disabled(),
                       # tooltipInput(actionButton, ns("zoom_in"), "", icon = icon("plus"),
                       #              tooltip = "Zoom in"),
                       # tooltipInput(actionButton, ns("zoom_out"), "", icon = icon("minus"),
                       #              tooltip = "Zoom out"),
                       # tooltipInput(actionButton, ns("zoom_fit"), "", icon = icon("resize-vertical", lib = "glyphicon"),
                       #             type = "toggle", tooltip = "Switch to optimal zoom<br/>for visible peaks"),
                       tooltipInput(actionButton, ns("zoom_move_left"), "", icon = icon("arrow-left"),
                                    tooltip = "Move back in time") %>% disabled(),
                       tooltipInput(actionButton, ns("zoom_move_right"), "", icon = icon("arrow-right"),
                                    tooltip = "Move forward in time") %>% disabled(),
                       tooltipInput(actionButton, ns("zoom_back"), "", icon = icon("rotate-left"),
                                    tooltip = "Revert to previous view") %>% disabled()
                ),
                column(width = 4, align = "right",
                       tooltipInput(actionButton, ns("plot_refresh"), "Re-plot", icon = icon("refresh"),
                                    tooltip = "Refresh the plot with the selected filters and plot options.") %>% disabled(),
                       spaces(1),
                       # FIXME
                       # tooltipInput(actionButton, ns("data_download"), "Data", icon = icon("download"),
                       #              tooltip = "Download the plotted data.") %>% disabled(),
                       # spaces(1),
                       plotDownloadLink(ns("plot_download"), label = "Save") %>% disabled()
                )
              )
          ),
          div(id = ns("data_plot_messages"), h3(textOutput(ns("data_plot_message")))),
          div(id = ns("data_plot_div"),
              plotOutput(ns("data_plot"), height = "100%",
                         dblclick = ns("data_plot_dblclick"),
                         brush = brushOpts(
                           id = ns("data_plot_brush"),
                           delayType = "debounce",
                           direction = "x",
                           resetOnNew = TRUE
                         )) %>%
                withSpinner(type = 5, proxy.height = paste0(plot_height - 50, "px"))
          )
      )
    ),

    # traces box ----
    div(id = ns("traces_box"),
      default_box(
        title = "Data Traces", width = 4,
        selectorTableUI(ns("traces_selector"), height = 120),
        footer = div(
          tooltipInput(actionButton, ns("traces_refresh"), label = "Re-plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new data trace selection."),
          spaces(1),
          selectorTableButtons(ns("traces_selector"))
        )
      )
    ) %>% hidden(),

    # groups box ----
    div(id = ns("groups_box"),
        default_box(
          title = "Data Groups", width = 4,
          selectorTableUI(ns("groups_selector"), height = 75),
          footer = div(
            tooltipInput(actionButton, ns("groups_refresh"), label = "Re-plot", icon = icon("refresh"),
                         tooltip = "Refresh plot with new data groups selection."),
            spaces(1),
            selectorTableButtons(ns("groups_selector"))
          )
        )
    ) %>% hidden(),

    # options box -----
    div(id = ns("options_box"),
        default_box(
          title = "Plot Options", width = 4,
          fluidRow(
            h4("Errors:") %>% column(width = 4),
            checkboxInput(ns("show_errors"), NULL, value = FALSE) %>%
              column(width = 2),
            h4("Outliers:") %>% column(width = 4),
            checkboxInput(ns("show_outliers"), NULL, value = TRUE) %>%
             column(width = 2)
            ),
          fluidRow(
            h4("Plot height:") %>% column(width = 4),
            numericInput(ns("plot_height"), NULL, value =  plot_height, min = 100, step = 50) %>%
              column(width = 8)),
          fluidRow(
            h4("Time intervals:") %>% column(width = 4),
            numericInput(ns("time_intervals_number"), NULL, value = NA, min = 1, step = 1) %>% column(width = 3),
            selectInput(ns("time_intervals_unit"), NULL, choices = c("default", "mins", "hours", "days"), selected = "default") %>% column(width = 5)
          ),
          fluidRow(
            h4("Legend:") %>% column(width = 4),
            selectInput(ns("legend_position"), NULL, choices = c("right", "bottom", "hide"), selected = "bottom") %>% column(width = 8)
          ),
          fluidRow(
            h4("Font Size:") %>% column(width = 4),
            numericInput(ns("font_size"), NULL, value = 18, min = 6, step = 1) %>%
              column(width = 8)
          ),
          footer = tooltipInput(actionButton, ns("options_refresh"), label = "Re-plot",
                                icon = icon("refresh"),
                                tooltip = "Refresh plot with new plot settings.") %>% disabled()
        )
    ) %>% hidden(),

    # summary box -----
    div(id = ns("summary_box"),
        default_box(
          title = "Summary of Plotted Data", width = 12,
          tooltipInput(numericInput, ns("digits"), label = NULL, value = 2, step = 1, tooltip = "Enter number of digits to display."),
          tableOutput(ns("summary_table"))
        )
    ) %>% hidden()

  )

}
