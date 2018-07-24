
dataPlotServer <- function(input, output, session, data_manager) {

  # namespace
  ns <- session$ns

  # zooming ====
  zoom_factor <- 2 # zoom in and out factor with each click
  zoom_move <- 0.5 # sideways move interval
  values <- reactiveValues(
    refresh_data_plot = NULL,
    valid_plot = FALSE,
    zoom_stack = list(list(zoom = NULL, x_min = NULL, x_max = NULL))
  )

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

  # reset zoom stack
  observeEvent(data_manager$get_selected_experiments(), {
    values$zoom_stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
  })

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

  # visibility of plot messages and main div ====
  observe({
    toggle("data_plot_div", condition = length(data_manager$get_selected_experiments()) > 0)
    toggle("data_plot_messages", condition = length(data_manager$get_selected_experiments()) == 0 | !values$valid_plot)
    toggleState("fetch_data", condition = length(data_manager$get_selected_experiments()) > 0)
  })

  # plot messages ====
  output$data_plot_message <- renderText({
    validate(
      need(length(data_manager$get_selected_experiments()) > 0, "Please select at least one experiment.") %then%
      need(values$valid_plot, "Please 'Fetch Data' to query the database.")
    )
  })

  # fetch data ====
  observeEvent(input$fetch_data, {
    data_manager$refresh_device_data_logs()
    data_manager$get_device_data_logs()
    toggleState("render_plot", TRUE)
    refresh_data_plot() # NOTE consider not triggering this here
  })

  # data plot buttons

  observeEvent(values$valid_plot, {
    toggleState("zoom_all", condition = values$valid_plot)
    toggleState("zoom_move_left", condition = values$valid_plot)
    toggleState("zoom_move_right", condition = values$valid_plot)
    toggleState("zoom_back", condition = values$valid_plot)
    toggleState("plot_download-download_dialog", condition = values$valid_plot)
    toggleState("options_refresh", condition = values$valid_plot)
  })

  # generate data plot ====

  refresh_data_plot <- function() {
     if (is.null(values$refresh_data_plot)) values$refresh_data_plot <- 1
     else values$refresh_data_plot <- values$refresh_data_plot + 1
  }

  observeEvent(input$render_plot, refresh_data_plot())
  observeEvent(input$options_refresh, refresh_data_plot())

  generate_data_plot <- eventReactive(values$refresh_data_plot, {
    message("INFO: generating data plot")
    logs <- data_manager$get_device_data_logs()
    if (!is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)) {
      logs <- data_manager$get_device_data_logs_in_time_interval(logs, get_last_zoom()$x_min, get_last_zoom()$x_max)
    }
    if (nrow(logs) == 0) {
      values$valid_plot <- FALSE
      ggplot() + annotate("text", x = 0, y = 0, label = "no data available yet for\nthe selected experiment(s)", vjust = 0.5, hjust = 0.5, size = 10) + theme_void()
    } else {
      values$valid_plot <- TRUE

      # date breaks
      if (input$time_intervals_unit != "default" && !is.null(input$time_intervals_number) && input$time_intervals_number > 0) {
        date_breaks <- str_c(input$time_intervals_number, " ", input$time_intervals_unit)
      } else {
        date_breaks <- NULL
      }
      p <- ll_plot_device_data_logs(logs, date_breaks = date_breaks, show_error_range = input$show_errors)

      # legend position
      if (input$legend_position == "bottom") {
        p <- p + theme(legend.position = "bottom", legend.direction="vertical")
      } else if (input$legend_position == "hide") {
        p <- p + theme(legend.position = "none")
      }

      # font size
      if (!is.null(input$font_size) && input$font_size > 0)
        p <- p + theme(text = element_text(size = input$font_size))

      return(p)
    }
  })

  # data plot output ====

  output$data_plot <- renderPlot(generate_data_plot(), height = eventReactive(values$refresh_data_plot, input$plot_height))

  # plot download ====
  download_handler <- callModule(
    plotDownloadServer, "plot_download",
    plot_func = generate_data_plot,
    filename_func = reactive({
      glue("{format(now(), '%Y_%m_%d')}-",
           "{collapse(data_manager$get_selected_experiments(), sep = '_')}",
           ".pdf")
    }))

}


dataPlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    default_box(
      title = "Data Plot", width = 8,
      div(style = "min-height: 500px;",
          div(id = ns("data_plot_actions"),
              fluidRow(
                column(width = 4,
                       tooltipInput(actionButton, ns("fetch_data"), "Fetch Data", icon = icon("cloud-download"),
                                    tooltip = "Fetch the most recent data from the data base.")) %>% disabled(),
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
                       tooltipInput(actionButton, ns("render_plot"), "Re-plot", icon = icon("refresh"),
                                    tooltip = "Refresh the plot with the selected filters and plot options.") %>% disabled(),
                       spaces(1),
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
                withSpinner(type = 5, proxy.height = "450px")
          )
      )
    ),

    # options box -----
    div(id = ns("options_box"),
        default_box(
          title = "Plot Options", width = 4,
          fluidRow(
            h4("Errors:") %>% column(width = 4),
            checkboxInput(ns("show_errors"), NULL, value = FALSE) %>%
              column(width = 8)
            # h4("Outliers:") %>% column(width = 4),
            # checkboxInput(ns("show_outliers"), NULL, value = TRUE) %>%
            #   column(width = 2)
            ),
          fluidRow(
            h4("Plot height:") %>% column(width = 4),
            numericInput(ns("plot_height"), NULL, value = 500, min = 100, step = 50) %>%
              column(width = 8)),
          fluidRow(
            h4("Time intervals:") %>% column(width = 4),
            numericInput(ns("time_intervals_number"), NULL, value = NA, min = 1, step = 1) %>% column(width = 3),
            selectInput(ns("time_intervals_unit"), NULL, choices = c("default", "mins", "hours", "days"), selected = "default") %>% column(width = 5)
          ),
          fluidRow(
            h4("Legend:") %>% column(width = 4),
            selectInput(ns("legend_position"), NULL, choices = c("right", "bottom", "hide"), selected = "right") %>% column(width = 8)
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
    )


  )

}
