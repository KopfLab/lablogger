#' Lab Logger UI
#'
#' Generates the user interface part of the isoviewer app
#'
#' @param app_title the title of the application
#' @param app_color the dashboard color, see \link[shinydashboard]{dashboardPage} skin for available options
app_ui <- function(app_title = "Lab Logger", app_color = "red", timezone = NULL) {

  #box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = app_color)

  dashboardPage(
    # SKIN ----
    skin = app_color,

    # HEADER ----
    dashboardHeader(title = app_title, titleWidth = 150),

    # SIDEBAR ---
    dashboardSidebar(

      width = 150,
      sidebarMenu(
        id = "menu",
        h5("Lab Logger", as.character(packageVersion("lablogger")), align = "center"),
        if (!is.null(timezone)) h5(timezone, align = "center"),
        "login" %>% menuItem("Login", tabName = ., icon = icon("log-in", lib = "glyphicon"), selected = TRUE),
        "data" %>% menuItem("Data", tabName = ., icon = icon("line-chart")),
        "devices" %>% menuItem("Devices", tabName = ., icon = icon("cogs")),
        "experiments" %>% menuItem("Experiments", tabName = ., icon = icon("flask")),
        "live" %>% menuItem("Webcams", tabName = ., icon = icon("camera"))
      ),

      # HEADER ----
      tags$head(
        tags$style(
          type="text/css",
          HTML(str_c(
            # error validation output
            #".shiny-output-error-validation { color: red; font-size: 16px; }", # do we want this red?
            ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
            # adjust sidebar height
            #".sidebar {height: 2000px}", # FIXME: make this dynamically long enough
            # body top padding
            ".box-body {padding-top: 5px; padding-bottom: 0px}",
            # pads on shiny items
            ".form-group, .selectize-control {margin-bottom: 0px;}",
            # custom background box
            str_interp(".box.box-solid.box-info>.box-header{color:#fff; background: ${col}; background-color: ${col};}", list(col = box_default)),
            str_interp(".box.box-solid.box-info{border:1px solid ${col};}", list(col = box_default)),
            sep="\n"))
        )
      ),

      # USE SHINY JS AND EXTENSIONS ---
      useShinyjs()

    ),

    # BODY ====
    dashboardBody(

      div(class = "row",
          tabItems(
            # login ====
            tabItem("login", loginUI("login", title = app_title)),

            # all other tabs ====
            tabItem("data", div(id = "data-panel", column(width = 12, uiOutput("data") %>% withSpinner(type = 5, proxy.height = "450px")))),
            tabItem("devices", div(id = "devices-panel", column(width = 12, uiOutput("devices")))),
            tabItem("experiments", div(id = "experiments-panel", column(width = 12, uiOutput("experiments")))),
            tabItem("live", div(id = "live-panel", column(width = 12, uiOutput("live"))))
            ## old live
            # h2(
            #   actionLink("refresh_cams", "Reload cameras", icon = icon("gear")),
            #   bsTooltip("refresh_cams", "Reload the cameras"),
            #   align = "center"
            # ),
            # uiOutput("raspicams") %>% withSpinner(type = 5, proxy.height = "480px")

          )
      )
    )
  )


  #             fluidRow(
  #
  #               # plot box
  #               box(
  #                 title = textOutput("last_gs_update"),
  #                 h4(align = "right", id = "dl_actions",
  #                    #actionLink("refresh", "Refresh plot", icon = icon("gear")),
  #                    #bsTooltip("refresh", "Refresh the plot"),
  #                    downloadLink("dl_plot", class = NULL, icon("line-chart"), "Save"), " | ",
  #                    bsTooltip("dl_plot", "Save the plot as a PDF"),
  #                    downloadLink("dl_excel", class = NULL, icon("file-excel-o"), "Excel"), " | ",
  #                    bsTooltip("dl_excel", "Download data as Excel file"),
  #                    downloadLink("dl_data", class = NULL, icon("save"), "Data"),
  #                    bsTooltip("dl_data", "Download data RData file")
  #                 ) %>% hidden(),
  #                 status = "primary", solidHeader = TRUE, width = 9,
  #                 #plotOutput("main_plot", height = "100%"),
  #                 plotlyOutput("main_plot", height = "600px"),
  #                 br()
  #               ),
  #
  #               column(width = 3,
  #                      # plot settings box
  #                      box(title = "Plot settings", solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = FALSE, width = 12,
  #                          checkboxGroupInput("vessels", "Experiments", c(), inline = TRUE),
  #                          checkboxGroupInput("traces", "Traces", c(), inline = FALSE),
  #
  #                          # div( style = "",
  #                          #      div(style="display:inline-block;",tags$label("Height:")),
  #                          #      div(style="display:inline-block; width: 60px;",
  #                          #          tags$input(id = "main_plot_height", type = "number", value = 400, min = 100, step = 50, class = "form-control")),
  #                          #      div(style="display:inline-block;",tags$label("px"))
  #                          # ),
  #                          #
  #                          # radioButtons("legend", "Legend Position", c("Right" = "right", "Below" = "below"), selected = "right", inline = TRUE),
  #                          dateRangeInput("date_range", label = NULL,
  #                                         format = "yyyy-mm-dd", startview = "month", weekstart = 0,
  #                                         language = "en", separator = " to "),
  #
  #                          radioButtons("time_format", "Time format", choices = c("Date&Time"="datetime", "Duration"="time.hrs"), selected = "datetime", inline = TRUE)
  #                      )
  #               )
  #
  #             )
  #     )
  #
  #   )
  # )

}
