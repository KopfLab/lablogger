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
        h5(a("Lab Logger", href = "https://github.com/KopfLab/lablogger", target = "_blank"),
           as.character(packageVersion("lablogger")), align = "center"),
        if (!is.null(timezone)) h5(timezone, align = "center"),
        "login" %>% menuItem("Login", tabName = ., icon = icon("log-in", lib = "glyphicon"), selected = TRUE),
        "experiments" %>% menuItem("Experiments", tabName = ., icon = icon("flask")),
        "devices" %>% menuItem("Devices", tabName = ., icon = icon("cogs")),
        "data" %>% menuItem("All Data", tabName = ., icon = icon("line-chart")),
        "live" %>% menuItem("Webcams", tabName = ., icon = icon("camera")),
        tags$li(a(uiOutput("help", inline = TRUE)))
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

}
