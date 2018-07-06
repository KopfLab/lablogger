#' Chemostat Control Center UI
#'
#' @description Generates the user interface part of the isoviewer app
app_ui <- function(title = "Labware CC") {

  color <- "red" # see ?dashboardPage for options
  #box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = color)

  dashboardPage(
    # SKIN ----
    skin = color,

    # HEADER ----
    dashboardHeader(title = title, titleWidth = 150),

    # SIDEBAR ---
    dashboardSidebar(

      width = 150,
      sidebarMenu(
        id = "menu",
        "login" %>% menuItem("Login", tabName = ., icon = icon("log-in", lib = "glyphicon"), selected = TRUE),
        "data" %>% menuItem("Data", tabName = ., icon = icon("line-chart")),
        "logs" %>% menuItem("Logs", tabName = ., icon = icon("database")),
        "experiments" %>% menuItem("Experiments", tabName = ., icon = icon("flask")),
        "devices" %>% menuItem("Devices", tabName = ., icon = icon("cogs")),
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
            tabItem(
              "login",
              div(id = "login-panel",
                  column(width = 12,
                         textInput("password", NULL, placeholder = "Please enter your password."),
                         selectInput("auto_login_trigger", NULL, choices = "1", selected = "1") %>% hidden(),
                         actionButton("login", "Login")
                  )),

              div(id = "welcome-panel", column(width = 12, h2("Welcome to ", title, ". You have been succesfully logged on."))) %>% hidden()
            ),

            # all other tabs ====
            tabItem("data", div(id = "data-panel", column(width = 12, uiOutput("data") %>% withSpinner(type = 5, proxy.height = "450px")))),
            tabItem("logs", div(id = "log-panel", column(width = 12, uiOutput("logs")))),
            tabItem("experiments", div(id = "experiments-panel", column(width = 12, uiOutput("experiments")))),
            tabItem("devices", div(id = "devices-panel", column(width = 12, uiOutput("devices")))),
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
