#' Chemostat Control Center UI
#'
#' @description Generates the user interface part of the isoviewer app
app_ui <- function(start_menu = "traces") {


  header <- dashboardHeader(title = "Chemostat", titleWidth = 150)
  sidebar <- dashboardSidebar(
    width = 150,
    sidebarMenu(
      "live" %>% menuItem("Live View", tabName = ., icon = icon("signal"), selected = start_menu == .),
      "traces" %>% menuItem("Traces", tabName = ., icon = icon("cog"), selected = start_menu == .)
    )
  )

  body <- dashboardBody(

    useShinyjs(),

    tabItems(
      # live view =====
      tabItem(
        "live",
        h2(
          actionLink("refresh_cams", "Reload cameras", icon = icon("gear")),
          bsTooltip("refresh_cams", "Reload the cameras"),
          align = "center"
        ),
        uiOutput("raspicams") %>% withSpinner(type = 5, proxy.height = "480px")
      ),

      tabItem("traces",

              fluidRow(

                # plot box
                box(
                  title = textOutput("last_gs_update"),
                  h4(align = "right", id = "dl_actions",
                     #actionLink("refresh", "Refresh plot", icon = icon("gear")),
                     #bsTooltip("refresh", "Refresh the plot"),
                     downloadLink("dl_plot", class = NULL, icon("line-chart"), "Save"), " | ",
                     bsTooltip("dl_plot", "Save the plot as a PDF"),
                     downloadLink("dl_excel", class = NULL, icon("file-excel-o"), "Excel"), " | ",
                     bsTooltip("dl_excel", "Download data as Excel file"),
                     downloadLink("dl_data", class = NULL, icon("save"), "Data"),
                     bsTooltip("dl_data", "Download data RData file")
                  ) %>% hidden(),
                  status = "primary", solidHeader = TRUE, width = 9,
                  #plotOutput("main_plot", height = "100%"),
                  plotlyOutput("main_plot", height = "600px"),
                  br()
                ),

                column(width = 3,
                       # plot settings box
                       box(title = "Plot settings", solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = FALSE, width = 12,
                           checkboxGroupInput("vessels", "Experiments", c(), inline = TRUE),
                           checkboxGroupInput("traces", "Traces", c(), inline = FALSE),

                           # div( style = "",
                           #      div(style="display:inline-block;",tags$label("Height:")),
                           #      div(style="display:inline-block; width: 60px;",
                           #          tags$input(id = "main_plot_height", type = "number", value = 400, min = 100, step = 50, class = "form-control")),
                           #      div(style="display:inline-block;",tags$label("px"))
                           # ),
                           #
                           # radioButtons("legend", "Legend Position", c("Right" = "right", "Below" = "below"), selected = "right", inline = TRUE),
                           dateRangeInput("date_range", label = NULL,
                                          format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                          language = "en", separator = " to "),

                           radioButtons("time_format", "Time format", choices = c("Date&Time"="datetime", "Duration"="time.hrs"), selected = "datetime", inline = TRUE)
                       )
                )

              )
      )

    )
  )


  # assemble dashboard page
  dashboardPage(header, sidebar, body)


}
