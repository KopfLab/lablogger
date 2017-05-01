

header <- dashboardHeader(title = "Chemostat", titleWidth = 150)
sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(
    menuItem("Live View", tabName = "live", icon = icon("signal")),
    menuItem("Traces", tabName = "traces", icon = icon("cog"), selected = TRUE)
  )
)

body <- dashboardBody(

  useShinyjs(),

  tabItems(
    tabItem("live", uiOutput("raspicams")),

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


cstat_ui <- dashboardPage(header, sidebar, body)
