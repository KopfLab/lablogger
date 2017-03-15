

header <- dashboardHeader(title = "Chemostat", titleWidth = 150)
sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(
    menuItem("Live View", tabName = "live", icon = icon("signal")),
    menuItem("Traces", tabName = "traces", icon = icon("cog"), selected = TRUE)
  )
)

body <- dashboardBody(

  tabItems(
    tabItem("live", uiOutput("raspicams")),

    tabItem("traces",

      fluidRow(

        # plot box
        box(
          title = textOutput("last_gs_update"),
          div(align = "right",
              #actionLink("refresh", "Refresh plot", icon = icon("gear")),
              #bsTooltip("refresh", "Refresh the plot"),
              downloadButton("save", "Save", icon("download")),
              bsTooltip("save", "Save the plot as a PDF")
          ),
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
                                  language = "en", separator = " to ")
               )


        )

      )
    )

  )
)


cstat_ui <- dashboardPage(header, sidebar, body)
