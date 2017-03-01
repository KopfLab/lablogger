

header <- dashboardHeader(title = "Chemostat", titleWidth = 150)
sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(
    menuItem("Live View", tabName = "live", icon = icon("signal")),
    menuItem("Traces", tabName = "traces", icon = icon("cog"), selected = TRUE)
  )
)

body <- dashboardBody(

  tabItems(
    tabItem("live",
      column(5,
             box(title = "Data logger 128.112.20.11", collapsible = FALSE, collapsed = FALSE, width = 12, status = "primary", solidHeader = TRUE,
                 tags$iframe(src="http://128.112.20.11", height = "500px", frameborder="0")
             )),
      column(7,
        box(title = "Camera 128.112.21.16", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success", solidHeader = TRUE,

            # note this doesn't really work because of security proection allowing cross iframe accessing!
            # the refresh must come from within the iframe (boo) but somehow I can't get the message listener
            # in the iframe to actually trigger from outside (this might be because it's a camera stream?)
            #div(align="center",
            #    HTML('<button type="button" class="btn btn-primary btn-sm"
            #        onClick = "iframe = document.getElementById(\'128.112.21.16\'); iframe.contentWindow.postMessage(\'refresh\', \'http://128.112.21.16:8081\');">
            #         Refresh&nbsp;<i class="fa fa-camera"></i></button>')),
            HTML('<iframe src="http://128.112.21.16:8081" id="128.112.21.16" width="640px" height="480px" frameborder="0"
                    onload="window.addEventListener(\'message\', function(event) {
                        alert(\'got it!\');
                    }, false);
                    // postMessage(\'test\', location.origin);">
                 </iframe> ')
            #tags$iframe(src="http://128.112.21.16:8081", id="128.112.21.16", width = "640px", height = "480px", frameborder="0")
        ),
        box(title = "Camera 128.112.20.236", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success", solidHeader = TRUE,
            tags$iframe(src="http://128.112.20.236:8081", width = "640px", height = "480px", frameborder="0")
        ),
        box(title = "Camera 128.112.21.22", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success", solidHeader = TRUE,
            tags$iframe(src="http://128.112.21.22:8081", width = "640px", height = "480px", frameborder="0")
        ))
    ),


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
                   checkboxGroupInput("vessels", "Vessels", c(), inline = TRUE),
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
