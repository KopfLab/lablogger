
generate_data_screen <- function(selector_name, selector_height = 100) {
  tagList(


    # DATA BOX ====
    default_box(
      title = "Data", width = 12,

      div(style = "min-height: 500px;",
          div(id = "data_plot_messages", textOutput("data_plot_message")),
          # div(align = "right", id = "data_plot_actions",
          #     # tooltipInput(actionButton, "render_data_plot", "Fetch Data", icon = icon("refresh"),
          #     #              tooltip = "Fetch the most recent data for the selected experiment(s) from the data base."),
          #     spaces(1)
          #     #plotDownloadLink(ns("plot_download")) # FIXME
          # ) %>% hidden(),
          div(id = "data_plot_div",
              plotOutput("data_plot", height = "100%") %>% withSpinner(type = 5, proxy.height = "450px")
          )
      )


      # fluidRow(
      #   h4("Scale signals:") %>% column(width = 4)),
      # # fluidRow(
      # #   h4("Scale time:") %>% column(width = 4),
      # #   selectInput(ns("scale_time"), NULL, choices = time_options, selected = "seconds") %>% column(width = 8)),
      # # selectorTableUI(ns("selector"), height = selector_height),
      # footer = div(
      #   #style = "height: 35px;",
      #   #selectorTableButtons(ns("selector")),
      #   spaces(1),
      #   tooltipInput(actionButton, "selector_refresh", label = "Plot", icon = icon("refresh"),
      #                tooltip = "Refresh plot with new selections.")
      # )
    )



  )
}
