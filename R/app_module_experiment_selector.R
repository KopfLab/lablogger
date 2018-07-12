
experimentSelectorServer <- function(input, output, session, data_manager) {

  # namespace
  ns <- session$ns

  # selector
  selector <- callModule(selectorTableServer, "selector", id_column = "exp_id", col_headers = c("ID", "Name", "Recording"))

  # update data
  observe({
    req(df <- data_manager$get_experiments())
    if (nrow(df) > 0) {
      df <- select(df, exp_id, exp_name, recording)
      selector$set_table(df)
    }
  })

  # update selected
  observe({
    selected_exps <- data_manager$get_selected_experiments()
    selector$set_selected(selected_exps)
  })

  # trigger refresh
  observeEvent(input$experiment_refresh, data_manager$refresh_experiments())

  # trigger select
  observe(data_manager$select_experiments(selector$get_selected()))

}


experimentSelectorUI <- function(id, width = 12, selector_height = 100) {
  ns <- NS(id)
  default_box(
    title = "Experiments", width = width,
    selectorTableUI(ns("selector"), height = selector_height),
    footer = div(
      tooltipInput(actionButton, ns("experiment_refresh"), label = "Refresh", icon = icon("refresh"), tooltip = "Refresh experiments."),
      spaces(1),
      selectorTableButtons(ns("selector"))
    )
  )
}
