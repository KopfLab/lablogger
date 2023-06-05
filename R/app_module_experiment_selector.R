# FIXME: replace data_manager with get_experiments, refresh_experiments, etc. functions
# this is the multiple experiment selector
# single experiment selector is implemented directly in app_module_experiment_overview
experimentSelectorServer <- function(input, output, session, data_manager) {

  # namespace
  ns <- session$ns

  # selector
  selector <- callModule(
    selectorTableServer, "selector",
    id_column = "exp_id",
    column_select = c(Description = exp_desc, Recording = recording)
  )

  # update data
  observe({
    req(df <- data_manager$get_experiments())
    isolate({
      if (nrow(df) > 0) {
        df <- select(df, exp_id, exp_desc, recording) %>%
          mutate(recording = ifelse(recording, "yes", "no"))
        selector$set_table(df)
      }
    })
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


experimentSelectorUI <- function(id, width = 12) {
  ns <- NS(id)
  default_box(
    title = "Experiments", width = width,
    selectorTableUI(ns("selector")),
    footer = div(
      tooltipInput(actionButton, ns("experiment_refresh"), label = "Refresh", icon = icon("sync"), tooltip = "Refresh experiments."),
      spaces(1),
      selectorTableButtons(ns("selector"))
    )
  )
}
