# Selector table

#' Selector table server
#'
#' This generates an rhandson table for selecting items to include in downstream operations.
#'
#' @param id_column name of the id column that should be used for reporting selection
#' @param col_headers name of the column headers
#' @param hot_mods hands on table modifiers
#' @family selector table module functions
selectorTableServer <- function(input, output, session, id_column, col_headers, hot_mods = NULL) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    table = NULL, # what is available
    selected = c(), # what is selected
    update_selected = 0 # trigger selection update (circumventing circular triggers with user selection)
  )

  # render table
  output$selection_table = renderRHandsontable({

    req(values$table) # if the table changes
    values$update_selected # if the selection changes
    module_message(ns, "debug", "rendering selection table")

    # isolate hereafter to generate rhandsontable
    isolate({
      table <- values$table
      table$include <- table[[id_column]] %in% values$selected
      hot <- table %>%
        select(include, everything()) %>%
        rhandsontable(colHeaders = c(" ", col_headers)) %>%
        hot_table(readOnly = TRUE, highlightRow = TRUE, columnSorting = FALSE, contextMenu = FALSE) %>%
        hot_col(col = " ", halign = "htCenter", readOnly = FALSE)
      if (!is.null(hot_mods) && is.function(hot_mods))
        hot <- hot_mods(hot)
      return(hot)
    })
  })

  # selection
  observeEvent(input$selection_table, {
    if (!identical(values$selected, input$selection_table) && !is.null(input$selection_table) && !is.null(values$table) && nrow(values$table) > 0) {
      selections <- input$selection_table %>% hot_to_r()
      if (nrow(selections) > 0) {
        values$selected <- selections %>% filter(include) %>% { .[[id_column]] }
      }
    }
  })

  observeEvent(input$select_all, {
    values$selected <- values$table[[id_column]]
    values$update_selected <- values$update_selected + 1
  })

  observeEvent(input$deselect_all, {
    values$selected <- c()
    values$update_selected <- values$update_selected + 1
  })

  # button visibility
  observe({
    toggle("select_all", condition = !is.null(values$table))
    toggle("deselect_all", condition = !is.null(values$table))
  })

  # functions
  set_table <- function(table, initial_selection = c()) {
    isolate({
      if (is.null(table) || is.null(values$table) || !identical(table, values$table)) {
        initial <- is.null(values$table)
        values$table <- table
        if (initial && length(initial_selection) > 0)
          set_selected(initial_selection)
      }
    })
  }

  set_selected <- function(selected) {
    isolate({
      if (!identical(selected, values$selected)) {
        values$selected <- selected
        values$update_selected <- values$update_selected + 1
      }
    })
  }

  get_selected <- reactive({
    # make sure all returned selected are valid
    if (length(values$selected) == 0) return(c())
    else return(values$selected[values$selected %in% values$table[[id_column]]])
  })

  # return functions
  list(
    set_table = set_table,
    set_selected = set_selected,
    get_selected = get_selected
  )
}


#' Selector table UI
#' @inheritParams isofilesLoadUI
#' @family selector table module functions
selectorTableUI <- function(id, height = "200px") {
  ns <- NS(id)
  rHandsontableOutput(ns("selection_table"), width = "100%", height = height) %>%
    withSpinner(type = 7, proxy.height = height)
}

#' Selector table buttons
#' @inheritParams isofilesLoadUI
#' @family selector table module functions
selectorTableButtons <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("select_all"), "Select all", icon = icon("check-square-o")),
    spaces(1),
    actionButton(ns("deselect_all"), "Deselect", icon = icon("square-o"))
  )
}
