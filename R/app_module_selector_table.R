# Selector table

#' Selector table server
#'
#' This generates an rhandson table for selecting items to include in downstream operations.
#'
#' @param id_column name of the ID column - make a rownumber or concatenated column if there is no unique identificer column otherwise
#' @param column_select dplyr select statement to choose displayed columns and headers
#' @param page_lengths page length options, first one will be selected
#' @param initial_page_length initially selected page length, first entry of the page_lengths by default
#' @param dom the available table control elements and their order
#' @family selector table module functions
selectorTableServer <- function(input, output, session, id_column, column_select = everything(), page_lengths = c(5, 10, 20), initial_page_length = page_lengths[1], dom = "fltip") {

  # safety checks
  stopifnot(!missing(id_column))

  # namespace
  ns <- session$ns

  # column selection
  column_select_quo <- rlang::enquo(column_select)

  # reactive values
  values <- reactiveValues(
    table = NULL, # what is available
    selected = c(), # what is selected
    update_selected = 0 # trigger selection update (circumventing circular triggers with user selection)
  )

  # trigger selection updates
  update_selected <- function() values$update_selected <- values$update_selected + 1
  observeEvent(values$update_selected, {
    module_message(ns, "debug", "updating selections in selection table")
    proxy <- DT::dataTableProxy("selection_table")
    DT::selectRows(proxy, which(values$table[[id_column]] %in% values$selected))
  })

  # render table
  output$selection_table <- DT::renderDataTable({
    values$table # trigger with update of the data table (whole re-render required for client-side)
    isolate({
      validate(need(values$table, "None available.")) # trigger if the table changes
      module_message(ns, "debug", "(re-) rendering selection table")
      row_names <- values$table[[id_column]]
      df <- select(values$table, !!column_select_quo) %>% as.data.frame()
      rownames(df) <- row_names
      update_selected()
      df
    })},
    options = list(
      pageLength = initial_page_length,
      lengthMenu = page_lengths,
      searchDelay = 100,
      dom = dom
    ),
    server = FALSE # client side usually faster to use
  )

  # selection
  observe({
    selections <- input$selection_table_rows_selected
    isolate({
      if (!identical(values$selected, selections) && !is.null(values$table) && nrow(values$table) > 0) {
        values$selected <- if (is.null(selections)) c() else values$table[[id_column]][selections]
      }
    })
  })

  observeEvent(input$select_all, {
    # select all that match the current filter
    values$selected <- unique(c(values$selected, values$table[[id_column]][input$selection_table_rows_all]))
    update_selected()
  })

  observeEvent(input$deselect_all, {
    # deselect all
    values$selected <- c()
    update_selected()
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
      if (!identical(selected, values$selected) && (length(selected) > 0 || length(values$selected) > 0)) {
        values$selected <- selected
        update_selected()
      }
    })
  }

  get_selected <- reactive({
    # make sure all returned selected are valid
    if (length(values$selected) == 0) return(c())
    else return(values$selected[values$selected %in% values$table[[id_column]]])
  })

  get_selected_items <- reactive({
    # get the actual table items that are selected
    values$table[values$table[[id_column]] %in% values$selected, ]
  })

  # return functions
  list(
    set_table = set_table,
    set_selected = set_selected,
    get_selected = get_selected,
    get_selected_items = get_selected_items
  )
}


#' Selector table UI
#' @family selector table module functions
selectorTableUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("selection_table"))
}

#' Selector table buttons
#' @family selector table module functions
selectorTableButtons <- function(id) {
  ns <- NS(id)
  tagList(
    tooltipInput(actionButton, ns("select_all"), "Select all",
                 icon = icon("check-square-o"),
                 tooltip = "Select all items that match the current search in addition to those already selected."),
    spaces(1),
    tooltipInput(actionButton, ns("deselect_all"), "Deselect",
                 icon = icon("square-o"),
                 tooltip = "Deselct all items (irrespective of the search).")
  )
}
