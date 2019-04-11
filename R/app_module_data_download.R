#' Data Download Server
#' @param data_func reactive function providing the data
#' @param filename_func reactive function returning the default file name
dataDownloadServer <- function(input, output, session, data_func, filename_func) {

  # namespace
  ns <- session$ns

  # save dialog
  save_dialog <- reactive({
    modalDialog(
      title = "Save data", fade = FALSE, easyClose = TRUE, size = "s",
      textInput(ns("save_name"), "Filename:", filename_func()),
      checkboxGroupInput(ns("format"), "Formats:",
                         c("R Data Storage (.rds)" = ".rds",
                           "Excel (.xlsx)" = ".xlsx",
                           "Comma Separated Values (.csv)" = ".csv")),
      footer =
        tagList(
          downloadButton(ns("download"), label = "Download", icon = icon("download")),
          modalButton("Close")
        )
    )})
  observeEvent(input$download_dialog, showModal(save_dialog()))
  observe(shinyjs::toggleState("download", length(input$format) > 0))

  # download handler
  output$download <- downloadHandler(
    filename = function() { isolate(stringr::str_replace(input$save_name, "(\\.zip)?$", ".zip")) },
    content = function(filename) {
      module_message(ns, "debug", "saving data ", input$save_name, " (formats ", paste(input$format, collapse = ", "), ")")
      file_paths <- isolate(paste0(stringr::str_replace(input$save_name, "\\.zip$", ""), input$format))
      ll_write_device_data_logs_to_file(device_data_logs = data_func(), file_path = file_paths, zip = filename)
    })

}


#' Data Download Link
#' @param label Label for the download link
dataDownloadLink <- function(id, label = "Save", tooltip = "Save the data in various formats") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("download_dialog"), label, icon = icon("save"),
               tooltip = tooltip)
}
