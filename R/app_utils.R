`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# display module message
# @param type if this is an info meessage or debug (debug only shows if in debug mode)
module_message <- function(ns, type = c("info", "debug"), ...) {
  if (type == "debug" && !default(debug)) return()
  prefix <- if(type == "info") "INFO: " else if (type == "debug") "DEBUG: " else stop("don't know message type", type)
  prefix <- paste(as.character(Sys.time()), prefix)
  cat(file=stderr(), prefix, ..., " (NS: ", ns(NULL),")\n", sep = "")
}

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}

# convenience function for adding input with tooltip with default parameters
tooltipInput <- function(input, inputId, ..., tooltip = NULL, hidden = FALSE, disabled = FALSE) {
  input_tag <- do.call(input, args = c(list(inputId = inputId), list(...)))
  if (hidden) input_tag <- shinyjs::hidden(input_tag)
  if (disabled) input_tag <- shinyjs::disabled(input_tag)
  tagList(
    input_tag,
    if (!is.null(tooltip)) bsTooltip(inputId, tooltip)
  )
}

# convenience function for adding output with tooltip with default parameters
tooltipOutput <- function(input, outputId, ..., tooltip = NULL) {
  tagList(
    do.call(input, args = c(list(outputId = outputId), list(...))),
    if (!is.null(tooltip)) bsTooltip(outputId, tooltip)
  )
}

# default box
default_box <- function(..., status = "info", solidHeader = TRUE, collapsible = TRUE) {
  box(..., status = status, solidHeader = solidHeader, collapsible = collapsible)
}

# success modal
success_modal <- function(..., title = "Success", show = TRUE) {
  modal <- modalDialog(
    title = h3(title, align = "center"),
    fade = FALSE, easyClose = TRUE, size = "m",
    span(...),
    footer = modalButton("Close")
  )
  if (show) showModal(modal)
  else return(modal)
}

# error modal
error_modal <- function(..., title = "A problem occurred", show = TRUE) {
  modal <- modalDialog(
    title = h3(title, align = "center", style = "color: red;"),
    fade = FALSE, easyClose = TRUE, size = "m",
    span(..., style = "color: red;"),
    footer = modalButton("Close")
  )
  if (show) showModal(modal)
  else return(modal)
}
