`%then%` <- shiny:::`%OR%`

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
tooltipInput <- function(input, inputId, ..., tooltip = NULL) {
  tagList(
    do.call(input, args = c(list(inputId = inputId), list(...))),
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
