#' Start the user interface
#'
#' Run the user interface for the chemostat control center
#'
#' @param db_host data base host
#' @param db_name data base name
#' @param db_user data base user
#' @param db_pwd data base user password
#' @param db_drv data base driver function, default is PostgreSQL
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' (note: if \code{launch=TRUE}, \code{...} gets ignored)
#' @export
run <- function(db_host, db_name, db_user, db_pwd, db_drv = PostgreSQL, ..., launch = TRUE) {

  cat("\n\n***************************************************************",
      "\nINFO: Launching chemostat control center GUI...",
      "\nINFO: Connection to database... ")

  # start data base pool
  pool <- dbPool(drv = db_drv(), host = db_host, dbname = db_name, user = db_user, password = db_pwd)
  onStop(function() { poolClose(pool) })
  cat("successful.\n")

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = app_ui(),
    server = app_server(pool)
  )

  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}


# # Install the user interface
# #
# # Installs the user interface in the target folder with the provided call parameters for launching it via \code{\link[shiny]{runApp}} or as a server application.
# # @param install_dir the installation directory (has to exist)
# # @param ... parameters for the \code{run} function
# install <- function(install_dir, data_dir = ".", ...) {
#   if(!dir.exists(install_dir)) stop("directory does not exist: ", install_dir, call. = FALSE)
#
#   # generate function call
#   dots <- c(list(data_dir = data_dir), list(...))
#   parameters <-
#     lapply(dots, function(i)
#       if(is.numeric(i) || is.logical(i)) str_c("=", i)
#       else if (is.character(i)) str_c(" = \"", i, "\"")
#       else stop("don't know how to process ", class(i))
#     ) %>%
#     { str_c(names(.), unlist(.)) } %>%
#     str_c(collapse = ", ") %>%
#     { if(length(.) > 0) str_c(., ", ") else "" } %>%
#     str_c("launch = FALSE")
#
#   # call
#   sprintf("library(isoviewer)\nrun(%s)", parameters) %>%
#     cat(file = file.path(install_dir, "app.R"))
#   message("Info: installed isoviewer app into directory '", install_dir, "'")
# }

