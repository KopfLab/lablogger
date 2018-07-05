#' Start the user interface
#'
#' Run the user interface for the chemostat control center
#'
#' @param group_id which group to run for
#' @param access_token access token for particle account
#' @param pool the database pool
#' @param password which password to require for login. If NULL, login will be automatic. (NOTE: maybe manage by data base at some point?)
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' (note: if \code{launch=TRUE}, \code{...} gets ignored)
#' @export
run <- function(group_id, access_token, pool, app_pwd = NULL, ..., launch = TRUE) {

  glue("\n\n***************************************************************",
       "\nINFO: Launching chemostat control center GUI for group '{group_id}'...") %>%
    message()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = app_ui(),
    server = app_server(group_id = group_id, access_token = access_token, pool = pool, app_pwd = app_pwd)
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

