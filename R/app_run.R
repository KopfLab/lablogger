#' Run the user interface
#'
#' This function runs the user interface for the lab logger.
#'
#' @param group_id which group to run for
#' @param access_token access token for particle account
#' @param pool ideally database connection pool, see \link[pool]{dbPool} but can also be a single db connection (not recommended)
#' @param password which password to require for login. If NULL, login will be automatic. (NOTE: maybe manage by data base at some point?)
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' (note: if \code{launch=FALSE}, \code{...} gets ignored)
#' @export
ll_run_gui <- function(group_id, access_token, pool, app_pwd = NULL, timezone = Sys.getenv("TZ"), ..., launch = TRUE) {

  glue("\n\n***************************************************************",
       "\nINFO: Launching lab logger GUI for group '{group_id}' in timezone {timezone}...",
       "{if (default(debug)) {'\nINFO: debug mode ON'} else {''}}") %>%
    message()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = app_ui(group_id = group_id, timezone = timezone),
    server = app_server(group_id = group_id, access_token = access_token, pool = pool, app_pwd = app_pwd, timezone = timezone)
  )

  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}

