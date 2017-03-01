
#' Start the user interface
#'
#' To run this function stand-alone as a script from any folder on your system, copy the
#' run (Unix systems) or run.bat (Windows) systems from the GitHub repository
#' (\link{http://www.github.com/KopfLab/labwareC3/inst/scripts/}
#' and modify depending on your R executable.
#'
#' @param base_dir the directory for local data files
#' @param ... passed on to the \link{shiny::runApp} call and could include parameters such as host or port
#' @export
run_gui <- function(base_dir = ".", ...) {

  app_dir <- system.file("gui", package = "labwareC3")
  if (app_dir == "")
    stop("Could not find GUI directory. Try re-installing `labwareC3`.", call. = FALSE)

  labwareC3:::run_gui_dev(base_dir = base_dir, app_dir = app_dir, ...)
}


#' for GUI development
#' not exported to namespace but helpful for active development of the shiny app
run_gui_dev <- function(base_dir = ".", app_dir = file.path("inst", "gui"), ...) {

  if (!file.exists(app_dir))
    stop("App directory does not exist, something might be wrong with the `labwareC3` installation: ", app_dir)

  if (!file.exists(base_dir))
    stop("Could not find base directory '", base_dir, " from the current working directory: ", getwd())

  # store base_dir for shiny
  if (R.utils::isAbsolutePath(base_dir))
    .GlobalEnv$.base_dir <- base_dir
  else
    .GlobalEnv$.base_dir <- R.utils::filePath(getwd(), base_dir)
  on.exit(rm(.base_dir, envir=.GlobalEnv))

  # start app
  shiny::runApp(app_dir, display.mode = "normal", ...)
}

