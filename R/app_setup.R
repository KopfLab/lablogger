#' Setup the user interface app
#'
#' Sets up the necessary files for launching the user interface in the target folder. Keeps sensitive authentication information separate from the app launch script and allows for easy running of the gui via \code{\link[shiny]{runApp}} or deployment as a server application (e.g. on shinyapps.io). IMPORTANT: never add the created \code{credentials.R} file to any publically accessible shared folder or github repository - the database and particle account information you have stored there could be compromised!
#' @param dir the directory where to create the launch scripts (absolute path or relative to your current working directory)
#' @param overwrite whether to overwrite existing launch files (default is FALSE)
#' @export
ll_setup_gui <- function(dir, overwrite = FALSE) {

  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  glue::glue("Info: setting up lablogger launch files in folder '{dir}'...") %>% message()

  # copy app.R template file
  message(" - creating 'app.R' launch script... ", appendLF = FALSE)
  file_exists <- file.exists(file.path(dir, "app.R"))
  if (!file_exists || overwrite) {
    file.copy(
      from = system.file(package = "lablogger", "templates", "app.R"),
      to = file.path(dir, "app.R"),
      overwrite = TRUE
    )
  }
  glue::glue(
    if (file_exists && overwrite) "existing file overwritten."
    else if (file_exists) "failed (file already exists, use overwrite = TRUE)"
    else "complete"
  ) %>% message()

  # copy credentials.R template file
  message(" - creating 'credentials.R' authentication file... ", appendLF = FALSE)
  file_exists <- file.exists(file.path(dir, "credentials.R"))
  if (!file_exists || overwrite) {
    file.copy(
      from = system.file(package = "lablogger", "templates", "credentials.R"),
      to = file.path(dir, "credentials.R"),
      overwrite = TRUE
    )
  }
  glue::glue(
    if (file_exists && overwrite) "existing file overwritten."
    else if (file_exists) "failed (file already exists, use overwrite = TRUE)"
    else "complete"
  ) %>% message()

  # add to gitignore
  message(" - adding authentication file to .gitignore... ", appendLF = FALSE)
  cat("# exclude credentials\ncredentials.R\n", file = file.path(dir, ".gitignore"), append = TRUE)
  message("complete.")
}
