initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    c3.quiet = FALSE,
    c3.tz = Sys.getenv("TZ"),
    c3.access_token = "",
    c3.con = NULL,
    c3.request_timeout = 3
  )
  options(default_options)
}

.onLoad <- function(libname, pkgname) {
  initialize_options()
  invisible()
}
