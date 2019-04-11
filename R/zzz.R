initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    ll.quiet = FALSE,
    ll.access_token = "",
    ll.con = NULL,
    ll.request_timeout = 3,
    ll.debug = FALSE
  )
  options(default_options)
}

.onLoad <- function(libname, pkgname) {
  initialize_options()
  invisible()
}
