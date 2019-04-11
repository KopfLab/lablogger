#' Read data logs
#'
#' Read in an .rds, .csv or .xlsx file with device data logs downloaded from the GUI.
#'
#' @param file_path path to the .rds or .xlsx file
#' @return device data logs data frame
#' @family data logs functions
#' @export
ll_read_device_data_logs_from_file <- function(file_path, quiet = default(quiet)) {

  # safety checks
  if (missing(file_path) || length(file_path) != 1)
    stop("please provide a single file path to an .rds, .csv OR .xlsx file", call. = FALSE)
  if (!file.exists(file_path))
    stop("file path does not exist: ", file_path, call. = FALSE)

  # file info
  supported <- c("rds", "xlsx", "csv")
  ext <- stringr::str_match(file_path, "\\.(\\w+)$")[,2]

  if (ext == "rds") {
    df <- readr::read_rds(file_path)
  } else if (ext == "xlsx") {
    df <- readxl::read_excel(file_path)
  } else if (ext == "csv") {
    df <- readr::read_csv(file_path)
  } else {
    glue::glue(
      "unknown file extension: {ext}",
      " (supported: {paste0(supported, collapse = ', ')})") %>%
      stop(call. = FALSE)
  }

  # info message
  if (!quiet) {
    glue::glue("Info: read {nrow(df)} device data log entries from .{ext} file") %>% message()
  }

  return(df)
}
