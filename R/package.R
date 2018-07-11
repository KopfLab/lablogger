#' @keywords internal
"_PACKAGE"

#' @importFrom rlang quo quos enquo !! !!! is_quosure quo_expr quo_text quo_is_null quo_is_symbol quo_is_lang lang_head sym eval_tidy
#' @importFrom curl new_handle curl_fetch_memory
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom glue glue collapse
#' @importFrom dplyr select rename filter as_data_frame %>% tbl collect mutate data_frame everything starts_with ends_with left_join arrange
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw theme_void facet_grid scale_x_datetime labs annotate
#' @importFrom lubridate with_tz force_tz now ymd_hms as.duration
#' @importFrom purrr map map_lgl map_chr map_dbl map2 map2_chr walk
#' @importFrom stringr str_c str_replace fixed str_interp
#' @importFrom readr write_rds read_rds
#' @importFrom DBI dbExecute
#' @importFrom pool dbPool
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom tidyr gather
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs hidden show hide toggle
#' @importFrom shinyAce aceEditor
#' @importFrom shinyBS bsTooltip
#' @importFrom shinycssloaders withSpinner
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable rhandsontable hot_table hot_col hot_to_r
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom methods is
NULL

#' install the latest version of labwareC3 from GitHub
#' @param ref which version to install, master (=newest) is the default
#' @export
update_package <- function(ref = "master") {
  on.exit({
    remove.packages("labwareC3")
    devtools::install_github("kopflab/labwareC3", ref = ref)
    message("\nInstallation complete: labwareC3 version ", packageVersion("labwareC3"), "\n")
  })
}

#' authenticate for google spreadsheets and store token in target directory (allows GUI to access the spreadsheet data)
#' @param data_dir the data directory where the GUI is intended to run
#' @export
generate_gs_token <- function(data_dir) {
  if(missing(data_dir)) stop("Requires a target data directory where to save the token", call. = F)
  token <- gs_auth(new_user = TRUE)
  saveRDS(token, file.path(data_dir, "gs_token.rds"))
}
