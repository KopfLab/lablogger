#' @keywords internal
"_PACKAGE"

#' @importFrom rlang quo quos enquo !! !!! is_quosure quo_squash quo_text quo_is_null quo_is_symbol quo_is_call sym syms eval_tidy parse_expr
#' @importFrom curl new_handle curl_fetch_memory handle_setopt
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom glue glue
#' @importFrom tibble deframe tibble
#' @importFrom dplyr select rename filter as_data_frame %>% tbl collect mutate data_frame everything starts_with ends_with left_join full_join semi_join arrange group_by ungroup do summarize bind_rows between desc case_when
#' @importFrom tidyr gather nest unnest spread
#' @importFrom broom tidy glance
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon theme_bw theme_void facet_grid scale_x_datetime labs annotate %+% ggsave theme element_text
#' @importFrom lubridate with_tz now ymd_hms as.duration as_datetime
#' @importFrom purrr map map_lgl map_chr map_dbl map_int map2 map2_chr walk walk2 map2_int
#' @importFrom stringr str_c str_replace fixed str_interp str_to_lower str_detect str_extract
#' @importFrom readr write_rds read_rds
#' @importFrom DBI dbExecute
#' @importFrom pool dbPool
#' @importFrom RPostgreSQL PostgreSQL
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs hidden show hide toggle disabled toggleState
#' @importFrom shinyAce aceEditor
#' @importFrom shinyBS bsTooltip
#' @importFrom shinycssloaders withSpinner
#' @importFrom stats setNames cor.test lm
#' @importFrom utils packageVersion
#' @importFrom methods is
NULL
