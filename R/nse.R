# resolve default cols in a list of quos
resolve_defaults <- function(quos) {
  resolve_default <- function(x) if (quo_is_lang(x) && lang_head(x) == sym("default")) eval_tidy(x) else x
  if (is_quosure(quos)) return(resolve_default(quos))
  else map(quos, resolve_default)
}

# Convert quo to text accounting for plain text and symbol quos
quos_to_text <- function(lquos, check_for_validity = TRUE, variable = "variable") {
  single_quo <- is_quosure(lquos)
  lquos <- quos(!!!lquos)
  are_text_quos <- map_lgl(lquos, ~is.character(quo_expr(.x)))
  are_symbol_quos <- map_lgl(lquos, quo_is_symbol)

  # check for validity
  if (check_for_validity && !all(ok <- are_text_quos | are_symbol_quos)) {
    params <-
      str_c(names(lquos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
            map_chr(lquos[!ok], quo_text)) %>%
      glue::glue_collapse("', '", last = "' and '")
    if (sum(!ok) > 1)
      glue("parameters '{params}' do not refer to valid {variable} names") %>% stop(call. = FALSE)
    else
      glue("parameter '{params}' does not refer to a valid {variable} name") %>% stop(call. = FALSE)
  }

  text_quos <-
    map2_chr(lquos, are_text_quos, function(lquo, is_text)
      if(is_text) quo_expr(lquo) else quo_text(lquo)) %>%
    as.list()
  if (single_quo) return(text_quos[[1]])
  else return(text_quos)
}

