# general data retrieval functions
validate_db_connection <- function(con_quo) {
  if(!is_quosure(con_quo)) stop("connection not supplied as quosure", call. = FALSE)
  con <- resolve_defaults(con_quo)
  while (is_quosure(con)) con <- eval_tidy(con)
  return(con)
}

# convert data to sql compatible
to_sql <- function(...) {
  values <- list(...)
  if (length(values) == 1 && is.list(values[1])) values <- values[[1]]
  values %>%
    map_chr(
      ~
        if (is.null(.x) || is.na(.x)) {
          "NULL"
        } else if (is.character(.x)) {
          sprintf("'%s'", str_replace(.x, fixed("'"), "''"))
        } else if (is.numeric(.x)) {
          as.character(.x)
        } else if (is.logical(.x)) {
          if (.x) 'true' else 'false'
        } else {
          stop(glue("unsupported value type: {class(.x)[1]}"), call. = FALSE)
        }) %>%
    glue::glue_collapse(", ")
}

# convert whole df to sql compatible list of values
df_to_sql <- function(df) {
  df %>%
    ungroup() %>%
    mutate(rowid = dplyr::row_number()) %>%
    nest(-rowid) %>%
    mutate(sql = map_chr(data, ~to_sql(as.list(.x)))) %>%
    { str_c("(", glue::glue_collapse(.$sql, sep = "), ("), ")") }
}

# make insert statement from data frame
df_to_insert_sql <- function(df, table) {
  glue("INSERT INTO {table} ({glue::glue_collapse(names(df), sep = ', ')}) VALUES {df_to_sql(df)}")
}

# run sql with error catching
run_sql <- function(sql, con) {
  con <- validate_db_connection(enquo(con))
  tryCatch(
    result <- dbExecute(con, as.character(sql)),
    error = function(e) {
      glue("SQL statement failed ('{sql}') with message:\n{e$message}") %>% stop(call. = FALSE)
    })
  return(result)
}

# run insert sql
run_insert_sql <- function(df, table, con, quiet) {
  result <-
    df %>%
    df_to_insert_sql(table) %>%
    run_sql(con)

  if (!quiet) glue("{result} new record(s) created.") %>% message()
  return(result)
}
