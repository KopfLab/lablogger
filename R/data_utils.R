# general utils =====

# function to reliably turn a list of lists into a data frame
# @param lists_df a data frame with a column that has lists in each row
# @param column the name of the column that has the list
# @param unnest_single_values whether to unnest single values (values that have a none or only a single entry for all retrieve records)
# @param unpack_sub_lists - whether to unpack remaining list columns (only evaluated if unnest_single_values = TRUE)
# @param nest_into_data_frame - whether to nest the unpacked lists into a data frame or keep the columns all unpacked
unpack_lists_tibble <- function(lists_df, column = lists, unnest_single_values = TRUE, unpack_sub_lists = FALSE, nest_into_data_frame = FALSE) {

  # id rows
  col_quo <- enquo(column)
  lists_df_original <- mutate(lists_df, ..nr.. = dplyr::row_number())

  # convert lists into data frame format
  lists_df <-
    lists_df_original %>%
    rename(lists = !!col_quo) %>%
    mutate(name = map(lists, ~if(length(.x) == 0) { NA_character_ } else { names(.x) })) %>%
    unnest(name) %>%
    mutate(
      class = map2_chr(lists, name, ~class(.x[[.y]])[1]),
      length = map2_int(lists, name, ~length(.x[[.y]])),
      value = map2(lists, name, ~.x[[.y]]),
      name = str_to_lower(name)
    )

  # data classes
  data_classes <-
    lists_df %>%
    filter(name != "NA") %>%
    mutate(..order.. = dplyr::row_number()) %>%
    group_by(..order.., name) %>%
    summarize(
      data_class = unique(class)[1],
      value_max_n = as.integer(max(length)),
      .groups = "drop"
    ) %>%
    ungroup() %>% arrange(..order..) %>% select(-..order..)

  # lists wide
  lists_df_wide <- lists_df %>%
    select(..nr.., name, value) %>%
    spread(name, value)

  # deal with NAs
  if ("<NA>" %in% names(lists_df_wide))
    lists_df_wide <- select(lists_df_wide, -`<NA>`)

  # remove columns that hold no data at all
  null_cols <- filter(data_classes, value_max_n == 0 | data_class == "NULL")$name
  data_classes <- filter(data_classes, !(value_max_n == 0 | data_class == "NULL"))
  lists_df_wide <- lists_df_wide[!names(lists_df_wide) %in% null_cols]

  # fill NULL values with NA to not loose records during unnesting (except for lists)
  if (nrow(data_classes) > 0) {
    for (i in 1:nrow(data_classes)) {
      lists_df_wide <-
        with(data_classes[i,], {
          # make sure the function exists
          if (exists(data_class)) {
            if (data_class %in% c("character", "integer", "numeric", "logical"))
              default_value <- do.call(str_c("as.", data_class), args = list(NA))
            else
              default_value <- do.call(data_class, args=list())
            # note, could also do this with a right_join back in (but perhaps slower?)
            mutate(lists_df_wide,
                   !!name := map(!!sym(name), ~if (is.null(.x) || length(.x) == 0) { default_value } else { .x }))
          } else {
            # don't do anything if it's not a standard class
            lists_df_wide
          }
        })
    }
  }

  # unnest all the ones that have only single value
  if (unnest_single_values) {
    unnest_cols <- filter(data_classes, value_max_n == 1,
                          data_class %in% c("character", "integer", "numeric", "logical"))$name
    lists_df_wide <- unnest(lists_df_wide, dplyr::all_of(unnest_cols))
  }

  # unpack sub lists
  if (unpack_sub_lists) {
    unpack_cols <- filter(data_classes, data_class == "list")$name
    for (col in unpack_cols) {
      new_data <<- lists_df_wide %>% rename(..parent_nr.. = ..nr..) %>%
        unpack_lists_tibble(
          column = !!sym(col), unnest_single_values = unnest_single_values,
          # don't allow recursive unpacking for now, always nest into data frame
          unpack_sub_lists = FALSE, nest_into_data_frame = TRUE)

      lists_df_wide <-
        lists_df_wide %>% rename(..parent_nr.. = ..nr..) %>%
        unpack_lists_tibble(
          column = !!sym(col), unnest_single_values = unnest_single_values,
          # don't allow recursive unpacking for now, always nest into data frame
          unpack_sub_lists = FALSE, nest_into_data_frame = TRUE) %>%
        rename(..nr.. = ..parent_nr..)
    }
  }

  # nest into data frame
  if (nest_into_data_frame) {
    lists_df_wide <- lists_df_wide %>%
      select(!!!syms(c("..nr..", data_classes$name))) %>%
      nest(!!col_quo := c(-..nr..))
  } else {
    # no nesting, just select right columns
    lists_df_wide <- select(lists_df_wide, !!!syms(c("..nr..", data_classes$name)))
  }

  # merge with original data
  lists_df_original %>%
    select(!!quo(-!!sym(quo_text(col_quo)))) %>%
    left_join(lists_df_wide, by = "..nr..") %>%
    select(-..nr..)
}

# remove list columns from a data frame
remove_list_columns <- function(df) {
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  list_cols <- df %>% purrr:::map_lgl(rlang::is_list)
  df[!list_cols]
}

# data simplification ====

spread_state_columns <- function(df) {
  df %>%
    mutate(value = ifelse(!is.na(units), str_c(value, " ", units), value)) %>%
    select(-units) %>%
    spread(key, value)
}

spread_data_columns <- function(df) {
  df %>%
    mutate(
      key = str_c("#", idx, ": ", key),
      value = ifelse(!is.na(units), str_c(value, " ", units), value)
    ) %>%
    select(-units, -idx) %>%
    spread(key, value)
}

# calculations =====

#' Convert datetime to duration
#'
#' Does not need to be sorted upfront, makes different to minimum datetime within the group_by groupings.
#'
#' @param df data frame with datetime column
#' @param units any time unit that lubridate understands
#' @return df with new column \code{duration}
#' @export
ll_calculate_duration <- function(df, units) {
  stopifnot(!missing(units))
  df %>%
    mutate(duration = as.duration(datetime - min(datetime)) %>% as.numeric(units))
}
