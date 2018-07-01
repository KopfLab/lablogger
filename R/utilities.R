# function to reliably turn a list of lists into a data frame
# @param lists_df a data frame with a column that has lists in each row
# @param column the name of the column that has the list
# @param unnest_single_values whether to unnest single values (values that have a none or only a single entry for all retrieve records)
# @param unpack_sub_lists - whether to unpack remaining list columns (only evaluated if unnest_single_values = TRUE)
# @param nest_into_data_frame - whether to nest the unpacked lists into a data frame or keep the columns all unpacked
unpack_lists_data_frame <- function(lists_df, column = lists, unnest_single_values = TRUE, unpack_sub_lists = FALSE, nest_into_data_frame = FALSE) {

  # id rows
  col_quo <- enquo(column)
  lists_df_original <- mutate(lists_df, ..nr.. = row_number())

  # convert lists into data frame format
  lists_df <-
    lists_df_original %>%
    rename(lists = !!col_quo) %>%
    mutate(name = map(lists, ~if(length(.x) == 0) { NA_character_ } else { names(.x) })) %>%
    unnest(name, .drop = FALSE) %>%
    mutate(
      class = map2_chr(lists, name, ~class(.x[[.y]])[1]),
      length = map2_int(lists, name, ~length(.x[[.y]])),
      value = map2(lists, name, ~.x[[.y]]),
      name = str_to_lower(name)
    )

  # data classes
  data_classes <-
    lists_df %>%
    group_by(name) %>%
    summarize(
      data_class = unique(class)[1],
      value_max_n = as.integer(max(length)))

  # lists wide
  lists_df_wide <- lists_df %>%
    select(..nr.., name, value) %>%
    spread(name, value)

  # deal with NAs
  if ("<NA>" %in% names(lists_df_wide))
    lists_df_wide <- select(lists_df_wide, -`<NA>`)

  # remove columns that hold no data at all
  null_cols <- filter(data_classes, value_max_n == 0 | data_class == "NULL")$name
  lists_df_wide <- lists_df_wide[!names(lists_df_wide) %in% null_cols]

  # fill NULL values with NA to not loose records during unnesting (except for lists)
  for (i in 1:nrow(data_classes)) {
    lists_df_wide <-
      with(data_classes[i,], {
        # make sure the function exists
        if (exists(data_class)) {
          if (data_class %in% c("character", "integer", "numeric"))
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

  # unnest all the ones that have only single value
  if (unnest_single_values) {
    col_order <- names(lists_df_wide)
    unnest_cols <- data_classes %>%
      filter(value_max_n == 1, data_class %in% c("character", "integer", "numeric", "logical")) %>%
      {.$name}
    lists_df_wide <- unnest(lists_df_wide, !!!syms(unnest_cols), .drop = FALSE) %>%
      select(!!!syms(col_order))
  }

  # unpack sub lists
  if (unpack_sub_lists) {
    col_order <- names(lists_df_wide)
    unpack_cols <- filter(data_classes, data_class == "list")$name
    for (col in unpack_cols) {
      lists_df_wide <-
        lists_df_wide %>% rename(..parent_nr.. = ..nr..) %>%
        unpack_lists_data_frame(
          column = !!sym(col), unnest_single_values = unnest_single_values,
          # don't allow recursive unpacking for now, always nest into data frame
          unpack_sub_lists = FALSE, nest_into_data_frame = TRUE) %>%
        rename(..nr.. = ..parent_nr..)
    }
  }

  # nest into data frame
  if (nest_into_data_frame) {
    lists_df_wide <- nest(lists_df_wide, -..nr.., .key = !!col_quo)
  }

  # merge with original data
  lists_df_original %>%
    select(!!quo(-!!sym(quo_text(col_quo)))) %>%
    left_join(lists_df_wide, by = "..nr..") %>%
    select(-..nr..)
}
