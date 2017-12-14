# general data retrieval functions


# retrieve active cameras
get_camera_df <- function(pool) {
  cat("\nInfo: retrieving active cameras... ")
  df <- tbl(pool, "camera") %>%
    filter(active) %>%
    collect()
  cat(glue("found {nrow(df)}\n"))
  return(df)
}



# maybe implement something like this
# sqlQuery <- function (query) {
#
#   # creating DB connection object with RMysql package
#   DB <- dbConnect(MySQL(), user="youruser", password='yourpassword', dbname='yourdb', host='192.168.178.1')
#
#   # close db connection after function call exits
#   on.exit(dbDisconnect(DB))
#
#   # send Query to btain result set
#   rs <- dbSendQuery(DB, query)
#
#   # get elements from result sets and convert to dataframe
#   result <- fetch(rs, -1)
#
#   # return the dataframe
#   return(result)
# }
