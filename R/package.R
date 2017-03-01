#' @title Chemostat Control Center (C3)
#' @description Test
#' @name labwareC3
#' @docType package
#' @author Sebastian Kopf
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom stats setNames
#'
#' @include gui.R
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
