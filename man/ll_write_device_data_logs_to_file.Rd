% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_write.R
\name{ll_write_device_data_logs_to_file}
\alias{ll_write_device_data_logs_to_file}
\title{Save device data logs}
\usage{
ll_write_device_data_logs_to_file(
  device_data_logs,
  file_path,
  zip = FALSE,
  quiet = default(quiet)
)
}
\arguments{
\item{device_data_logs}{data logs to save}

\item{file_path}{path to the .rds, .xlsx and/or .csv files (can be multiple). All folders must already exist.}

\item{zip}{whether to zip the resulting file(s). Use \code{zip = TRUE} to have the function infer the zip file name from the .rds/.xlsx file path, or alternatively provide a file path with a specific name for the zip file (\code{zip = file.path(...)}).}

\item{generate_rmd}{whether to generate an RMarkdown file for analysis. If both .rds and .xlsx file paths are provided, reads from the one listed first. Use \code{generate_rmd = TRUE} to have the function infer the Rmd file name from the .rds/.xlsx file path, or alternatively provide a file path with a specific name for the Rmd file (\code{generate_rmd = file.path(...)}).}
}
\value{
returns the logs invisibly for piping
}
\description{
Write device data logs to an .rds, .xlsx (excel), and/or .csv file and optionally compress them into a zip archive. Note that target files will be overwritten if they already exist.
}
\seealso{
Other data logs functions: 
\code{\link{ll_get_device_data_logs}()},
\code{\link{ll_plot_device_data_logs}()},
\code{\link{ll_read_device_data_logs_from_file}()}
}
\concept{data logs functions}
