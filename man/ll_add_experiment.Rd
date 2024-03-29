% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{ll_add_experiment}
\alias{ll_add_experiment}
\title{Add new experiment}
\usage{
ll_add_experiment(
  exp_id,
  exp_desc = NA,
  exp_notes = NA,
  group_id = default(group_id),
  con = default(con),
  quiet = default(quiet)
)
}
\arguments{
\item{exp_id}{the unique identifier of the experiment (usually a few letter code). Will error if id already exists.}

\item{group_id}{group name, must exist in database (will error if not)}

\item{exp_name}{longer name of the experiment}

\item{desc}{device description}

\item{device_type_id}{device type name, must exist in database (will error if not)}

\item{particle_id}{optional, will be automatically filled in the first time the device logs to the database}

\item{in_use}{whether device is in use (if not, cannot log any data)}
}
\description{
Add new experiment
}
