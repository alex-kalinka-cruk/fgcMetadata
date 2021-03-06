% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_metadata.R
\name{experiment_metadata}
\alias{experiment_metadata}
\title{experiment_metadata}
\usage{
experiment_metadata(meta, experiment_csv)
}
\arguments{
\item{meta}{An object of class \code{fgcMeta}, as produced by \code{fgcMetadata::start_fgc_project}.}

\item{experiment_csv}{A path to an experiment '.csv' file.}
}
\value{
Returns an object of class \code{fgcMeta}. Also saves data in \code{experiment.csv} in \code{meta$data_dir}.
}
\description{
Add sample metadata to an object of class \code{fgcMeta}, as produced by \code{fgcMetadata::start_fgc_project}.
}
