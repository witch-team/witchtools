% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/witch_data.R
\name{witch_data}
\alias{witch_data}
\title{Handler for witch-data files in WITCH}
\usage{
witch_data(
  file,
  version = NULL,
  idir = getOption("witchtools.idir"),
  method = getOption("witchtools.method"),
  noCheck = getOption("witchtools.noCheck", FALSE),
  repo = getOption("witchtools.witch_data_repo"),
  remote = getOption("witchtools.witch_data_remote")
)
}
\arguments{
\item{file}{Name of the file in the with-data repository}

\item{version}{Release version of the file (required for method piggyback)}

\item{idir}{directory to read/download files}

\item{method}{'piggyback' or 'witch-data'.}

\item{noCheck}{For the piggyback method, don't check and download the file.}

\item{repo}{github repository name}

\item{remote}{dvc remote storage}
}
\description{
Returns the location of a file from the witch-data repository for processing,
after downloading it from github if necessary using the piggyback library.
}
\examples{
\dontrun{
witch_data("ssp/ssp_population.csv", version = "v0.0.1")
witch_data("ssp/ssp_population.csv", method = "witch-data")
witch_data("ssp/ssp_population.csv", noCheck = TRUE)
}
}
