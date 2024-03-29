% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_libraries.R
\name{require_package}
\alias{require_package}
\title{Install, if necessary, and load a R library}
\usage{
require_package(package, loading = TRUE)
}
\arguments{
\item{package}{package name}

\item{loading}{if TRUE, package is loaded}
}
\description{
Install, if necessary, and load a R library
}
\examples{
\dontrun{
require_package("data.table", loading = FALSE)
}
}
