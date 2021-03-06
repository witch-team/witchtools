% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/witch_region_mapping.R
\name{witch_region_mapping}
\alias{witch_region_mapping}
\title{Read region mapping from a regional WITCH GAMS file.}
\usage{
witch_region_mapping(f)
}
\arguments{
\item{f}{Regional WITCH GAMS file}
}
\description{
\code{witch_region_mapping} reads a regional WITCH GAMS file and returns the
regional mapping as defined in the file. The function scans the GAMS file
with the assumptions that each iso3 is on one line and in a set named map_*.
}
\examples{
\dontrun{
witch_region_mapping("input/regions/witch17.inc")
}

}
