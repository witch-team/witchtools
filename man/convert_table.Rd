% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_table.R
\name{convert_table}
\alias{convert_table}
\title{Convert time periods and regions in a table.}
\usage{
convert_table(
  .x,
  ...,
  options = list(),
  time_mapping = NULL,
  from_reg = NULL,
  to_reg = NULL,
  agg_weight = NULL,
  regions = NULL,
  do_time_period = TRUE,
  do_region = TRUE,
  info = FALSE
)
}
\arguments{
\item{.x}{a well-formatted data.table.}

\item{\dots}{parameters to send to convert_region or
convert_time_period.}

\item{options}{a list of parameters to send to convert_region or
convert_time_period.}

\item{time_mapping}{a time mapping data.table.}

\item{from_reg}{initial regional mapping name or a data.table with
the mapping.}

\item{to_reg}{final regional mapping name  or a data.table with the mapping.}

\item{agg_weight}{aggregation weight data.table}

\item{regions}{optional list of region mappings (see Details for format)}

\item{do_time_period}{logical indicating whether years should be converted.}

\item{do_region}{logical indicating whether region should be converted.}

\item{info}{logical indicating whether to include information, only necessary
when the agg_operator is "sumby".}
}
\value{
a data.table or list containing a converted data.table and
information about the coperture if available.
}
\description{
\code{convert_table} returns a list containing a data.table where values are
converted from years into time periods and from one regional mapping to
another. The function is calling \code{convert_time_period} and
\code{convert_region}. More details about the parameters in these functions.
The input table might be a data.table or a data.frame and
should contain a column "value". All others columns are considered as
indices.
}
\examples{
\dontrun{

convert_table(gdp_iso3, to_reg = "witch17", time_mapping = "t30")
}

}
\seealso{
\code{\link{convert_gdx}} for WITCH gdx files.

Other conversion functions: 
\code{\link{convert_gdx}()},
\code{\link{convert_region}()},
\code{\link{convert_sqlite}()},
\code{\link{convert_time_period}()}
}
\concept{conversion functions}
