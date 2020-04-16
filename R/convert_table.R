#' Convert time periods and regions in a table.
#'
#' \code{convert_table} returns a list containing a data.table where values are
#' converted from years into time periods and from one regional mapping to
#' another. The function is calling \code{convert_time_period} and
#' \code{convert_region}. More details about the parameters in these functions.
#' The input table might be a data.table or a data.frame and
#' should contain a column "value". All others columns are considered as
#' indices.
#'
#' @family conversion functions
#' @seealso \code{\link{convert_gdx}} for WITCH gdx files.
#'
#' @param .x a well-formatted data.table.
#' @param \dots parameters to send to convert_region or
#' convert_time_period.
#' @param options a list of parameters to send to convert_region or
#' convert_time_period.
#' @param time_mapping a time mapping data.table.
#' @param from_reg initial regional mapping name or a data.table with
#' the mapping.
#' @param to_reg final regional mapping name  or a data.table with the mapping.
#' @param agg_weight aggregation weight data.table
#' @param regions optional list of region mappings (see Details for format)
#' @param do_time_period logical indicating whether years should be converted.
#' @param do_region logical indicating whether region should be converted.
#' @param info logical indicating whether to include information, only necessary
#'  when the agg_operator is "sumby".
#'
#' @return a data.table or list containing a converted data.table and
#' information about the coperture if available.
#' @export
#' @examples
#' \dontrun{
#'
#' convert_table(gdp_iso3, to_reg = 'witch17', time_mapping = "t30")
#'
#' }
#'

convert_table <- function(.x,
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
                          ) {

  dots <- list(...)
  ndots <- length(dots)
  dots <- c(dots, options)

  if (do_time_period) {
    time_params <- dots[which(names(dots) %in% c("do_interp",
                                                 "do_extrap",
                                                 "do_past_extrap",
                                                 "year_name",
                                                 "value_name",
                                                 "period_name",
                                                 "fun.aggregate",
                                                 "na.rm",
                                                 "verbose"))]
    .x <- do.call(convert_time_period, c(list(.x = data.table::setDT(.x),
                                              time_mapping = time_mapping),
                                         time_params))
  }

  .info_share <- NULL

  if (do_region) {

    region_params <- dots[which(names(dots) %in% c("agg_operator",
                                                 "agg_missing"
                                                 ))]

    .conv <- do.call(convert_region, c(list(.x = data.table::setDT(.x),
                                            from_reg = from_reg,
                                            to_reg = to_reg,
                                            agg_weight = agg_weight,
                                            regions = regions,
                                            info = info),
                                       region_params))

    if (info) {
      .x <- .conv[['data']]
      .info_share <- .conv[['info']]
    } else {
      .x <- .conv
    }

  }

  if (info) {
    return(list(data = .x, info = .info_share))
  } else {
    return(.x)
  }

}
