#' Convert a formatted data.table from year to time period.
#'
#' \code{convert_time_period} returns a formatted data.table aggregating
#' yearly values into several-year time periods.
#' The input data.table should contain the columns "year" and "value",
#' while the other columns are are considered as id columns.
#' The resulting data.table will have a column "t" instead of "year.
#' Note that the period, year and value column names can be specified.
#' The time mapping between year and time period should be provided as
#' a data.table with columns "year" and "t", and refyear for interpolation and
#' extrapolation. In case of missing periods, these can be linearly interpolated
#' or constantly interpolated. If several years are mapped into the same period,
#' values are averaged or the function \code{fun.aggregate} is used.
#'
#' @param .x a well-formatted data.table.
#' @param time_mapping a time mapping data.table.
#' @param do_interp logical indicating whether linear interpolation should be done.
#' @param do_extrap logical indicating whether constant extrapolation should be done.
#' @param do_past_extrap logical indicating whether constant extrapolation should be done for past value.
#' @param year_name string column name of year in original data.
#' @param value_name string column name of value in original data.
#' @param period_name string column name of period in original data.
#' @param fun.aggregate function to aggregate yearly values in a period.
#' @param na.rm logical indicating whether missing values should be removed.
#' @param verbose logical indicating whether running in verbose mode.
#' @return a converted data.table
#' @importFrom stats approx
#' @export
#' @examples
#' \dontrun{
#'   # load time mapping t30
#'   f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
#'   tm <- load_timescale_mapping(f)
#'
#'   # original data.table
#'   dd <- data.table(year = c(2010,2020), value = 1:2)
#'
#'   # Convert into time period with linear interpolation
#'   convert_time_period(dd, tm, do_interp = T)
#' }

convert_time_period <- function(.x, time_mapping,
                                do_interp = FALSE,
                                do_extrap = FALSE,
                                do_past_extrap = FALSE,
                                year_name = "year",
                                value_name = "value",
                                period_name = "t",
                                fun.aggregate = mean,
                                na.rm = TRUE,
                                verbose = FALSE) {

  if (!data.table::is.data.table(.x)) stop('.x should be a data.table')

  # Merge time mapping
  .x[, (year_name) := as.numeric(get(year_name))]
  .x <- merge(.x, time_mapping[,.(t,year)],
              by.x = year_name,
              by.y = "year",
              allow.cartesian = TRUE)

  if (nrow(.x) > 0 & (do_extrap | do_interp)) {

    .ry <- as.numeric(unique(time_mapping$refyear))
    .dy <- unique(.x$year)

    if (!"tperiod" %in% names(time_mapping)) {
      time_mapping[, tperiod := t]
    }
    .rt <- unique(time_mapping$tperiod)
    missing_t <- .rt[!.rt %in% .x$t]
    missing_time <- subset(time_mapping,tperiod %in% missing_t & refyear == year)

    if (!do_extrap) {
      if (do_past_extrap) {
        missing_time <- missing_time[year <= max(.dy)]
      } else {
        missing_time <- missing_time[year >= min(.dy) & year <= max(.dy)]
      }
    }
    if (!do_interp) {
      missing_time <- missing_time[!year %in% setdiff(.ry[.ry >= min(.dy) & .ry <= max(.dy)],.dy)]
    }

    if (nrow(missing_time) > 0) {
      if (verbose) {
        cat(crayon::magenta(paste0(" -  fill values for ",paste(missing_time$year,collapse = ","),".\n")))
      }

      inter_extra <- function(sd){
        if (nrow(sd) == 1) {
          v <- sd[[value_name]]
        } else {
          v <- approx(x = sd[[year_name]], y = sd[[value_name]],
                      xout = missing_time$year, rule = 2)$y
        }
        return(list(year = missing_time$year,
                    t = missing_time$t,
                    value = v))
      }
      .newdata <- .x[,inter_extra(.SD),
                     by = c(colnames(.x)[!colnames(.x) %in% c(value_name,year_name,period_name)])]
      .x <- rbind(.x,.newdata)
    }
  }

  .x[, (year_name) := NULL]

  # Take the average value over time range
  .x <- .x[, .(value = fun.aggregate(value,na.rm = na.rm)),
                 by = c(colnames(.x)[colnames(.x) != value_name])]
  .x[is.nan(value),value := NA]

  return(.x)

}