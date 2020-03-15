# Convert from year to period t (mean value, interpolate, extrapolate)
# time_mapping should have the columns year,tperiod,refyear
#' @importFrom stats approx
#' @export

convert_time_period <- function(.x, time_mapping,
                                do_extrap = FALSE, do_interp = FALSE,
                                year_name = "year", value_name = "value",
                                period_name = "t",
                                verbose = TRUE) {

  # Merge time mapping
  .x[[year_name]] = as.numeric(.x[[year_name]])
  .x <- merge(.x, time_mapping[,.(t,year)], by = year_name,
                 allow.cartesian = TRUE)

  # Check if interpolation/extrapolation are required
  .ry <- as.numeric(unique(time_mapping$refyear))
  .dy <- unique(.x$year)
  if (nrow(.x) > 0) {
    do_extrap <- ((max(.ry) > max(.dy)) | (min(.ry) < min(.dy))) & do_extrap
    do_interp <- (length(setdiff(.ry[.ry >= min(.dy) & .ry <= max(.dy)],.dy)) > 0) & do_interp
  } else {
    # if no rows, override
    do_extrap <- do_interp <- FALSE
  }

  if (do_extrap & verbose) {
    cat(crayon::magenta(paste0(" - extrapolating outside [", min(.dy), ";",
                               max(.dy),"].\n")))
  }

  if (do_interp & verbose) {
    interp_years = setdiff(.ry[.ry >= min(.dy) & .ry <= max(.dy)],.dy)
    cat(crayon::magenta(paste0(" -  interpolating in ",
                               paste(interp_years,collapse = ","),".\n")))
  }

  if ((do_extrap | do_interp)) {

    .rt <- unique(time_mapping$tperiod)
    missing_t <- .rt[!.rt %in% .x$t]
    missing_time <- subset(time_mapping,tperiod %in% missing_t & refyear == year)
    if (!do_extrap) {
      missing_time <- missing_time[year <= max(.dy)]
    }
    if (!do_interp) {
      missing_time <- missing_time[!year %in% setdiff(.ry[.ry >= min(.dy) & .ry <= max(.dy)],.dy)]
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

  .x[, (year_name) := NULL]

  # Take the average value over time range
  .x <- .x[, .(value = mean(value,na.rm = TRUE)),
                 by = c(colnames(.x)[colnames(.x) != value_name])]
  .x[is.nan(value),value := NA]

  return(.x)

}
