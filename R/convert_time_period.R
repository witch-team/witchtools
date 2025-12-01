#' Convert a data.table from year to time period.
#'
#' \code{convert_time_period} returns a formatted data.table aggregating
#' values from years into time periods. The required format of the input data is
#' described in Details.
#'
#' The input data.table should contain the columns "year" and "value",
#' while the other columns are are considered as id columns.
#' The resulting data.table will have a column "t" instead of "year.
#' Note that the period, year and value column names can be specified.
#'
#' The time mapping between year and time period should be provided as
#' a data.table with columns "year" and "t", and refyear for interpolation and
#' extrapolation. In case of missing periods, these can be interpolated using
#' linear, exponential, or spline methods. If several years are mapped into the
#' same period, values are averaged or the function \code{fun.aggregate} is used.
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}},
#' \code{\link{convert_gdx}}.
#'
#' @param .x a well-formatted data.table.
#' @param time_mapping a time mapping data.table.
#' @param do_interp logical indicating whether interpolation
#' should be done.
#' @param interp_method character string specifying interpolation method:
#' "linear" (default), "exponential", or "spline" with optional method suffix.
#' For spline interpolation, you can specify the method: "spline" (uses "fmm"),
#' "spline_fmm", "spline_natural", "spline_periodic", "spline_monoH.FC", or
#' "spline_hyman".
#' @param do_extrap logical indicating whether constant extrapolation
#' should be done.
#' @param do_past_extrap logical indicating whether constant extrapolation
#' should be done for past value.
#' @param year_name string column name of year.
#' @param value_name string column name of value.
#' @param time_aggregate function to aggregate yearly values
#' ("mean","sum","max").
#' @param na.rm logical indicating whether missing values should be removed.
#' @param verbose logical indicating whether running in verbose mode.
#'
#' @return a converted data.table
#' @importFrom stats approx splinefun
#' @export
#' @examples
#'
#' library(data.table)
#'
#' # original data.table
#' dd <- data.table(year = 2005:2050, value = 1:46)
#'
#' # Convert yearly time-serie into time period
#' convert_time_period(dd, "t30")
convert_time_period <- function(.x,
                                time_mapping,
                                do_interp = FALSE,
                                interp_method = "linear",
                                do_extrap = FALSE,
                                do_past_extrap = FALSE,
                                year_name = "year",
                                value_name = "value",
                                time_aggregate = "mean",
                                na.rm = TRUE,
                                verbose = FALSE) {
  if (!data.table::is.data.table(.x)) .x <- data.table::setDT(.x)

  # Check input
  if (!time_aggregate %in% c("mean", "sum", "max")) {
    stop(paste0("time_aggregate function not implemented."))
  }

  # Parse and validate interpolation method
  .base_method <- interp_method
  .spline_method <- "fmm"  # default for spline

  if (startsWith(interp_method, "spline")) {
    .base_method <- "spline"
    if (interp_method != "spline") {
      # Extract method after underscore
      .parts <- strsplit(interp_method, "_", fixed = TRUE)[[1]]
      if (length(.parts) == 2 && .parts[1] == "spline") {
        .spline_method <- .parts[2]
        if (!.spline_method %in% c("fmm", "natural", "periodic", "monoH.FC", "hyman")) {
          stop(paste0(
            "Invalid spline method '", .spline_method, "'. ",
            "Must be one of: fmm, natural, periodic, monoH.FC, hyman."
          ))
        }
      } else {
        stop(paste0(
          "Invalid interp_method format. ",
          "Use 'spline' or 'spline_<method>' where method is one of: ",
          "fmm, natural, periodic, monoH.FC, hyman."
        ))
      }
    }
  } else if (!.base_method %in% c("linear", "exponential")) {
    stop(paste0(
      "interp_method must be 'linear', 'exponential', or 'spline' ",
      "(optionally with method suffix like 'spline_natural')."
    ))
  }

  # Guess time mapping if not directly provided
  if (is.character(time_mapping)) {
    if (!time_mapping %in% c(names(witchtools::time_mappings))) {
      stop(paste0("time_mapping should provided by time_mappings."))
    }
    time_mapping <- time_mappings[[time_mapping]]
  } else if (!data.table::is.data.table(time_mapping)) {
    stop(paste0("time_mapping should be a character or a data.table."))
  }

  # Merge time mapping
  .x[, (year_name) := as.numeric(get(year_name))]
  .x <- merge(.x, time_mapping[, .(t, year)],
    by.x = year_name,
    by.y = "year",
    allow.cartesian = TRUE
  )

  if (nrow(.x) > 0 & (do_extrap | do_interp)) {
    .ry <- as.numeric(unique(time_mapping$refyear))
    .dy <- unique(.x$year)

    if (!"tperiod" %in% names(time_mapping)) {
      time_mapping[, tperiod := t]
    }
    .rt <- unique(time_mapping$tperiod)
    missing_t <- .rt[!.rt %in% .x$t]
    missing_time <- subset(
      time_mapping,
      tperiod %in% missing_t & refyear == year
    )

    if (!do_extrap) {
      if (do_past_extrap) {
        missing_time <- missing_time[year <= max(.dy)]
      } else {
        missing_time <- missing_time[year >= min(.dy) & year <= max(.dy)]
      }
    }
    if (!do_interp) {
      missing_time <- missing_time[!year %in%
        setdiff(.ry[.ry >= min(.dy) &
          .ry <= max(.dy)], .dy)]
    }

    if (nrow(missing_time) > 0) {
      if (verbose) {
        cat(crayon::magenta(paste0(
          "   time_period: fill ",
          paste(missing_time$year, collapse = ","),
          ".\n"
        )))
      }

      inter_extra <- function(sd) {
        if (nrow(sd) == 1) {
          v <- sd[[value_name]]
        } else {
          # Linear interpolation with constant extrapolation (rule = 2)
          v <- approx(
            x = sd[[year_name]], y = sd[[value_name]],
            xout = missing_time$year, rule = 2
          )$y

          # Override interior points with alternative interpolation method
          if (.base_method != "linear") {
            .interp_mask <- missing_time$year >= min(sd[[year_name]]) &
              missing_time$year <= max(sd[[year_name]])
            if (any(.interp_mask)) {
              if (.base_method == "exponential") {
                if (any(sd[[value_name]] <= 0)) {
                  warning(
                    "Exponential interpolation requires positive values. ",
                    "Using linear."
                  )
                } else {
                  v[.interp_mask] <- exp(approx(
                    x = sd[[year_name]], y = log(sd[[value_name]]),
                    xout = missing_time$year[.interp_mask], rule = 1
                  )$y)
                }
              } else if (.base_method == "spline") {
                .spline_fn <- splinefun(
                  x = sd[[year_name]], y = sd[[value_name]],
                  method = .spline_method
                )
                v[.interp_mask] <- .spline_fn(missing_time$year[.interp_mask])
              }
            }
          }
        }
        return(list(
          year = missing_time$year,
          t = missing_time$t,
          value = v
        ))
      }
      .newdata <- .x[, inter_extra(.SD),
        by = c(colnames(.x)[!colnames(.x) %in%
          c(
            value_name,
            year_name,
            "t"
          )])
      ]
      .x <- rbind(.x, .newdata)
    }
  }

  .x[, (year_name) := NULL]

  # Take the average value over time range
  if (time_aggregate == "mean") {
    .x <- .x[, .(value = mean(value, na.rm = na.rm)),
             by = c(colnames(.x)[colnames(.x) != value_name])
    ]
  }

  # Sum the value over time range
  if (time_aggregate == "sum") {
    .x <- .x[, .(value = sum(value, na.rm = na.rm)),
             by = c(colnames(.x)[colnames(.x) != value_name])
    ]
  }

  # Maximum over time range
  if (time_aggregate == "max") {
    .x <- .x[, .(value = max(value, na.rm = na.rm)),
             by = c(colnames(.x)[colnames(.x) != value_name])
    ]
  }

  .x[is.nan(value), value := NA]
  data.table::setnames(.x, "value", value_name)

  return(.x)
}
