

convert_DT <- function(.x,
                    ...,
                    params = list(),
                    time_mapping = NULL,
                    do_time_period = T,
                    do_region = T
                    ) {

  dots <- list(...)
  ndots <- length(dots)
  dots <- c(dots, params)

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
    .x <- do.call(convert_time_period, c(list(.x = .x,
                                              time_mapping = time_mapping),
                                         time_params))
  }

  return(.x)

}
