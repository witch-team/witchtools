# Single item conversion for batch conversion function
convert_item <- function(.data,
                         reg_id,
                         time_id,
                         region_name,
                         item_param,
                         time_mappings,
                         region_mappings,
                         weights,
                         guess_region,
                         guess_input_t,
                         default_agg_missing) {

  year <- type <- value <- NULL # due to NSE notes in R CMD check

  if (region_name %in% colnames(.data)) {
    data.table::setnames(.data, region_name, guess_region)
  }
  if ("t" %in% colnames(.data) & guess_input_t == "t30") {
    data.table::setnames(.data, "t", "year")
    .data[, year := paste(as.numeric(.data$year) * 5 + 2000)]
  }

  data_indices <- colnames(.data)

  convpar <- list()

  # Time period conversion
  do_time_period <- ("year" %in% colnames(.data) & time_id != "year")

  if (do_time_period) {
    data_indices[data_indices == "year"] <- "t"
  }

  # Region conversion
  data_reg <- intersect(colnames(.data), names(region_mappings))
  do_region <- FALSE
  # deal with case when data_reg == character(0)
  if (length(data_reg) == 1) {
    do_region <- (data_reg != reg_id)
    data_indices[data_indices == data_reg] <- region_name
  }
  from_reg <- NULL
  to_reg <- NULL

  if (do_region) {

    # Ensure region is lower case / iso3 is upper case
    if (data_reg != "iso3") {
      .idx <- which(data_reg == colnames(.data))
      .data[[.idx]] <- tolower(.data[, get(data_reg)])
    } else {
      .data$iso3 <- toupper(.data$iso3)
    }

    # Set initial regional mapping
    if (data_reg != "iso3") {
      from_reg <- region_mappings[[data_reg]]
    } else {
      from_reg <- "iso3"
    }

    # Set final regional mapping
    to_reg <- region_mappings[[reg_id]]
  }

  # Conversion options
  convopt <- c(convpar,
    do_extrap = (nrow(item_param[type == "extrap" &
      value == "skip"]) == 0),
    do_interp = (nrow(item_param[type == "interp" &
      value == "skip"]) == 0),
    do_past_extrap = TRUE
  )

  if (!convopt[["do_extrap"]]) {
    convopt[["do_past_extrap"]] <- FALSE
  }

  # No inter/extrapolation for stochastic branch
  if (stringr::str_detect(time_id, "branch")) {
    convopt[["do_extrap"]] <- FALSE
    convopt[["do_interp"]] <- FALSE
  }

  # Missing values
  convopt[["agg_missing"]] <- default_agg_missing
  if (nrow(item_param[type == "missing_values"]) > 0) {
    convopt[["agg_missing"]] <- item_param[type == "missing_values"][1, value]
  }

  # Regional aggregation operator
  convopt[["agg_operator"]] <- "sum"
  if (nrow(item_param[type == "nagg"]) > 0) {
    convopt[["agg_operator"]] <- item_param[type == "nagg"][1, value]
  }

  # Regional aggregation weight
  nweight <- "pop"
  if (nrow(item_param[type == "nweight"]) > 0) {
    nweight <- item_param[type == "nweight"][1, value]
  } else {
    nweight <- ifelse(convopt[["agg_operator"]] %in% c("mean"), "cst", "gdp")
  }
  # Ensure max and min have cst weight
  if (convopt[["agg_operator"]] %in% c("min", "max")) {
    nweight <- "cst"
  }

  if (!nweight %in% names(weights)) {
    stop(paste(nweight, "not in weights!"))
  }

  # Time aggregation function (mean, sum, max)
  tagg <- "mean"
  if (nrow(item_param[type == "tagg"]) > 0) {
    tagg <- item_param[type == "tagg"][1, value]
  }

  .conv <- convert_table(.data,
    time_mapping = time_mappings[[time_id]],
    time_aggregate = tagg,
    from_reg = from_reg,
    to_reg = region_mappings[[reg_id]],
    agg_weight = weights[[nweight]],
    options = convopt,
    do_time_period = do_time_period,
    do_region = do_region,
    verbose = TRUE,
    info = TRUE
  )

  .data <- .conv[["data"]]
  .info_share <- .conv[["info"]]

  if (reg_id %in% colnames(.data)) {
    data.table::setnames(.data, reg_id, region_name)
  }

  # rename reg_id in necessary (when no region conversion is required)
  if (reg_id %in% colnames(.data)) {
    cat(crayon::blue(paste("   [same]\n")))
  } else if (do_region) {
    cat(crayon::blue(paste0(
      "   [agg: ", convopt[["agg_operator"]], ", wgt: ",
      nweight, "]\n"
    )))
  }

  # Ensure the original order is kept
  data.table::setcolorder(.data, data_indices)

  if (!is.null(.info_share)) {
    if (reg_id %in% colnames(.info_share)) {
      data.table::setnames(.info_share, reg_id, region_name)
    }
    data.table::setcolorder(.info_share, data_indices)
  }

  return(list(.data, .info_share))
}
