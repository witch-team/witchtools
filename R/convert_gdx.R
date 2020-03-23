# Main function to convert GDX
#' @importFrom stats approx

convert_gdx <- function(gdxfile,
                        reg_id,
                        time_id,
                        region_mappings,
                        time_mappings,
                        weights = default_weights,
                        output_directory,
                        guess_input_n = "witch17",
                        guess_input_t = "t30",
                        default_missing_values = "zero",
                        default_meta_param = witch_default_meta_param()){

  if (!file.exists(gdxfile)) stop(paste(gdxfile, "does not exist!"))

  cat(crayon::blue$bold(paste("Processing", basename(gdxfile),"\n")))

  .gdx <- gdxtools::gdx(gdxfile)

  # parameter collector
  params <- list()
  vars <- list()

  # load meta_data
  meta_param <- default_meta_param
  if ("meta_param" %in% .gdx$sets$name) {
    dt_meta_param <- data.table::setDT(.gdx["meta_param"])
    names(dt_meta_param) <- c("parameter","type","value")
    meta_param <- rbind(meta_param,dt_meta_param)
  }

  items <- c(.gdx$parameters$name, .gdx$variables$name)

  #Loop over all parameters and variables in the file
  for (item in items) {

    item_type <- ifelse(item %in% .gdx$parameters$name, "parameter", "variable")

    # uses as.data.table to keep attribute 'gams'
    .data <- data.table::setDT(.gdx[item])
    text <- attributes(.data)[["gams"]]

    if ("n" %in% colnames(.data)) {
      data.table::setnames(.data, "n", guess_input_n)
    }
    if ("t" %in% colnames(.data) & guess_input_t == "t30") {
      data.table::setnames(.data, "t", "year")
      .data[,year := paste(as.numeric(.data$year) * 5 + 2000)]
    }

    data_indices <- colnames(.data)

    # Time conversion
    if ("year" %in% colnames(.data) &
        !stringr::str_detect(basename(gdxfile),"hist")) {

      # Check inter/extrapolation parameters
      do_extrap <- (nrow(meta_param[parameter == item &
                                      type == "extrap" &
                                      value == "skip"]) == 0)
      do_interp <- (nrow(meta_param[parameter == item &
                                      type == "interp" &
                                      value == "skip"]) == 0)
      do_past_extrap <- TRUE
      if (!do_extrap) do_past_extrap <- FALSE

      # No inter/extrapolation for stochastic branch
      if (stringr::str_detect(time_id, "branch")) {
        do_extrap <- FALSE
        do_interp <- FALSE
      }

      # Convert years into time periods
      .data <- convert_time_period(.data,
                                   time_mappings[[time_id]],
                                   do_interp,
                                   do_extrap,
                                   do_past_extrap)

      # Update indices
      data_indices[data_indices == "year"] <- "t"

    }

    # Region conversion
    input_reg_id <- intersect(colnames(.data), names(region_mappings))
    param_agg <- ""

    # Region has been identified?
    if (length(input_reg_id) == 1) {

      # Detect the type of missing value
      missing_values <- default_missing_values
      if (nrow(meta_param[parameter == item & type == "missing_values"]) > 0) {
        missing_values <- meta_param[parameter == item &
                                       type == "missing_values"][1,value]
      }

      # Ensure region/iso3 is lower case
      if (input_reg_id != "iso3") {
        .data[[which(input_reg_id == colnames(.data))]] <- tolower(.data[,get(input_reg_id)])
      } else {
        .data$iso3 <- toupper(.data$iso3)
      }

      # Select aggregation type (sum or mean)
      find_w <- meta_param[parameter == item & type == "nagg"]
      if (nrow(find_w) > 0) {
        param_agg <- find_w[1,value]
      }else{
        param_agg <- "sum"
      }

      # Select weighting
      find_w <- meta_param[parameter == item & type == "nweight"]
      if (nrow(find_w) > 0) {
        param_w <- find_w[1,value]
      }else{
        param_w <- ifelse(param_agg == "mean","cst","gdp")
      }

      # Ensure max and min have cst weight
      if (param_agg %in% c("min","max")) {
        param_w <- "cst"
      }

      # Same mapping input-data, do nothing
      if (input_reg_id != reg_id) {

        cat(crayon::blue(paste0(" - ", item_type, " ", item, " [agg: ", param_agg, ", wgt: ", param_w, "]\n")))

        .conv <- convert_region(.data,
                                from_reg = input_reg_id,
                                to_reg = reg_id,
                                agg_operator = param_agg,
                                agg_weight = param_w,
                                region_mappings,
                                weights,
                                missing_values)

        .data <- .conv[['data']]
        .info_share <- .conv[['info']]

      } else {

        cat(crayon::blue(paste(" -", item_type, item, "[same]\n")))

        # Change the region column name and indices
        data.table::setnames(.data, reg_id, "n")

      }

      # Update indices
      data_indices[data_indices == input_reg_id] <- "n"

    } else {

      cat(crayon::blue(paste(" -", item_type, item, "\n")))

    }

    # Ensure the order is kept
    data.table::setcolorder(.data,data_indices)

    # Mask indices with dummy names with stars, to be understood by GAMS
    if (length(colnames(.data)) == 1) {
      names(.data) <- "value"
    } else {
      indices <- subset(colnames(.data), colnames(.data) != "value")
      indices <- ifelse(indices %in% c("n","t"), indices, "*")
      names(.data) <- c(indices,"value")
    }

    # add to collector []
    attributes(.data) <- c(attributes(.data),gams = text)

    # add warnings if data contains NAs
    if (nrow(.data) > 0 & !item %in% c("carbonprice")) {
      if (anyNA(.data$value)) {
        warning(paste(gdxfile,'-',item,'contains NAs.'))
      }
    }

    .i <- list(.data)
    names(.i) <- item

    if (item_type == "parameter") params <- c(params, .i)
    if (item_type == "variable") vars <- c(vars, .i)

    # add an additionnal parameter '_info' when using sumby
    if (param_agg %in% c("sumby")) {
      if ("year" %in% colnames(.info_share)) {
        .info_share[,year := as.numeric(year)]
        .info_share <- merge(.info_share, time_mappings[[time_id]][,.(t,year)],
                             by = "year", allow.cartesian = TRUE)
        .info_share[, year := NULL]

        # Take the average value over time range
        .info_share <- .info_share[, .(value = mean(value,na.rm = TRUE)),
                                   by = c(colnames(.info_share)[!colnames(.info_share) %in% c('value')])]
        .info_share[is.nan(value),value := NA]

      }
      data.table::setcolorder(.info_share,data_indices)
      names(.info_share) <- c(indices,"value")
      .i <- list(.info_share)
      names(.i) <- paste0(item,'_info')
      params <- c(params, .i)
    }

  }

  cat(crayon::blue(paste(" -","writing gdx\n")))

  f <- file.path(output_directory,basename(gdxfile))

  gdxtools::write.gdx(f, params = params, vars_l = vars)

  return(gdxfile)
}
