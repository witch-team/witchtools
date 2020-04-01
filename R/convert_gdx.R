#! Batch convert parameters and variables in a GDX
#'
#' \code{convert_gdx} writes a converted GDX in the \code{output_directory}.
#' All parameters and variables from the input \code{gdxfile} are converted
#' using the \code{convert_table} function. Specific conversion options
#' are read in the parameter \code{meta_param} also stored in the gdxfile.
#'
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}} for single data.table.
#'
#' @param gdxfile location of the GDX file.
#' @param reg_id final regional aggregation.
#' @param time_id final time_period aggregation.
#' @param region_mappings a named list of region mapping data.table.
#' @param time_mappings a named list of time mapping data.table.
#' @param weights a named list of weights used by \code{convert_region}
#' @param output_directory directory where to write the converted GDX
#' @param guess_input_n input regional mapping if not explicitely defined
#' @param guess_input_t input time mapping if not explicitely defined
#' @param default_agg_missing default parameter to handle missing values in
#' \code{convert_region}
#' @param default_meta_param default meta_param
#' @export
#' @examples
#' \dontrun{
#' convert_gdx('input/build/data_climate.gdx','witch17','t30','data_witch17')
#' }
#'

convert_gdx <- function(gdxfile,
                        reg_id,
                        time_id,
                        output_directory,
                        region_mappings = region_mappings,
                        time_mappings = time_mappings,
                        weights = default_weights,
                        guess_input_n = "witch17",
                        guess_input_t = "t30",
                        default_agg_missing = "zero",
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

    item_param <- meta_param[parameter == item]

    cat(crayon::blue(paste(" -",item_type,item,"\n")))

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

    convpar <- list()

    # Time period conversion
    do_time_period <- ("year" %in% colnames(.data) &
                         !stringr::str_detect(basename(gdxfile),"hist"))

    if (do_time_period) {
      data_indices[data_indices == "year"] <- "t"
    }

    # Region conversion
    data_reg <- intersect(colnames(.data), names(region_mappings))
    do_region <- FALSE
    # deal with case when data_reg == character(0)
    if (length(data_reg) == 1) {
      do_region <- (data_reg != reg_id)
      data_indices[data_indices == data_reg] <- "n"
    }
    from_reg <- NULL
    to_reg <- NULL

    if (do_region) {

      # Ensure region is lower case / iso3 is upper case
      if (data_reg != "iso3") {
        .idx <- which(data_reg == colnames(.data))
        .data[[.idx]] <- tolower(.data[,get(data_reg)])
      } else {
        .data$iso3 <- toupper(.data$iso3)
      }

      # Set initial regional mapping
      if (data_reg != "iso3") {
        from_reg <- region_mappings[[data_reg]]
      } else {
        from_reg <- 'iso3'
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
                 do_past_extrap = TRUE)

    if (!convopt[['do_extrap']]) {
      convopt[['do_past_extrap']] <- FALSE
    }

    # No inter/extrapolation for stochastic branch
    if (stringr::str_detect(time_id, "branch")) {
      convopt[['do_extrap']] <- FALSE
      convopt[['do_interp']] <- FALSE
    }

    # Missing values
    convopt[['agg_missing']] <- default_agg_missing
    if (nrow(item_param[type == "missing_values"]) > 0) {
      convopt[['agg_missing']] <- item_param[type == "missing_values"][1,value]
    }

    # Aggregation operator
    convopt[['agg_operator']] <- "sum"
    if (nrow(item_param[type == "nagg"]) > 0) {
      convopt[['agg_operator']] <- item_param[type == "nagg"][1,value]
    }

    # Aggregation weight
    nweight <- "pop"
    if (nrow(item_param[type == "nweight"]) > 0) {
      nweight <- item_param[type == "nweight"][1,value]
    } else {
      nweight <- ifelse(convopt[['agg_operator']] %in% c("mean"),"cst","gdp")
    }
    # Ensure max and min have cst weight
    if (convopt[['agg_operator']] %in% c("min","max")) {
      nweight <- "cst"
    }

    if (!nweight %in% names(weights)) {
      stop(paste(nweight,'not in weights!'))
    }

    .conv <- convert_table(.data,
                            time_mapping = time_mappings[[time_id]],
                            from_reg = from_reg,
                            to_reg = region_mappings[[reg_id]],
                            agg_weight = weights[[nweight]],
                            options = convopt,
                            do_time_period = do_time_period,
                            do_region = do_region,
                            verbose = TRUE)

    .data <- .conv[['data']]
    .info_share <- .conv[['info']]

    # rename reg_id in necessary (when no region conversion is required)
    if (reg_id %in% colnames(.data)) {
      cat(crayon::blue(paste("   [same]\n")))
      data.table::setnames(.data, reg_id, "n")
    } else if (do_region) {
      cat(crayon::blue(paste0("   [agg: ", convopt[['agg_operator']], ", wgt: ",
                              nweight, "]\n")))
    }

    # Ensure the original order is kept
    data.table::setcolorder(.data,data_indices)

    # Table indices are stars, except from t and n
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
    if (nweight %in% c("sumby")) {
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
