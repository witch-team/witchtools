# Main function to convert GDX
#' @importFrom stats approx

convert_gdx <- function(gdxfile,
                        reg_id, time_id,
                        region_mappings, region_definitions,
                        time_mappings, weights,
                        output_directory,
                        guess_input_n = "witch17", guess_input_t = "t30",
                        default_missing_values = "zero",
                        default_meta_param = witch_default_meta_param()){

  if (!file.exists(gdxfile)) stop(paste(gdxfile, "does not exist!"))

  cat(crayon::blue$bold(paste("Processing", basename(gdxfile),"\n")))

  #define GDX file
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
    attributes(.data) <- c(attributes(.data), name = item)

    if ("n" %in% colnames(.data)) {
      data.table::setnames(.data, "n", guess_input_n)
    }
    if ("t" %in% colnames(.data) & guess_input_t == "t30") {
      data.table::setnames(.data, "t", "year")
      .data[,year := paste(as.numeric(.data$year) * 5 + 2000)]
    }

    data_indices <- colnames(.data)

    # Time conversion
    # TODO add flag skip_timescale
    if ("year" %in% colnames(.data) & !stringr::str_detect(basename(gdxfile),"hist")) {

      # Check if skip extrapolation
      do_extrap <- nrow(meta_param[parameter == item & type == "extrap"]) > 0
      do_interp <- nrow(meta_param[parameter == item & type == "interp"]) > 0

      # no inter/extrapolation for stochastic branch
      if (stringr::str_detect(time_id, "branch")) do_extrap <- do_interp <- FALSE

      time_mapping = time_mappings[[time_id]]

      .data <- convert_time_period(.data, time_mapping, do_extrap, do_interp)

      # Update indices
      data_indices[data_indices == "year"] <- "t"

    }

    # Region conversion
    input_reg_id <- intersect(colnames(.data), names(region_definitions))
    param_agg <- ""

    # Region has been identified?
    if (length(input_reg_id) == 1) {

      # Detect the type of missing value
      missing_values <- default_missing_values
      if (nrow(meta_param[parameter == item & type == "missing_values"]) > 0) {
        missing_values <- meta_param[parameter == item & type == "missing_values"][1,value]
      }

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

        # Add iso3 and data_reg mapping
        if (input_reg_id != "iso3") {
          .r <- merge(region_mappings[[input_reg_id]],region_mappings[[reg_id]], by = "iso3")
          .data <- merge(.data, .r, by = input_reg_id, allow.cartesian = TRUE)
        } else {
          .data <- merge(.data, region_mappings[[reg_id]], by = "iso3")
        }

        cat(crayon::blue(paste0(" - ", item_type, " ", item, " [agg: ", param_agg, ", wgt: ", param_w, "]\n")))

        # Add weight
        .data <- merge(.data, weights[[param_w]], by = "iso3")
        .data <- .data[!is.na(get(reg_id))]

        dkeys <- function(dd){
          return(c(colnames(dd)[!colnames(dd) %in% c("value","weight","sum_weight",names(region_definitions))]))
        }

        # Disaggregation
        if (input_reg_id != "iso3") {
          if (param_agg %in% c("sum","sumby")) {
            # total weights are computed because of missing zeros values
            .w <- merge(region_mappings[[input_reg_id]],weights[[param_w]],by = "iso3")
            .w <- .w[iso3 %in% unique(.data$iso3)]
            .w <- .w[,.(sum_weight = sum(weight)),by = input_reg_id]
            .data <- merge(.data,.w,by = input_reg_id)
            .data <- .data[, .(iso3,reg_id = get(reg_id),value = value * weight / sum_weight), by = c(dkeys(.data),input_reg_id) ]
          } else {
            if (param_agg %in% c("mean","set1","min","minw","max","maxw")) {
              .data <- .data[, .(iso3,reg_id = get(reg_id),value = value,weight), by = c(dkeys(.data),input_reg_id) ]
            } else {
              stop(paste("Disaggregation",param_agg,"not implemented"))
            }
          }
        } else {
          data.table::setnames(.data, reg_id, "reg_id")
        }

        # informed share
        if (param_agg %in% c("sumby")) {
          .w <- merge(region_mappings[[reg_id]],weights[[param_w]],by = "iso3")
          .w <- .w[,.(sum_weight = sum(weight)),by = reg_id]
          data.table::setnames(.w, reg_id, "reg_id")
          .data <- merge(.data,.w,by = "reg_id")
          .info_share <- .data[,.(value = sum(weight) / mean(sum_weight)),by = c(dkeys(.data))]
          data.table::setnames(.info_share, "reg_id", "n")
        }

        # Aggregation
        if (param_agg %in% c("sum","sumby")) {
          .data <- .data[, .(value = sum(value)), by = c(dkeys(.data)) ]
        } else {
          .w <- merge(region_mappings[[reg_id]],weights[[param_w]],by = "iso3")
          .w <- .w[,.(sum_weight = sum(weight)),by = reg_id]
          data.table::setnames(.w, reg_id, "reg_id")
          .data <- merge(.data,.w,by = "reg_id")
          if (param_agg == "mean") {
            if (missing_values == "zero") {
              .data <- .data[, .(value = sum(value * weight / sum_weight)), by = c(dkeys(.data)) ]
            }
            if (missing_values == "NA") {
              .data <- .data[, .(value = sum(value * weight / sum(weight))), by = c(dkeys(.data)) ]
            }
          } else if (param_agg == "set1") {
            if (missing_values == "zero") {
              .data <- .data[, .(value = round(sum(value * weight / sum_weight))), by = c(dkeys(.data)) ]
            }
            if (missing_values == "NA") {
              .data <- .data[, .(value = round(sum(value * weight / sum(weight)))), by = c(dkeys(.data)) ]
            }
          } else if (param_agg %in% c("min","minw")) {
            .data <- .data[, .(value = min(value[which(weight == min(weight))])), by = c(dkeys(.data)) ]
          } else if (param_agg %in% c("max","maxw")) {
            .data <- .data[, .(value = max(value[which(weight == max(weight))])), by = c(dkeys(.data)) ]
          }  else {
            stop(paste("aggregation",param_agg,"not implemented"))
          }
        }

        # Change the region column name
        data.table::setnames(.data, "reg_id", "n")

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
        .info_share <- merge(.info_share, time_mappings[[time_id]][,.(t,year)], by = "year", allow.cartesian = TRUE)
        .info_share[, year := NULL]

        # Take the average value over time range
        .info_share <- .info_share[, .(value = mean(value,na.rm = TRUE)), by = c(colnames(.info_share)[!colnames(.info_share) %in% c('value')])]
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
