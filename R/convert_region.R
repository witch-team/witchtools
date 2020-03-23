

convert_region <- function(.x,
                           input_reg_id,reg_id,
                           region_mappings,
                           weights,
                           param_agg,param_w,
                           missing_values) {

  # Same mapping input-data, do nothing
  if (input_reg_id == reg_id) return(.x)

  # region_mappings should contain input_reg_id or input_reg_id unless 'iso3'
  if (!input_reg_id %in% c('iso3',names(region_mappings))) {
    stop(paste0('region_mappings should contains ', input_reg_id,'.'))
  }
  if (!reg_id %in% c('iso3',names(region_mappings))) {
    stop(paste0('region_mappings should contains ', reg_id,'.'))
  }

  # Add iso3 and data_reg mapping
  if (input_reg_id != "iso3") {
    .r <- merge(region_mappings[[input_reg_id]],region_mappings[[reg_id]],
                by = "iso3")
    .x <- merge(.x, .r, by = input_reg_id, allow.cartesian = TRUE)
  } else {
    .x <- merge(.x, region_mappings[[reg_id]], by = "iso3")
  }

  # Add weight
  .x <- merge(.x, weights[[param_w]], by = "iso3")
  .x <- .x[!is.na(get(reg_id))]

  dkeys <- function(dd){
    return(c(colnames(dd)[!colnames(dd) %in% c("value","weight","sum_weight",
                                                 names(region_mappings))]))
  }

  # Disaggregation
  if (input_reg_id != "iso3") {
    if (param_agg %in% c("sum","sumby")) {
      # total weights are computed because of missing zeros values
      .w <- merge(region_mappings[[input_reg_id]],weights[[param_w]],
                  by = "iso3")
      .w <- .w[iso3 %in% unique(.x$iso3)]
      .w <- .w[,.(sum_weight = sum(weight)),
               by = input_reg_id]
      .x <- merge(.x,.w,by = input_reg_id)
      .x <- .x[, .(iso3,reg_id = get(reg_id),
                         value = value * weight / sum_weight),
                     by = c(dkeys(.x),input_reg_id) ]
    } else {
      if (param_agg %in% c("mean","set1","min","minw","max","maxw")) {
        .x <- .x[, .(iso3,reg_id = get(reg_id),
                           value = value,weight),
                       by = c(dkeys(.x),input_reg_id) ]
      } else {
        stop(paste("Disaggregation",param_agg,"not implemented"))
      }
    }
  } else {
    data.table::setnames(.x, reg_id, "reg_id")
  }

  # informed share
  if (param_agg %in% c("sumby")) {
    .w <- merge(region_mappings[[reg_id]],weights[[param_w]],by = "iso3")
    .w <- .w[,.(sum_weight = sum(weight)),by = reg_id]
    data.table::setnames(.w, reg_id, "reg_id")
    .x <- merge(.x,.w,by = "reg_id")
    .info_share <- .x[,.(value = sum(weight) / mean(sum_weight)),
                         by = c(dkeys(.x))]
    data.table::setnames(.info_share, "reg_id", "n")
  }

  # Aggregation
  if (param_agg %in% c("sum","sumby")) {
    .x <- .x[, .(value = sum(value)), by = c(dkeys(.x)) ]
  } else {
    .w <- merge(region_mappings[[reg_id]],weights[[param_w]],by = "iso3")
    .w <- .w[,.(sum_weight = sum(weight)),by = reg_id]
    data.table::setnames(.w, reg_id, "reg_id")
    .x <- merge(.x,.w,by = "reg_id")
    if (param_agg == "mean") {
      if (missing_values == "zero") {
        .x <- .x[, .(value = sum(value * weight / sum_weight)),
                       by = c(dkeys(.x)) ]
      }
      if (missing_values == "NA") {
        .x <- .x[, .(value = sum(value * weight / sum(weight))),
                       by = c(dkeys(.x)) ]
      }
    } else if (param_agg == "set1") {
      if (missing_values == "zero") {
        .x <- .x[, .(value = round(sum(value * weight / sum_weight))),
                       by = c(dkeys(.x)) ]
      }
      if (missing_values == "NA") {
        .x <- .x[, .(value = round(sum(value * weight / sum(weight)))),
                       by = c(dkeys(.x)) ]
      }
    } else if (param_agg %in% c("min","minw")) {
      .x <- .x[, .(value = min(value[which(weight == min(weight))])),
                     by = c(dkeys(.x)) ]
    } else if (param_agg %in% c("max","maxw")) {
      .x <- .x[, .(value = max(value[which(weight == max(weight))])),
                     by = c(dkeys(.x)) ]
    }  else {
      stop(paste("aggregation",param_agg,"not implemented"))
    }
  }

  if (param_agg %in% c("sumby")) {
    return(list(.x,.info_share))
  } else {
    return(.x)
  }

}
