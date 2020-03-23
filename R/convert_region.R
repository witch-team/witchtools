

convert_region <- function(.x,from_reg,to_reg,
                           agg_operator = "sum",
                           agg_weight = "gdp",
                           region_mappings,
                           weights = default_weights,
                           missing_values = "NA") {

  # Same mapping input-data, do nothing
  if (from_reg == to_reg) return(.x)

  # region_mappings should contain from_reg or to_reg unless 'iso3'
  if (!from_reg %in% c('iso3',names(region_mappings))) {
    stop(paste0('region_mappings should contains ', from_reg,'.'))
  }
  if (!to_reg %in% c('iso3',names(region_mappings))) {
    stop(paste0('region_mappings should contains ', to_reg,'.'))
  }

  # "sumby" requires from_reg="iso3
  if (agg_operator == "sumby" & from_reg != "iso3") {
    stop(paste0('Operator sumby requires from_reg == iso3.'))
  }

  # Not yet implemented
  if (to_reg == "iso3") {
    stop(paste0('to_reg == iso3 is not yet implemented.'))
  }

  # Add iso3 and data_reg mapping
  if (from_reg != "iso3") {
    .r <- merge(region_mappings[[from_reg]],region_mappings[[to_reg]],
                by = "iso3")
    .x <- merge(.x, .r, by = from_reg, allow.cartesian = TRUE)
  } else {
    .x <- merge(.x, region_mappings[[to_reg]], by = "iso3")
  }

  # Add weight
  .x <- merge(.x, weights[[agg_weight]], by = "iso3")
  .x <- .x[!is.na(get(to_reg))]

  dkeys <- function(dd){
    return(c(colnames(dd)[!colnames(dd) %in% c("value","weight","sum_weight",
                                               "iso3",from_reg,to_reg)]))
  }

  # Disaggregation
  if (from_reg != "iso3") {
    if (agg_operator %in% c("sum")) {
      # total weights are computed because of missing zeros values
      .w <- merge(region_mappings[[from_reg]],weights[[agg_weight]],
                  by = "iso3")
      .w <- .w[iso3 %in% unique(.x$iso3)]
      .w <- .w[,.(sum_weight = sum(weight)),
               by = from_reg]
      .x <- merge(.x,.w,by = from_reg)
      .x <- .x[, .(iso3,to_reg = get(to_reg),
                         value = value * weight / sum_weight),
                     by = c(dkeys(.x),from_reg) ]
    } else {
      if (agg_operator %in% c("mean","set1","min","minw","max","maxw")) {
        .x <- .x[, .(iso3, to_reg = get(to_reg),
                     value = value, weight),
                by = c(dkeys(.x),from_reg) ]
      } else {
        stop(paste("Operator ",agg_operator,"not implemented"))
      }
    }
  } else {
    data.table::setnames(.x, to_reg, "to_reg")
  }

  # informed share
  .info_share <- NULL
  if (agg_operator %in% c("sumby")) {
    .w <- merge(region_mappings[[to_reg]],weights[[agg_weight]],by = "iso3")
    .w <- .w[,.(sum_weight = sum(weight)),by = to_reg]
    data.table::setnames(.w, to_reg, "to_reg")
    .x <- merge(.x,.w,by = "to_reg")
    .info_share <- .x[,.(value = sum(weight) / mean(sum_weight)),
                    by = c(dkeys(.x))]
    data.table::setnames(.info_share, "to_reg", "n")
  }

  # Aggregation
  if (agg_operator %in% c("sum","sumby")) {
    .x <- .x[, .(value = sum(value)), by = c(dkeys(.x)) ]
  } else {
    .w <- merge(region_mappings[[to_reg]],weights[[agg_weight]],by = "iso3")
    .w <- .w[,.(sum_weight = sum(weight)),by = to_reg]
    data.table::setnames(.w, to_reg, "to_reg")
    .x <- merge(.x,.w,by = "to_reg")
    if (agg_operator == "mean") {
      if (missing_values == "zero") {
        .x <- .x[, .(value = sum(value * weight / sum_weight)),
                       by = c(dkeys(.x)) ]
      }
      if (missing_values == "NA") {
        .x <- .x[, .(value = sum(value * weight / sum(weight))),
                       by = c(dkeys(.x)) ]
      }
    } else if (agg_operator == "set1") {
      if (missing_values == "zero") {
        .x <- .x[, .(value = round(sum(value * weight / sum_weight))),
                       by = c(dkeys(.x)) ]
      }
      if (missing_values == "NA") {
        .x <- .x[, .(value = round(sum(value * weight / sum(weight)))),
                       by = c(dkeys(.x)) ]
      }
    } else if (agg_operator %in% c("min","minw")) {
      .x <- .x[, .(value = min(value[which(weight == min(weight))])),
                     by = c(dkeys(.x)) ]
    } else if (agg_operator %in% c("max","maxw")) {
      .x <- .x[, .(value = max(value[which(weight == max(weight))])),
                     by = c(dkeys(.x)) ]
    }  else {
      stop(paste("Operator",agg_operator,"not implemented"))
    }
  }

  # Change the region column name
  data.table::setnames(.x, "to_reg", "n")

  return(list(data = .x, info = .info_share))

}
