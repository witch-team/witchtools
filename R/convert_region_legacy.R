# Legacy region-conversion engine.
#
# This is the original convert_region() core, moved verbatim from
# R/convert_region.R. It downscales the table to iso3 country level
# (exploding each region row to all its member countries) and aggregates back
# into the target mapping. It is kept as the reference implementation: the
# equivalence tests compare the fast pair-coefficient engine against it, and
# it remains reachable via options(witchtools.convert_region_engine = "legacy").
#
# Arguments are the already-resolved pieces computed by convert_region():
# the input table, both mapping tables and their names, and the aggregation
# settings. Validation and the pass-through shortcut live in the dispatcher.

#' @noRd
convert_region_via_iso3 <- function(.x,
                                    rmap0, rname0,
                                    rmap1, rname1,
                                    agg_operator,
                                    agg_weight,
                                    agg_missing,
                                    info) {

  iso3 <- weight <- sum_weight <- value <- NULL # due to NSE notes in R CMD check

  # Add iso3 and data_reg mapping
  if (rname0 == "iso3") {
    .x <- merge(.x, rmap1, by = "iso3")
  } else {
    .r <- merge(rmap0, rmap1, by = "iso3")
    .x <- merge(.x, .r, by = rname0, allow.cartesian = TRUE)
  }

  # Add weight
  .x <- merge(.x, agg_weight, by = "iso3")
  .x <- .x[!is.na(get(rname1))]

  dkeys <- function(dd) {
    return(c(colnames(dd)[!colnames(dd) %in% c(
      "value",
      "weight", "sum_weight",
      "iso3", rname0, rname1
    )]))
  }

  # Disaggregation
  if (rname0 != "iso3") {
    if (agg_operator %in% c("sum","sumby")) {
      # total weights are computed because of missing zeros values
      .w <- merge(rmap0, agg_weight, by = "iso3")
      .w <- .w[iso3 %in% unique(.x$iso3)]
      .w <- .w[, list(sum_weight = sum(weight)), by = rname0]
      .x <- merge(.x, .w, by = rname0)
      .x <- .x[, list(iso3,
        rname1 = get(rname1),
        value = value * weight / sum_weight,
        weight
      ),
      by = c(dkeys(.x), rname0)
      ]
      if (agg_operator %in% c("sum")) {
        .x[, weight := NULL]
      }
    } else {
      if (agg_operator %in% c("mean", "set1", "min", "minw", "max", "maxw")) {
        .x <- .x[, .(iso3,
          rname1 = get(rname1),
          value,
          weight
        ),
        by = c(dkeys(.x), rname0)
        ]
      } else {
        stop(paste("Operator ", agg_operator, "not implemented"))
      }
    }
  } else {
    data.table::setnames(.x, rname1, "rname1")
  }

  # informed share
  .info_share <- NULL
  if (agg_operator %in% c("sumby")) {
    .w <- merge(rmap1, agg_weight, by = "iso3")
    .w <- .w[, .(sum_weight = sum(weight)), by = rname1]
    data.table::setnames(.w, rname1, "rname1")
    .x <- merge(.x, .w, by = "rname1")
    .info_share <- .x[, .(value = sum(weight) / mean(sum_weight)),
      by = c(dkeys(.x))
    ]
    data.table::setnames(.info_share, "rname1", rname1)
  }

  # Aggregation
  if (agg_operator %in% c("sum", "sumby")) {
    .x <- .x[, .(value = sum(value)), by = c(dkeys(.x))]
  } else {
    .w <- merge(rmap1, agg_weight, by = "iso3")
    .w <- .w[, .(sum_weight = sum(weight)), by = rname1]
    data.table::setnames(.w, rname1, "rname1")
    .x <- merge(.x, .w, by = "rname1")
    if (agg_operator == "mean") {
      if (agg_missing == "zero") {
        .x <- .x[, .(value = sum(value * weight / sum_weight)),
          by = c(dkeys(.x))
        ]
      }
      if (agg_missing == "NA") {
        .x <- .x[!is.na(value), .(value = sum(value * weight / sum(weight))),
          by = c(dkeys(.x))
        ]
      }
    } else if (agg_operator == "set1") {
      if (agg_missing == "zero") {
        .x <- .x[, .(value = round(sum(value * weight / sum_weight))),
          by = c(dkeys(.x))
        ]
      }
      if (agg_missing == "NA") {
        .x <- .x[, .(value = round(sum(value * weight / sum(weight)))),
          by = c(dkeys(.x))
        ]
      }
    } else if (agg_operator %in% c("min", "minw")) {
      .x <- .x[, .(value = min(value[which(weight == min(weight))])),
        by = c(dkeys(.x))
      ]
    } else if (agg_operator %in% c("max", "maxw")) {
      .x <- .x[, .(value = max(value[which(weight == max(weight))])),
        by = c(dkeys(.x))
      ]
    } else {
      stop(paste("Operator", agg_operator, "not implemented"))
    }
  }

  # Change the region column name
  data.table::setnames(.x, "rname1", rname1)

  if (info) {
    return(list(data = .x, info = .info_share))
  } else {
    return(.x)
  }
}
