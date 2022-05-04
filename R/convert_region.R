#' Convert a data.table from a regional mapping to another.
#'
#' \code{convert_region} returns a list containing a data.table where values are
#' converted from one regional mapping to another. The conversion is done by
#' downscaling values at country-level (if necessary) and updscaling them
#' into the final regional mapping. The list also contain an
#' optional data.table about the value coperture for the operator \code{sumby}.
#' The required format of the input data is described in Details.
#'
#' The input data.table should contain a column "value" and a column named by
#' the initial regional mapping \code{from_reg},
#' while the other columns are are considered as id columns.
#' The resulting data.table have a column named by the final regional
#' mapping \code{to_reg}.
#'
#' The regional mapping for \code{from_reg} and \code{to_reg} can be
#' provided in a named list through the parameter \code{regions}.
#' Regional mappings are 2-columns data.table with a column named 'iso3'
#' (for country ISO3)
#' and another one named as the regional mapping (for region name).
#' The name in the list should also be the regional mapping name.
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}},
#' \code{\link{convert_gdx}}.
#'
#'
#' @param .x a well-formatted data.table.
#' @param from_reg initial regional mapping name or a data.table with
#' the mapping.
#' @param to_reg final regional mapping name  or a data.table with the mapping.
#' @param agg_operator aggregation operator (See Details for the list of
#' possible values)
#' @param agg_weight aggregation weight data.table (See Details for the list of
#' possible values)
#' @param agg_missing tells how to deal with missing values ("NA" or "zero")
#' @param regions optional list of region mappings (see Details for format)
#' @param info logical indicating whether to include information, only required
#'  for the agg_operator "sumby".
#'
#' @return a list containing a converted data.table and information about
#'         the coperture if available.
#' @export
#' @examples
#' \dontrun{
#'
#' convert_region(gdp_iso3, to_reg = "witch17")
#' }
#'
convert_region <- function(.x,
                           from_reg = NULL,
                           to_reg,
                           agg_operator = "sum",
                           agg_weight = witchtools::default_weights[["gdp"]],
                           agg_missing = "NA",
                           regions = witchtools::region_mappings,
                           info = FALSE) {
  if (!data.table::is.data.table(.x)) .x <- data.table::setDT(.x)

  # Check weight
  if (is.null(agg_weight)) {
    stop("agg_weight is NULL. Check if it is well-defined.")
  }

  # Guess initial region from column names
  if (is.null(from_reg)) {
    guess_reg <- intersect(colnames(.x), unique(c("iso3", names(regions))))
    if (length(guess_reg) == 1) {
      from_reg <- guess_reg
    }
  }

  # Initial mapping
  if (is.character(from_reg)) {
    if (!from_reg %in% c("iso3", names(regions))) {
      stop(paste0("regions should contains the name ", from_reg, "."))
    }
    rmap0 <- regions[[from_reg]]
    rname0 <- from_reg
  } else if (data.table::is.data.table(from_reg)) {
    rmap0 <- from_reg
    rname0 <- region_id(rmap0)
  } else {
    stop(paste0("from_reg should be a character or a data.table."))
  }

  # Final mapping
  if (is.character(to_reg)) {
    if (!to_reg %in% c("iso3", names(regions))) {
      stop(paste0("regions should contains the name ", to_reg, "."))
    }
    rmap1 <- regions[[to_reg]]
    rname1 <- to_reg
  } else if (data.table::is.data.table(to_reg)) {
    rmap1 <- to_reg
    rname1 <- region_id(rmap1)
  } else {
    stop(paste0("to_reg should be a character or a data.table."))
  }

  # Same mappings input-output, do nothing
  if (rname0 == rname1) {
    return(.x)
  }

  # "sumby" might need info
  if (agg_operator == "sumby" & !info) {
    warning(paste0("Operator sumby might need info = TRUE."))
  }

  # Not yet implemented
  if (rname1 == "iso3") {
    stop(paste0("to_reg == iso3 is not yet implemented."))
  }

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
        .x <- .x[, .(value = sum(value * weight / sum(weight))),
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
