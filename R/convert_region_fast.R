# Fast region-conversion engine.
#
# The legacy engine downscales the table to iso3 country level (~x15 rows for
# witch17 input) and re-aggregates. But the downscale->upscale is a linear
# map: for every operator the result only depends on per-(from, to) pair
# aggregates of the country weights. This engine precomputes a tiny pair
# coefficient table (21 rows for witch17->witch20) and converts with a single
# join (~x1.2 rows) and one grouped aggregation, instead of the iso3 explosion
# and its four full-size intermediate copies.
#
# Numerical results match the legacy engine up to floating-point summation
# order (tolerance ~1e-12, covered by the equivalence tests in
# tests/testthat/test-convert-region-equivalence.R).

# Build the per-(from, to) coefficient table.
#
# Column semantics (mirroring the legacy engine's intermediate quantities):
#   w_pair   sum of country weights in (from n to)      [legacy :141 join]
#   w_min/w_max  min/max country weight in the pair     [for min*/max* ops]
#   iso3_min smallest iso3 of the pair                  [row-order emulation]
#   sw_from  sum of w_pair by from-region               [legacy :156-158]
#   sw_to    sum of weights over the FULL to-mapping    [legacy :190/:204]
#
# The legacy sum denominator filters countries to those present in the data
# (`iso3 %in% unique(.x$iso3)`), but since the explosion expands every present
# region to all its member countries and denominators are grouped by
# from-region, the filter is data-independent: sw_from computed here is
# identical for every from-region that appears in the data at all.
#' @noRd
build_region_coeff <- function(rmap0, rname0, rmap1, rname1, agg_weight) {
  iso3 <- weight <- w_pair <- .from <- NULL # due to NSE notes in R CMD check

  pw <- merge(rmap0, rmap1, by = "iso3")
  pw <- merge(pw, agg_weight, by = "iso3")
  pw <- pw[!is.na(get(rname1))]
  cf <- pw[, list(
    w_pair = sum(weight),
    w_min = min(weight),
    w_max = max(weight),
    iso3_min = min(iso3)
  ), by = c(rname0, rname1)]
  data.table::setnames(cf, c(rname0, rname1), c(".from", ".to"))
  cf[, sw_from := sum(w_pair), by = .from]
  sw_to <- merge(rmap1, agg_weight, by = "iso3")
  sw_to <- sw_to[, list(sw_to = sum(weight)), by = rname1]
  data.table::setnames(sw_to, rname1, ".to")
  cf <- merge(cf, sw_to, by = ".to")
  return(cf)
}

#' @noRd
convert_region_fast <- function(.x,
                                rmap0, rname0,
                                rmap1, rname1,
                                agg_operator,
                                agg_weight,
                                agg_missing,
                                info) {

  value <- w_pair <- sw_from <- sw_to <- NULL # due to NSE notes in R CMD check
  .row0 <- .from <- .to <- iso3_min <- w_min <- w_max <- gw <- NULL

  known_ops <- c("sum", "sumby", "mean", "min", "minw", "max", "maxw")
  if (!agg_operator %in% known_ops) {
    if (agg_operator == "set1") {
      # Dispatched to the legacy engine by convert_region(): round() is
      # discontinuous, so reassociated summation could flip .5 boundaries.
      stop("set1 is handled by the legacy engine.")
    }
    # Same message and trigger point as the legacy disaggregation branch.
    stop(paste("Operator ", agg_operator, "not implemented"))
  }

  cf <- build_region_coeff(rmap0, rname0, rmap1, rname1, agg_weight)

  idcols <- setdiff(names(.x), c(rname0, "value"))

  # Transient input-row tag for row-order emulation; removed on exit so the
  # caller's table is left untouched even on error.
  .x[, .row0 := .I]
  on.exit(
    if (".row0" %in% names(.x)) .x[, .row0 := NULL],
    add = TRUE
  )

  # One small join instead of the iso3 explosion. nomatch=NULL drops input
  # rows whose region is absent from the crosswalk/weights, as the legacy
  # inner merges do.
  xt <- cf[.x, on = c(".from" = rname0), allow.cartesian = TRUE, nomatch = NULL]

  if (agg_operator %in% c("sum", "sumby")) {

    # Legacy row order: groups appear in (from-region, input-row) order for
    # sum, with an extra leading to-region sort for sumby (its merge at :193).
    if (agg_operator == "sum") {
      data.table::setorder(xt, .from, .row0, iso3_min)
      out <- xt[, list(value = sum(value * w_pair / sw_from)),
        by = c(idcols, ".to")
      ]
      data.table::setnames(out, ".to", rname1)
      data.table::setcolorder(out, c(idcols, rname1, "value"))
      if (info) {
        return(list(data = out, info = NULL))
      }
      return(out)
    }

    data.table::setorder(xt, .to, .from, .row0, iso3_min)
    out <- xt[, list(value = sum(value * w_pair / sw_from)),
      by = c(".to", idcols)
    ]
    data.table::setnames(out, ".to", rname1)
    data.table::setcolorder(out, c(rname1, idcols, "value"))

    .info_share <- NULL
    .info_share <- xt[, list(value = sum(w_pair / sw_to)),
      by = c(".to", idcols)
    ]
    data.table::setnames(.info_share, ".to", rname1)
    data.table::setcolorder(.info_share, c(rname1, idcols, "value"))

    if (info) {
      return(list(data = out, info = .info_share))
    }
    return(out)
  }

  # mean / set1 / min* / max* : legacy replicates values to countries and
  # aggregates with the full to-mapping weight sums (sw_to).
  data.table::setorder(xt, .to, .from, .row0, iso3_min)

  if (agg_operator == "mean") {
    if (agg_missing == "zero") {
      out <- xt[, list(value = sum(value * w_pair) / sw_to[1L]),
        by = c(".to", idcols)
      ]
    } else {
      # agg_missing == "NA": legacy filters NA rows before aggregating, so
      # groups whose contributions are all NA disappear from the output.
      out <- xt[!is.na(value),
        list(value = sum(value * w_pair) / sum(w_pair)),
        by = c(".to", idcols)
      ]
    }
  } else if (agg_operator %in% c("min", "minw")) {
    # Legacy: min(value[weight == min(weight)]) over country rows; replicated
    # values make this expressible with the per-pair minimum weight.
    xt[, gw := min(w_min), by = c(".to", idcols)]
    out <- xt[w_min == gw, list(value = min(value)), by = c(".to", idcols)]
  } else {
    xt[, gw := max(w_max), by = c(".to", idcols)]
    out <- xt[w_max == gw, list(value = max(value)), by = c(".to", idcols)]
  }

  data.table::setnames(out, ".to", rname1)
  data.table::setcolorder(out, c(rname1, idcols, "value"))

  if (info) {
    return(list(data = out, info = NULL))
  }
  return(out)
}
