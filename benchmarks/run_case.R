#!/usr/bin/env Rscript
"Run one region-conversion benchmark cell.

Usage:
  run_case.R --engine=<e> --case=<c> --out=<csv> [--rows=<n>] [--op=<op>] [--route=<r>] [--to=<t>] [--table=<tab>] [--db=<db>] [--seed=<s>]

Options:
  --engine=<e>   Conversion engine: fast or legacy.
  --case=<c>     Case name: synthetic | globiom | passthrough.
  --out=<csv>    CSV file to append the result row to.
  --rows=<n>     Synthetic row count [default: 1000000].
  --op=<op>      Aggregation operator [default: sum].
  --route=<r>    Input region mapping [default: witch17].
  --to=<t>       Target region mapping [default: witch20].
  --table=<tab>  GLOBIOM table name (case=globiom).
  --db=<db>      GLOBIOM DuckDB path (case=globiom).
  --seed=<s>     RNG seed [default: 1].
" -> doc

opts <- docopt::docopt(doc)

lib <- file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))), "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages(library(witchtools))
suppressMessages(library(data.table))
source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))), "cases.R"))

engine <- opts$engine
op <- opts$op
route <- opts$route
to <- opts$to

if (opts$case == "globiom") {
  dt <- load_globiom_table(opts$db, opts$table)
} else {
  dt <- make_synthetic(route, as.numeric(opts$rows), as.integer(opts$seed))
}
if (opts$case == "passthrough") to <- route
n_in <- nrow(dt)

# Coefficient-build time in isolation (informs the caching decision).
coeff_s <- NA_real_
if (engine == "fast" && route != "iso3" && to != route) {
  t0 <- proc.time()
  cf <- witchtools:::build_region_coeff(
    witchtools::region_mappings[[route]], route,
    witchtools::region_mappings[[to]], to,
    witchtools::default_weights[["gdp"]]
  )
  coeff_s <- (proc.time() - t0)[["elapsed"]]
}

options(witchtools.convert_region_engine = engine)
gc(reset = TRUE)
t0 <- proc.time()
res <- convert_region(dt, from_reg = route, to_reg = to, agg_operator = op,
                      info = (op == "sumby"))
wall <- (proc.time() - t0)[["elapsed"]]

out <- if (is.list(res) && !data.table::is.data.table(res)) res$data else res
row <- data.frame(
  case = opts$case,
  table = if (is.null(opts$table)) "" else opts$table,
  engine = engine, rows_in = n_in, op = op,
  route = paste0(route, "->", to),
  wall_s = round(wall, 3),
  n_out = nrow(out),
  checksum = sprintf("%.6f", sum(out$value, na.rm = TRUE)),
  coeff_s = round(coeff_s, 4)
)
write.table(row, opts$out, sep = ",", row.names = FALSE,
            col.names = !file.exists(opts$out), append = file.exists(opts$out))
cat(sprintf("%s %s %s rows=%d wall=%.2fs\n",
            opts$case, engine, op, n_in, wall))
