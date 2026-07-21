# Benchmark input builders. Sourced by run_case.R.
#
# Synthetic tables mimic the GLOBIOM reporting shape: character id columns,
# a character period column t, a region column and a numeric value.

make_synthetic <- function(route, n_rows, seed = 1) {
  set.seed(seed)
  regs <- if (route == "iso3") {
    witchtools::region_mappings[["witch17"]]$iso3
  } else {
    unique(witchtools::region_mappings[[route]][[route]])
  }
  k <- ceiling(n_rows / (length(regs) * 30L))
  dt <- data.table::CJ(
    variable = paste0("v", seq_len(k)),
    scenario = c("s1", "s2"),
    t = as.character(1:15),
    region = regs,
    sorted = FALSE
  )
  dt[, value := stats::runif(.N, 0, 1000)]
  dt <- dt[seq_len(min(.N, n_rows))]
  data.table::setnames(dt, "region", route)
  return(dt)
}

# Load one table from a GLOBIOM DuckDB database (region column `n`, witch17).
# The witchtools region column name is required by convert_region, so `n`
# is renamed to `witch17` as convert_item would after region guessing.
load_globiom_table <- function(db, table) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dt <- data.table::setDT(
    DBI::dbGetQuery(con, paste0("SELECT * FROM \"", table, "\""))
  )
  data.table::setnames(dt, "n", "witch17")
  return(dt)
}
