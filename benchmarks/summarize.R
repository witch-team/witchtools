#!/usr/bin/env Rscript
# Join the per-cell CSV with peak-RSS figures from the GNU time files and
# write results.csv + results.md (side-by-side engines, speedup, mem ratio,
# checksum agreement).
suppressMessages(library(data.table))

res_dir <- file.path("benchmarks", "results")
cells <- fread(file.path(res_dir, "cells.csv"))

rss_of <- function(f) {
  ln <- grep("Maximum resident set size", readLines(f, warn = FALSE), value = TRUE)
  if (!length(ln)) return(NA_real_)
  as.numeric(sub(".*: ", "", ln)) / 1024 # MB
}
cells[, cell := paste0(
  ifelse(case == "globiom", paste0("globiom_", table),
    ifelse(case == "passthrough", paste0("passthrough_", rows_in),
      paste0("syn_", rows_in, "_", op))),
  ifelse(case == "synthetic" & route == "iso3->witch17",
         "", "")
)]
# time-file names were written by run_all.sh; recover them from its scheme
cells[, timefile := sprintf(
  "%s/raw/%s_%s.time", res_dir,
  ifelse(case == "globiom", paste0("globiom_", table),
    ifelse(case == "passthrough", paste0("passthrough_", rows_in),
      ifelse(route == "iso3->witch17", paste0("iso3_", rows_in, "_", op),
        paste0("syn_", rows_in, "_", op)))),
  engine
)]
cells[, peak_mb := sapply(timefile, rss_of)]

wide <- dcast(cells,
  case + table + rows_in + op + route ~ engine,
  value.var = c("wall_s", "peak_mb", "checksum", "n_out")
)
wide[, speedup := round(wall_s_legacy / wall_s_fast, 1)]
wide[, mem_ratio := round(peak_mb_legacy / peak_mb_fast, 1)]
wide[, checksum_ok := checksum_legacy == checksum_fast |
  abs(as.numeric(checksum_legacy) - as.numeric(checksum_fast)) <
    1e-6 * pmax(abs(as.numeric(checksum_legacy)), 1)]

fwrite(wide, file.path(res_dir, "results.csv"))

md <- c(
  "# convert_region engine benchmark",
  "",
  sprintf("Run: %s host cap: see run_all.sh", format(Sys.time(), "%Y-%m-%d")),
  "",
  "| case | rows | op | route | legacy s | fast s | speedup | legacy MB | fast MB | mem x | checksums |",
  "|---|---|---|---|---|---|---|---|---|---|---|",
  wide[, sprintf("| %s%s | %s | %s | %s | %.2f | %.2f | %.1fx | %.0f | %.0f | %.1fx | %s |",
    case, ifelse(table == "", "", paste0(":", table)),
    format(rows_in, big.mark = ","), op, route,
    wall_s_legacy, wall_s_fast, speedup,
    peak_mb_legacy, peak_mb_fast, mem_ratio,
    ifelse(checksum_ok, "match", "MISMATCH"))]
)
dnf <- file.path(res_dir, "dnf.txt")
if (file.exists(dnf)) {
  md <- c(md, "", "## Did not finish (OOM-killed under the memory cap)", "",
          paste0("- ", readLines(dnf)))
}
writeLines(md, file.path(res_dir, "results.md"))
cat("Wrote", file.path(res_dir, "results.md"), "\n")
