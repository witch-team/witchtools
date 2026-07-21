#!/usr/bin/env bash
# Run the full region-conversion benchmark matrix.
#
# Every cell runs in a fresh Rscript subprocess inside a systemd scope with a
# hard memory cap and swap disabled, so an over-budget run is OOM-killed
# (recorded as DNF) instead of thrashing the machine. Peak RSS is taken from
# GNU time. All artifacts stay under benchmarks/ (never /tmp, which is tmpfs).
#
# Usage: benchmarks/run_all.sh [MemoryMax, default 6G]
set -u
cd "$(dirname "$0")/.."

MEM="${1:-6G}"
RES=benchmarks/results
RAW=$RES/raw
CSV=$RES/cells.csv
mkdir -p "$RAW"
rm -f "$CSV" "$RAW"/*.time

# One-time install of the current source into a private library.
mkdir -p benchmarks/lib
R CMD INSTALL --no-docs --no-help . -l benchmarks/lib >/dev/null

GLOBIOM_DB="${WITCH_GLOBIOM_DB:-/home/lolow/Sync/03-WITCH/witch-master/data_witch17/data_globiom2025_rep.duckdb}"

run_cell() {
  local name="$1"; shift
  echo "== $name"
  systemd-run --user --scope --same-dir -q \
    -p "MemoryMax=$MEM" -p MemorySwapMax=0 \
    /usr/bin/time -v -o "$RAW/$name.time" \
    Rscript benchmarks/run_case.R --out="$CSV" "$@" \
    || echo "DNF $name" >> "$RES/dnf.txt"
}

for engine in legacy fast; do
  # Synthetic witch17 -> witch20
  for rows in 100000 1000000 4000000; do
    for op in sum mean sumby; do
      run_cell "syn_${rows}_${op}_${engine}" \
        --engine=$engine --case=synthetic --rows=$rows --op=$op
    done
  done
  # Pass-through no-regression
  run_cell "passthrough_1000000_${engine}" \
    --engine=$engine --case=passthrough --rows=1000000
  # iso3 upscale
  for rows in 250000 1000000; do
    run_cell "iso3_${rows}_sum_${engine}" \
      --engine=$engine --case=synthetic --rows=$rows --route=iso3 --to=witch17
  done
  # Real GLOBIOM tables
  if [ -f "$GLOBIOM_DB" ]; then
    for tab in mean_gdpcap1 sum_forest2; do
      op=${tab%%_*}
      run_cell "globiom_${tab}_${engine}" \
        --engine=$engine --case=globiom --db="$GLOBIOM_DB" --table="$tab" --op="$op"
    done
  fi
done

Rscript benchmarks/summarize.R
