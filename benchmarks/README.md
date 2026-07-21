# Region-conversion benchmarks

Compares the `fast` (pair-coefficient) and `legacy` (iso3-explosion) engines
of `convert_region()` on synthetic GLOBIOM-shaped tables and on real tables
from the witch-master GLOBIOM DuckDB database.

    benchmarks/run_all.sh [MemoryMax]   # default cap 6G

Each cell runs in a fresh Rscript subprocess inside a systemd scope
(`MemoryMax`, swap disabled) so an over-budget run is OOM-killed and recorded
as DNF instead of swap-thrashing the machine. Peak RSS comes from
`/usr/bin/time -v`. Set `WITCH_GLOBIOM_DB` to point at a different GLOBIOM
database. Summary lands in `results/results.md`.
