# convert_region engine benchmark

Run: 2026-07-21 host cap: see run_all.sh

| case | rows | op | route | legacy s | fast s | speedup | legacy MB | fast MB | mem x | checksums |
|---|---|---|---|---|---|---|---|---|---|---|
| globiom:mean_gdpcap1 | 2,142,000 | mean | witch17->witch20 | NA | 6.39 | NAx | NA | 973 | NAx | NA |
| globiom:sum_forest2 | 2,142,000 | sum | witch17->witch20 | NA | 5.59 | NAx | NA | 788 | NAx | NA |
| passthrough | 1,000,000 | sum | witch17->witch17 | 0.00 | 0.00 | 1.0x | 162 | 162 | 1.0x | match |
| synthetic |   100,000 | mean | witch17->witch20 | 1.31 | 0.26 | 5.1x | 462 | 146 | 3.2x | match |
| synthetic |   100,000 | sum | witch17->witch20 | 1.21 | 0.24 | 5.1x | 523 | 138 | 3.8x | match |
| synthetic |   100,000 | sumby | witch17->witch20 | 1.91 | 0.43 | 4.5x | 523 | 140 | 3.7x | match |
| synthetic |   250,000 | sum | iso3->witch17 | 0.06 | 0.06 | 0.9x | 141 | 140 | 1.0x | match |
| synthetic | 1,000,000 | mean | witch17->witch20 | 12.68 | 2.50 | 5.1x | 2730 | 396 | 6.9x | match |
| synthetic | 1,000,000 | sum | iso3->witch17 | 0.35 | 0.34 | 1.0x | 284 | 285 | 1.0x | match |
| synthetic | 1,000,000 | sum | witch17->witch20 | 12.67 | 2.32 | 5.5x | 3362 | 348 | 9.7x | match |
| synthetic | 1,000,000 | sumby | witch17->witch20 | 20.10 | 4.18 | 4.8x | 3222 | 425 | 7.6x | match |
| synthetic | 4,000,000 | mean | witch17->witch20 | NA | 9.97 | NAx | NA | 1280 | NAx | NA |
| synthetic | 4,000,000 | sum | witch17->witch20 | NA | 8.68 | NAx | NA | 1115 | NAx | NA |
| synthetic | 4,000,000 | sumby | witch17->witch20 | NA | 16.25 | NAx | NA | 1402 | NAx | NA |

## Did not finish (OOM-killed under the memory cap)

- DNF syn_4000000_sum_legacy
- DNF syn_4000000_mean_legacy
- DNF syn_4000000_sumby_legacy
- DNF globiom_mean_gdpcap1_legacy
- DNF globiom_sum_forest2_legacy
