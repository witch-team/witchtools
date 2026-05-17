# SSA regions for a given region mapping.

`ssa_regions` returns a vector of region representing Sub-Saharan
Africa.

## Usage

``` r
ssa_regions(region_mapping)
```

## Arguments

- region_mapping:

  a data.table of regional mapping.

## Value

a vector of region name.

## See also

Other misc functions:
[`brazil_regions()`](http://witchtools.witchmodel.org/reference/brazil_regions.md),
[`china_regions()`](http://witchtools.witchmodel.org/reference/china_regions.md),
[`eu27_regions()`](http://witchtools.witchmodel.org/reference/eu27_regions.md),
[`eu28_regions()`](http://witchtools.witchmodel.org/reference/eu28_regions.md),
[`eu_regions()`](http://witchtools.witchmodel.org/reference/eu_regions.md),
[`europe_regions()`](http://witchtools.witchmodel.org/reference/europe_regions.md),
[`india_regions()`](http://witchtools.witchmodel.org/reference/india_regions.md),
[`indonesia_regions()`](http://witchtools.witchmodel.org/reference/indonesia_regions.md),
[`oceania_regions()`](http://witchtools.witchmodel.org/reference/oceania_regions.md),
[`oecd_regions()`](http://witchtools.witchmodel.org/reference/oecd_regions.md),
[`region_id()`](http://witchtools.witchmodel.org/reference/region_id.md),
[`usa_regions()`](http://witchtools.witchmodel.org/reference/usa_regions.md)

## Examples

``` r
ssa_regions(region_mappings[["witch17"]])
#> [1] "ssa"
```
