# Region and Time Conversion

## Region conversion

In this section, we convert a dataset containing the GDP of all
countries into a 17-region mapping of the WITCH model, then into the
5-region mapping used in the SSP database.

First, let’s create the initial database, we will extract the GDP in
2005 from the default_weights. The column names have to be renamed. By
convention, column names must be “iso3” and “value”

``` r
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
library(witchtools)

# Creation of the initial dataset
# Note the column names: iso3 and value
gdp_iso3 <- copy(default_weights[['gdp']])
setnames(gdp_iso3,'weight','value')
gdp_iso3
#> Index: <value>
#>        iso3        value
#>      <char>        <num>
#>   1:    AFG 7.543866e+01
#>   2:    ALB 3.861787e+01
#>   3:    DZA 4.673124e+02
#>   4:    AGO 1.979695e+02
#>   5:    ARG 8.905475e+02
#>  ---                    
#> 246:    BLM 1.000000e-10
#> 247:    CYM 1.000000e-10
#> 248:    KNA 1.000000e-10
#> 249:    MAF 1.000000e-10
#> 250:    MSR 1.000000e-10
```

Now, using the function `convert_region`, we will aggregate this dataset
at country-level (ISO3) to the 17-region mapping of the WITCH model,
available as a default mapping.

``` r
# Aggregate value at WITCH regional scale (17 regions)
gdp_witch17 <- convert_region(gdp_iso3, to_reg = 'witch17')
gdp_witch17
#>         witch17      value
#>          <char>      <num>
#>  1:        laca  3811.0076
#>  2:       sasia  2616.5509
#>  3:         ssa  3281.0841
#>  4:      europe 22137.4325
#>  5:        mena  7253.0736
#>  6:          te  8156.9139
#>  7:     oceania  1501.8456
#>  8:      brazil  2969.7366
#>  9:      seasia  4873.5174
#> 10:      canada  1746.1213
#> 11:       china 22961.7494
#> 12:   indonesia  3118.3992
#> 13:       india  8377.0372
#> 14:      jpnkor  7243.6577
#> 15:      mexico  2300.2179
#> 16:         usa 19910.1964
#> 17: southafrica   749.2962
```

In a final step, we will convert the 17-region data into the 5-region of
the SSP database. The dataset is first disaggregated at iso3 level then
it is aggregated into 5 regions, automatically.

``` r
# Aggregate value at WITCH regional scale (17 regions)
gdp_r5 <- convert_region(gdp_witch17, to_reg = 'r5')
gdp_r5
#>        r5     value
#>    <char>     <num>
#> 1:  r5lam  8973.866
#> 2: r5oecd 52788.486
#> 3: r5asia 44228.718
#> 4:  r5maf 11283.454
#> 5:  r5ref  5733.314
```

## Time conversion

In this section, we convert a yearly time-serie into the default time
period of the WITCH model, 5 year-periods from 2005 to 2150, using the
function `convert_time_period`.

``` r

# Time-serie
dd <- data.table(tech = c("tech1","tech2"), year = 2005:2050, value = 1:46)

# Aggregate values to the default WITCH time period mapping
res <- convert_time_period(dd, 't30')
print(nrow(res))
#> [1] 20
print(res, nrows = 20)
#>       tech      t value
#>     <char> <char> <num>
#>  1:  tech1      1     2
#>  2:  tech2      1     2
#>  3:  tech2      2     6
#>  4:  tech1      2     6
#>  5:  tech1      3    11
#>  6:  tech2      3    11
#>  7:  tech2      4    16
#>  8:  tech1      4    16
#>  9:  tech1      5    21
#> 10:  tech2      5    21
#> 11:  tech2      6    26
#> 12:  tech1      6    26
#> 13:  tech1      7    31
#> 14:  tech2      7    31
#> 15:  tech2      8    36
#> 16:  tech1      8    36
#> 17:  tech1      9    41
#> 18:  tech2      9    41
#> 19:  tech2     10    45
#> 20:  tech1     10    45
#>       tech      t value
#>     <char> <char> <num>
```

The function `convert_time_period` can also extrapolate the missing
periods.

``` r

# Aggregate values to the default WITCH time period mapping with extrapolation
res <- convert_time_period(dd, 't30', do_extrap = TRUE)
print(nrow(res))
#> [1] 60
print(tail(res))
#>      tech      t value
#>    <char> <char> <num>
#> 1:  tech2     25    46
#> 2:  tech2     26    46
#> 3:  tech2     27    46
#> 4:  tech2     28    46
#> 5:  tech2     29    46
#> 6:  tech2     30    46
```
