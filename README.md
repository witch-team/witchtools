
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Because of a bug, duplicate the header below in the md file before building pkgdown  -->

# witchtools <img src="man/figures/logo.png" align="right" alt="" width="120" />

> Data Management for IAMs

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![devel:0.4.4](https://img.shields.io/badge/devel%20version-0.4.4-green.svg)](https://github.com/witch-team/witchtools)
[![codecov](https://codecov.io/gh/witch-team/witchtools/branch/master/graph/badge.svg)](https://codecov.io/gh/witch-team/witchtools)
<!-- badges: end -->

## Overview

The `witchtools` package provides a toolkit to manage data in integrated
assessment models. `witchtools` is part of the tool ecosystem of the
[WITCH model](https://www.witchmodel.org). The package makes it possible
to easily and transparently reproduce the data processing between the
raw data, the data used in the model and the results uploaded in the
IAMC database. While the package has been developed for the [WITCH
model](https://www.witchmodel.org), it is already used to simply process
data for other uses or models.

## Features

- Conversion of a yearly time series into a time-period (including
  possible decision tree structure) based time series.
- Conversion of data from one regional mapping to another one.
- Default time-period mappings, regional mappings and aggregation
  weights used by the WITCH model
- Batch conversion of the make_data files from the input directory of
  the WITCH model
- Easy access to the witch-team/witch-data repository

## Installation

    if (!requireNamespace("remotes"))
      install.packages("remotes")

    remotes::install_github("witch-team/witchtools")

## Usage

``` r
library(witchtools)

# Convert from 17 WITCH regions to the 5 SSP regions 
gdp_r5 <- convert_region(gdp_witch17, to_reg = 'r5')

# Convert from yearly to the WITCH time perdios
dd_t30 <- convert_region(dd_year, time_mapping = 't30')

# for WITCH user: Generate input data of the WITCH model
witch_translate_data(region = 'witch17', timescale = 't30')
```
