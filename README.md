# witchtools: Data Management for IAMs<img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![](https://img.shields.io/badge/devel%20version-0.2.0-green.svg)](https://github.com/witch-team/witchtools)[![](https://img.shields.io/badge/lifecycle-maturing-purple.svg)](https://github.com/witch-team/witchtools)
<!-- badges: end -->


## Overview

The *witchtools* package provides a toolkit to manage data in integrated assessment models, 
with a primary focus on the [WITCH model](https://www.witchmodel.org).

## Features

- Convert yearly data into time-period data.
- Convert data from one regional mapping to another one.


## Installation

```
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("witch-team/witchtools")
```

## Example

``` r
library(witchtools)
```
