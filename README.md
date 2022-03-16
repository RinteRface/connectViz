
# connectViz

<!-- badges: start -->
  [![R-CMD-check](https://github.com/RinteRface/connectViz/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/connectViz/actions)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->

The goal of `{connectViz}` is to offer a collection of helper functions and 'htmlwidgets' to help admins or user better understand how 'RStudio Connect' is used in their organization. The package provides plug and play visualizations that can be customized depending on needs.

## Installation

You can install the development version of `{connectViz}` like so:

``` r
remotes::install_github("RinteRface/connectViz")
```

## Prerequisites
Before starting with `{connectViz}`, makes sure to have the two following environment variables setup:

  - `CONNECT_SERVER`: points to the RStudio Connect server for instance `https://beta.rstudioconnect.com`.
  - `CONNECT_API_KEY`: the API key you generate from RStudio Connect server. 
  



## Contribute
This package is still a proof of concept. If you want to help improving it and test it on your own RStudio Connect server:
- Install it.
- Run the [vignette](https://github.com/RinteRface/connectViz/blob/main/inst/examples/simple-rmd/analysis.Rmd).
- Report any error [here](https://github.com/RinteRface/connectViz/issues), by submitting a new issue, including the RStudio Connect version, `{connectapi}` version ... 

Thanks in advance for your support. 

## Code of Conduct
  
  Please note that the connectViz project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.


