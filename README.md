
<!-- README.md is generated from README.Rmd. Please edit that file -->

# storeMap

<!-- badges: start -->
<!-- badges: end -->

This package implements an R Shiny application, showcasing interactive
map functionality using the R Leaflet package.

### Installation

You can install the development version of storeMap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ztgroton/storeMap")
```

NOTE: You’ll still need to render `README.Rmd` regularly, to keep
`README.md` up-to-date. `devtools::build_readme()` is handy for this.

### Example

Below is an example of how to launch the application from the R Console:

``` r
library(storeMap)
storeMap::app_pkg_run()
```

### Publishing

The app can be published to a central server (Shiny Server, Posit
Connect etc.) using the top-level ‘app.R’ file.
