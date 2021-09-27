
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KarstID: Analysis of Karst Spring Hydrographs

<!-- badges: start -->
<!-- badges: end -->

The goal of KarstID is to implement common analyses of karst spring
hydrographs in R through a Shiny application. It includes recession
curves, statistical, classified discharges and correlational and
spectral analyses. The application also allows performing a
classification of the hydrological functioning and comparing the results
to a database of 78 karst systems.

## Installation

You can install KarstID from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("busemorose/KarstID")
```

## Launch

This is how you launch the app:

``` r
library(KarstID)
KarstID()
```
