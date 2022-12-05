
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPtrends

<!-- badges: start -->
<!-- badges: end -->

This is an interface to work with Google Trends. It uses the
[gtrendsR](https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf)
package, but extends functionality a bit. Users are allowed to search
for up to 13 brands (Google Trends limit is 5), tables and graphs are
returned with raw data, share of search data, and change in search
(z-scored).

Functionality is through a Shiny App that comes with the package. Follow
directions to use within the app.

<img src="https://res.cloudinary.com/dn83gtg0l/image/upload/v1670267567/Screen_Shot_2022-12-05_at_11.10.10_AM.png" width="80%" /><img src="https://res.cloudinary.com/dn83gtg0l/image/upload/v1670267563/Screen_Shot_2022-12-05_at_11.11.24_AM.png" width="80%" />

## Installation

You can install the development version of GSPtrends from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/GSPtrends")
```

## To run the Shiny app:

This is a basic example which shows you how to solve a common problem:

``` r
library(GSPtrends)
runApp()
```
