
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPtrends

The GSPtrends package provides an easy way to build queries and call the
Google Trends API without needing much background with R code. Upon
calling the main function `interest_over_time()` the user is asked a
series of questions. Answers are stored in a list and used as arguments
to build the API call.

The function will default to the current working directory of the user.
If the user wishes to save the data and graphs to another location,
they’ll have an opportunity to change the directory within the workflow
of the function.

## Plots and Visuals

When the data is returned, the package then creates a series of graphs
for the user in a selected directory.

Graphs include:

-   Raw search interest (stacked area and line plots)
-   Share of search if multiple terms were used (stacked area and line
    plots)
-   Moving averages of each (MA3, 5, 7, 13)
-   Z-scores of the rolling raw data to demonstrate changes in search
    behavior for each term
-   Correlations between each series are presented in a heatmap

If more than 3 years of data is returned:

-   Time series decomposition of each series (via STL decomposition)
-   Individual series graphed with the seasonal adjustment of each
    series

## Data

All data is saved to Excel workbooks. Data is in both tidy and wide
formats.

## Installation

You can install the development version of GSPtrends from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/GSPtrends")
```

## To run:

Only one function is necessary to run everything

``` r
library(GSPtrends)
interest_over_time()
```
