% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_decomposition.R
\name{ts_decomposition}
\alias{ts_decomposition}
\title{Time Series Decompositions}
\usage{
ts_decomposition(tbl, term)
}
\arguments{
\item{tbl}{Data frame returned by Google Trends API}

\item{term}{Keyword to focus on for decompositions}
}
\value{
List of graphs - STL decomposition; Seasonally adjusted plot; Seasonality v Trend
}
\description{
If the trends data is longer than 3 years in length, function runs an STL decomposition on each series.

See Hyndman "Forecasting: Principles and Practice" \url{https://otexts.com/fpp3/stlfeatures.html} for details.
}
\examples{
\dontrun{
decomp(tbl, directory, project)}
}
