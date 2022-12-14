% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cover_sheet.R
\name{cover_sheet}
\alias{cover_sheet}
\title{Cover sheet for excel output}
\usage{
cover_sheet(tbl, arg_list)
}
\arguments{
\item{tbl}{Data frame returned by Google Trends API}

\item{arg_list}{Additional arguments provided by user}
}
\value{
Tibble that will end up on the 1st sheet of excel output
}
\description{
Cover sheet for excel output
}
\examples{
\dontrun{
cover_sheet(tbl, arg_list)}
}
