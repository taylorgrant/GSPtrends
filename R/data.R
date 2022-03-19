#' Google Trends categories
#'
#' The category names and IDs used when specifying a category in a trends search.
#'
#' @format A dataframe with 1426 categories and IDs
#'
#' \describe{
#' \item{name}{Category name, e.g. Arts & Entertainment}
#' \item{id}{Numeric ID, e.g., 3}
#' }
"categories"


#' Google Trends US metro codes
#'
#' The metro areas and geo codes used by Google in its trends searches. Data was
#' created from geo code from
#' \url{"https://trends.google.com/trends/api/explore/pickers/geo"}
#'
#' @format A data frame with 210 metros by name, id, state code and geo code
#'
#' \describe{
#' \item{name}{Name of DMA, e.g., Anchorage AK}
#' \item{id}{Three digit id code, e.g., 743}
#' \item{code}{State abbreviation, e.g., AK}
#' \item{geo}{Geo region code used by Google Trends, e.g., US-AK-74}3
#' }
"metros"
