#' Find the keyword with peak search interest
#'
#' @param tbl A data frame of data returned by Google Trends API
#'
#' @return One row data frame of Trends data with hits score of 100
#' @export
#'
#' @examples
#' \dontrun{
#' find100(data)}
find100 <- function(tbl) {
  tmp <- tbl %>%
    dplyr::mutate(hits = as.numeric(hits),
                  hits = ifelse(is.na(hits), 0, hits),
                  date = as.Date(date)) %>%
    dplyr::filter(hits == 100) %>%
    dplyr::distinct(keyword, .keep_all = TRUE)
}
