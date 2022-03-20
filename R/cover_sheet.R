#' Cover sheet for excel output
#'
#' @param tbl Data frame returned by Google Trends API
#' @param arg_list Additional arguments provided by user
#'
#' @return Tibble that will end up on the 1st sheet of excel output
#' @export
#'
#' @examples
#' \dontrun{
#' cover_sheet(tbl, arg_list)}
cover_sheet <- function(tbl, arg_list) {
  top <- find100(tbl)
  terms <- paste(arg_list$kw, collapse = ", ")
  range <- paste0(as.character(range(tbl$date))[1], "- ", as.character(range(tbl$date))[2])
  key <- c("Date run:", "Search Terms:", "Category:", "Timeframe/Duration",
           "Trend type:", "Dates covered",
           "Term with Peak Search:")
  value <- c(as.character(Sys.Date()), terms,
             arg_list$category_name, arg_list$duration,
             arg_list$gprop, range, top$keyword)
  cover <- tibble::tibble(key, value)
}
