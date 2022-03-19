#' Search for Google Trend categories by name
#'
#' Using str_detect to search, returns partial matches; capitalization doesn't matter
#'
#' @param term Term to use in the search
#'
#' @return A two column data frame of matched terms
#' @export
#' @importFrom utils data
#'
#' @examples
#' cat_search("audio")
cat_search <- function(term) {
  data("categories", envir = environment())
  categories %>%
    dplyr::filter(stringr::str_detect(term, stringr::fixed(term, ignore_case = TRUE))) %>%
    dplyr::distinct(id, .keep_all = TRUE)
}
