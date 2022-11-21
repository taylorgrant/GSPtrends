#' Split a string into a vector of strings
#'
#' @param string A comma separated string
#'
#' @return A vector of strings based on comma locations
#' @export
#'
#' @examples
#' bare_combine("t1, t2, t3, t4")
bare_combine <- function(string) {
  bits <- unlist(strsplit(string, ",", " "))
  bits <- stringr::str_trim(bits)
}
