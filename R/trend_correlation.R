#' Estimate correlations among Google Trend keywords
#'
#' @description Estimates the correlations across keywords with a correlation table.
#'
#' @param tbl Data frame output from Google Trends API
#'
#' @return Data frame of correlation data
#' @export
#'
#' @examples
#' \dontrun{
#' trend_correlation(tbl)}
trend_correlation <- function(tbl) {
  # max length for each keyword
  max_length <- length(unique(tbl$date))

  # widen all data for cor.test
  out1 <- tbl |>
    dplyr::select(date, hits, keyword) |>
    tidyr::pivot_wider(names_from = keyword,
                       values_from = hits) |>
    tibble::column_to_rownames(var = "date")

  # create ids for even split on time, group, and widen
  out2 <- tbl |>
    dplyr::group_by(keyword) |>
    dplyr::mutate(id = rep(letters[1:4], each = ceiling(max_length/4))[1:max_length]) |>
    dplyr::select(date, id, hits, keyword) |>
    tidyr::pivot_wider(id_cols = c(date, id),
                       names_from = keyword,
                       values_from = hits) |>
    tibble::column_to_rownames(var = "date") |>
    dplyr::group_split(id, .keep = FALSE)
  # get ranges to use for names
  names <- dplyr::bind_rows(tibble(id = 'aa', xx = 'overall'), tbl |>
                              dplyr::group_by(keyword) |>
                              dplyr::mutate(id = rep(letters[1:4], each = ceiling(max_length/4))[1:max_length]) |>
                              dplyr::group_by(id) |>
                              dplyr::summarise(xx = paste(range(date), collapse = " - "))) |>
    dplyr::pull(xx)
  # put all into single list
  x <- c(list(out1), out2)
  # function
  tmpcorr <- function(dat) {
    corrr::colpair_map(dat, calc_p_value_sig, 0.05) |>
      corrr::shave() |>
      dplyr::mutate_all(tidyr::replace_na, "")
  }
  # run it
  out <- purrr::map(x, tmpcorr) |>
    purrr::set_names(nm = names)
}
