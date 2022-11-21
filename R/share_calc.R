#' Calculate share of search
#'
#'@description Calculates the share of search for each search term and date period.
#' Share is calculated for raw hits data as well as several moving averages (via Shiny sliderInput).
#' Z-scores are also calculated for each time period and Keyword.
#' @param tbl Data frame output from Google Trends API
#'
#' @return tibble with Google Trends data as well as share of search and z-scored search
#' @export
#'
#' @examples
#' \dontrun{
#' share_calc(tbl)}
share_calc <- function(tbl, ma) {

  # if sliderInput is 0, standard share
  if (ma == 0) {
    tmp <- tbl |>
      dplyr::group_by(date) |>
      dplyr::mutate(raw_share = hits/sum(hits)) |>
      dplyr::mutate(ma_share = raw_share) |>
      dplyr::group_by(keyword) |>
      dplyr::mutate(z_ma_raw = scale(ma_share)) |>
      dplyr::ungroup()
  } else {
    ma_ba <- switch(ma, "3" = 1, "5" = 2, "7" = 3, "9" = 4, "11" = 5, "13" = 6)
    tmp <- tbl |>
      dplyr::group_by(date) |>
      dplyr::mutate(raw_share = hits/sum(hits)) |>
      dplyr::group_by(keyword) |>
      dplyr::mutate(mov_avg = glue::glue("ma{ma}"),
                    ma_raw = slider::slide_dbl(hits, mean, .before = ma_ba, .after = ma_ba, .complete = TRUE)) |>
      dplyr::group_by(date) |>
      dplyr::mutate(ma_share = ma_raw/sum(ma_raw)) |>
      dplyr::group_by(keyword) |>
      dplyr::mutate(z_ma_raw = scale(ma_raw)) |>
      dplyr::ungroup()
  }

  p1 <- ggplot2::ggplot(tmp, ggplot2::aes(x = date, y = ma_share, group = keyword, fill = keyword)) +
    ggplot2::geom_area(stat = "identity",
                       color = 'white',
                       size = .1,
                       alpha = .9)

  list(tmp = tmp, plot = p1)

}
