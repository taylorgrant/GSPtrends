#' Calculate share of search
#'
#'@description Calculates the share of search for each search term and date period.
#' Share is calculated for raw hits as well as multiple moving averages.
#' Z-scores are also calculated for each time period and Keyword.
#' @param tbl Data frame output from Google Trends API
#'
#' @return A tidy data frame with share calculations for each term overall, MA3, MA5, MA7, and MA13.
#' @export
#'
#' @examples
#' \dontrun{
#' share_calc(tbl)}
share_calc <- function(tbl) {
  tmp <- tbl %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(raw_share = hits/sum(hits)) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(ma3 = slider::slide_dbl(hits, mean, .before = 1, .after = 1, .complete = TRUE),
                  ma5 = slider::slide_dbl(hits, mean, .before = 2, .after = 2, .complete = TRUE),
                  ma7 = slider::slide_dbl(hits, mean, .before = 3, .after = 3, .complete = TRUE),
                  ma13 = slider::slide_dbl(hits, mean, .before = 6, .after = 6, .complete = TRUE)) %>%
    tidyr::pivot_longer(-c(date:category, raw_share),
                        names_to = "ma",
                        values_to = "ma_raw") %>%
    dplyr::group_by(date, ma) %>%
    dplyr::mutate(ma_share = ma_raw/sum(ma_raw)) %>%
    dplyr::group_by(keyword, ma) %>%
    dplyr::mutate(z_ma_raw = scale(ma_raw)) %>%
    dplyr::ungroup()
}
