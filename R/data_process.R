#' Clean data for saving
#'
#' @description Cleaning data and compiling into a single Excel sheet
#'
#' @param tbl Data frame returned by Google Trends API
#' @param calc_tbl Calculated table in wide form
#' @param corr_tbl Correlation table
#' @param arg_list Additional arguments passed through by user
#'
#' @return Excel sheets saved to data subfolder
#' @export
#'
#' @examples
#' \dontrun{
#' data_process(tbl, calc_tbl, corr_tbl, arg_list)}
data_process <- function(tbl, calc_tbl, corr_tbl, arg_list) {

  # cover sheet
  cover <- cover_sheet(tbl, arg_list)

  # wide version of trends data
  gtrends_wide <- tbl %>%
    tidyr::pivot_wider(
      names_from = keyword,
      values_from = hits
    )

  # raw share of search
  raw_share <- calc_tbl %>%
    dplyr::filter(ma == "ma3") %>%
    dplyr::select(date,keyword:raw_share) %>%
    tidyr::pivot_wider(
      names_from = keyword,
      values_from = raw_share)

  # moving average spreads for each
  ma_spread <- function(calc_tbl, m) {
    calc_tbl %>%
      dplyr::filter(ma == m) %>%
      dplyr::select(-c(hits, raw_share)) %>%
      tidyr::pivot_wider(
        names_from = keyword,
        values_from = c(ma_raw, ma_share, z_ma_raw)) %>%
      dplyr::rename_with(~stringr::str_replace_all(., 'ma_raw', "smoothed_interest"), dplyr::starts_with("ma_raw")) %>%
      dplyr::rename_with(~stringr::str_replace_all(., 'ma_share', "smoothed_share"), dplyr::starts_with("ma_share")) %>%
      dplyr::rename_with(~stringr::str_replace_all(., 'z_ma_raw', "zscore"), dplyr::starts_with("z_ma_raw"))
  }
  avgs <- c("ma3", "ma5", "ma7", "ma13")
  ma_out <- purrr::map2(list(calc_tbl), avgs, ma_spread) %>%
    purrr::set_names(avgs)

  # summary
  simple_summary <- tbl %>%
    dplyr::group_by(keyword) %>%
    dplyr::summarise(min = min(hits), median = stats::median(hits),
                     avg = mean(hits), max = max(hits),
                     sd = stats::sd(hits))

  # correlation data
  correlation_table <- corr_tbl

  data_list <- list(cover_sheet = cover,
                    gtrends_output = tbl, gtrends_output_wide = gtrends_wide,
                    raw_share = raw_share,
                    smoothed_ma3 = ma_out$ma3, smoothed_ma5 = ma_out$ma5,
                    smoothed_ma7 = ma_out$ma7, smoothed_ma13 = ma_out$ma13,
                    simple_summary = simple_summary,
                    correlation_table = correlation_table)
}
