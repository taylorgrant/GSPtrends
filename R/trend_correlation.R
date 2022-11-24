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
  # spread data to wide for cor.test
  out_1 <- tbl |>
    dplyr::select(date, hits, keyword) |>
    tidyr::pivot_wider(names_from = keyword,
                       values_from = hits) |>
    tibble::column_to_rownames(var = "date")

  # function for test
  calc_p_value <- function(vec_a, vec_b, sig_level) {
    test_res <- stats::cor.test(vec_a, vec_b)
    round(stats::cor.test(vec_a, vec_b)$estimate, 2)
  }

  calc_p_value_sig <- function(vec_a, vec_b, sig_level) {
    test_res <- stats::cor.test(vec_a, vec_b)
    sig <- dplyr::if_else(test_res$p.value < sig_level, "*", "")
    paste0(round(stats::cor.test(vec_a, vec_b)$estimate, 2), sig)
  }

  # graph first
  corr_out <- corrr::colpair_map(out_1, calc_p_value, 0.05) |>
    corrr::stretch()

  # get median correlation for text color in heatmap
  med_r <- stats::median(corr_out$r, na.rm = TRUE)
  corr_gr_out <- corrr::colpair_map(out_1, calc_p_value_sig, 0.05) |>
    corrr::stretch() |>
    dplyr::rename(rstar = r) |>
    dplyr::left_join(corr_out, by = c("x" = "x",
                               "y" = "y")) |>
    dplyr::mutate(txt_col = ifelse(r < med_r, "a", "b"))

  # p1 <- ggplot2::ggplot(data = corr_gr_out, ggplot2::aes(x = x, y = y, fill = r, label = rstar)) +
  #   ggplot2::geom_tile(color = "white") +
  #   ggplot2::geom_text(ggplot2::aes(col = txt_col), show.legend = FALSE,
  #                      size = 6) +
  #   ggplot2::scale_color_manual(values = c("a" = "white","b" = "black")) +
  #   ggplot2::theme_minimal() +
  #   viridis::scale_fill_viridis(option="E",
  #                               name = NULL,
  #                               # begin = .1, end = .9,
  #                               na.value = "gray") +
  #   ggplot2::labs(x = NULL, y = NULL,
  #        title = "Correlation in trends between search terms",
  #        subtitle = glue::glue("{range(tbl$date)[1]} -TO- {range(tbl$date)[2]}"))


  # table data second
  corr_tbl <- corrr::colpair_map(out_1, calc_p_value_sig, 0.05) |>
    corrr::shave() |>
    dplyr::mutate_all(tidyr::replace_na, "")

  list(corr_plot = p1,
       corr_tbl = corr_tbl)
}
