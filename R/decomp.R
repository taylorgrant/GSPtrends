#' Time Series Decompositions
#'
#' @description If the trends data is longer than 3 years in length, function runs an STL decomposition on each series.
#'
#' See Hyndman "Forecasting: Principles and Practice" \url{https://otexts.com/fpp3/stlfeatures.html} for details.
#'
#' @param tbl Data frame returned by Google Trends API
#' @param directory Location where decompositions are saved; user will have previously defined directories
#' @param project Name of project; previously provided by user
#'
#' @return Data frame and graphs. Data frame with decomp metrics. Graphs are saved into a `/graphs/decompositions/` subdirectory.
#' @export
#'
#' @examples
#' \dontrun{
#' decomp(tbl, directory, project)}
decomp <- function(tbl, directory, project) {
  if (tbl$date[2] - tbl$date[1] <= 7) {
    ts_out <- tbl %>%
      dplyr::mutate(date = tsibble::yearweek(date)) %>%
      tsibble::as_tsibble(key = keyword, index = date)

  } else {
    ts_out <- tbl %>%
      dplyr::mutate(date = tsibble::yearmonth(date)) %>%
      tsibble::as_tsibble(key = keyword, index = date)
  }

  pal <- c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00",
           "#ca6702", "#bb3e03", "#ae2012", "#9b2226")

  # now estimate the decomposition
  dcmp <- ts_out %>%
    fabletools::model(stl = feasts::STL(hits))

  stl_features <- ts_out %>% fabletools::features(hits, feasts::feat_stl)
  # save the decomposition data
  stl_list <- list(decomp_data = fabletools::components(dcmp), stl_features = stl_features)
  writexl::write_xlsx(stl_list, path = file.path(directory$sub_data, glue::glue("{project}_stl_decomposition.xlsx")))

  # graph
  dcmp_plot <- function(term, directory) {
    p1 <- fabletools::components(dcmp) %>%
      dplyr::filter(keyword == term) %>%
      fabletools::autoplot(scale_bars = FALSE,
                           size = .8) +
      ggplot2::ggtitle(label = glue::glue("{term} - STL decomposition"),
                       subtitle = "hits = trend + season_year + remainder")
    fname <- glue::glue("{term}_decomp.png")
    ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "decomposition"),
                    dpi = "print", height = 8, width = 9, device = "png")

    p2 <- fabletools::components(dcmp) %>%
      tsibble::as_tsibble() %>%
      dplyr::filter(keyword == term) %>%
      fabletools::autoplot(hits, colour="black", size = .6) +
      ggplot2::geom_line(ggplot2::aes(y=season_adjust), colour = "blue", size = 1) +
      ggplot2::labs(x = NULL, y = "Search Interest (0-100)",
                    title = glue::glue("Seasonally Adjusted Search Interest for {term}"),
                    caption = "Seasonally adjusted search interest (blue)\nOriginal data (black)")
    fname <- glue::glue("{term}_seasadjust.png")
    ggplot2::ggsave(filename = fname, plot = p2, path = file.path(directory, "decomposition"),
                    dpi = "print", height = 8, width = 9, device = "png")

    p3 <- stl_features %>%
      ggplot2::ggplot(aes(x = trend_strength, y = seasonal_strength_year,
                          fill = keyword)) +
      ggplot2::geom_point(size = 10, pch = 21) +
      ggplot2::scale_fill_manual(values = pal, name = NULL) +
      ggplot2::labs(x = "Strength of Trend (0-1)", y = "Strength of Seasonality (0-1)",
                    caption = "Higher (lower) values indicate stronger (weaker) trend/seasonality in the data")
    fname <- glue::glue("STL_comparison.png")
    ggplot2::ggsave(filename = fname, plot = p3, path = file.path(directory, "decomposition"),
                    dpi = "print", height = 8, width = 9, device = "png")

  }
  terms <- tbl %>% dplyr::distinct(keyword) %>% dplyr::pull()
  purrr::walk2(terms, directory$sub_graph, dcmp_plot)

}
