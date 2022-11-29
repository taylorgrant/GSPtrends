#' Time Series Decompositions
#'
#' @description If the trends data is longer than 3 years in length, function runs an STL decomposition on each series.
#'
#' See Hyndman "Forecasting: Principles and Practice" \url{https://otexts.com/fpp3/stlfeatures.html} for details.
#'
#' @param tbl Data frame returned by Google Trends API
#' @param term Keyword to focus on for decompositions
#'
#' @return List of graphs - STL decomposition; Seasonally adjusted plot; Seasonality v Trend
#' @export
#'
#' @examples
#' \dontrun{
#' decomp(tbl, directory, project)}
ts_decomposition <- function(tbl, term) {

  if (stringr::str_detect(unique(tbl$time), "now")) {
    return()
  } else {
    if (tbl$date[2] - tbl$date[1] <= 7) {
      ts_out <- tbl  |>
        dplyr::mutate(date = tsibble::yearweek(date),
                      keyword = stringr::str_to_title(keyword)) |>
        tsibble::as_tsibble(key = keyword, index = date)

    } else {
      ts_out <- tbl |>
        dplyr::mutate(date = tsibble::yearmonth(date),
                      keyword = stringr::str_to_title(keyword)) |>
        tsibble::as_tsibble(key = keyword, index = date)
    }
  }

  if (range(tbl$date)[2] - range(tbl$date)[1] >= 1095) {

    pal1 <- palette_choice("bty")

    # estimate the decomposition
    dcmp <- ts_out |>
      fabletools::model(stl = feasts::STL(hits))

    stl_features <- ts_out |>  fabletools::features(hits, feasts::feat_stl)

    # graphing
    p1 <- fabletools::components(dcmp) |>
      dplyr::filter(keyword == term) |>
      fabletools::autoplot(scale_bars = FALSE,
                           size = .5,
                           color = "#36454F") +
      ggplot2::ggtitle(label = glue::glue("{term} - STL decomposition"),
                       subtitle = "hits = trend + season_year + remainder") +
      theme_xf() +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgray", color = NA)) +
      ggplot2::labs(x = "Timeframe", y = NULL)

    p1 <- plotly::ggplotly(p1) |>
      plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                 filename= 'image',
                                                 height= NULL,
                                                 width= NULL,
                                                 scale= 3)) |>
      plotly::layout(#margin = list(t = 105, b=110),
        title = list(text = paste0(term, ' - STL decomposition',
                                   '<br>',
                                   '<sup>',
                                   'hits = trend + season_year + remainder',
                                   '</sup>')))

    p2 <- fabletools::components(dcmp) |>
      tsibble::as_tsibble() |>
      dplyr::mutate(season_adjust = round(season_adjust, 1)) |>
      dplyr::filter(keyword == term) |>
      fabletools::autoplot(hits, size = .5,
                           color = "#36454F") +
      ggplot2::geom_line(ggplot2::aes(y=season_adjust), colour = "dodgerblue", size = .5) +
      ggplot2::labs(x = NULL, y = "Search Interest (0-100)") +
      theme_xf()

    p2 <- plotly::ggplotly(p2) |>
      plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                 filename= 'image',
                                                 height= NULL,
                                                 width= NULL,
                                                 scale= 3)) |>
      plotly::layout(margin = list(t = 105, b=110),
                     title = list(text = paste0('Seasonally adjusted search interest for: ', term,
                                                '<br>',
                                                '<sup>',
                                                'Seasonal adjusted (blue); Raw data (black)',
                                                '</sup>')))

    p3 <- stl_features |>
      ggplot2::ggplot(ggplot2::aes(x = trend_strength, y = seasonal_strength_year,
                                   fill = keyword,
                                   text = glue::glue("Keyword: {keyword}\n",
                                                     "Trend strength: {round(trend_strength, 2)}\n",
                                                     "Seasonal strength: {round(seasonal_strength_year,2)}"))) +
      ggplot2::geom_point(size = 10, pch = 21) +
      ggplot2::scale_fill_manual(values = pal1[seq.int(1, length(pal1), length.out = length(unique(tbl$keyword)))],
                                 name = NULL) +
      ggplot2::scale_x_continuous(limits = c(0,1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = "Strength of Trend (0-1)", y = "Strength of Seasonality (0-1)",
                    caption = "Higher (lower) values indicate stronger (weaker) trend/seasonality in the data") +
      theme_xf()

    p3 <- plotly::ggplotly(p3, tooltip = "text") |>
      plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                 filename= 'image',
                                                 height= NULL,
                                                 width= NULL,
                                                 scale= 3)) |>
      plotly::layout(margin = list(t = 105, b=110),
                     title = list(text = paste0('Comparing trend and seasonality',
                                                '<br>',
                                                '<sup>',
                                                'Higher (lower) values indicate stronger (weaker) trend/seasonality',
                                                '</sup>')))
  } else {
    set.seed(1234)
    df <- data.frame(x = rnorm(20), y = rnorm(20, 1,.5))
    p1 <- ggplot2::ggplot(df, ggplot2::aes(x,y)) +
      ggplot2::geom_blank() +ggplot2::theme_void() +
      ggplot2::annotate('text', x = 0, y = 1,
                        label = "Data must be over 3 years for a decomposition...")

    p2 <- p1
    p3 <- p1
  }

  list(tsp1 = p1,
       tsp2 = p2,
       tsp3 = p3)


}
