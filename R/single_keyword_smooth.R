#' Single term smoothed
#'
#' @param tbl Data frame with Google Trends data
#' @param term Term to isolate
#' @param ma Level of moving average
#'
#' @return graph with raw hits value and overlayed moving average line
#' @export
#'
#' @examples
#' \dontrun{
#' single_keyword_smooth(tbl, "cats", ma = 7)
#' }
single_keyword_smooth <- function(tbl, term, ma) {

  # slider input
  ma_ba <- switch(ma, "0" = 0, "2" = 1, "3" = 1, "4" = 2, "5" = 2,
                  "6" = 3, "7" = 3, "8" = 4, "9" = 4, "10" = 5, "11" = 5,
                  "12" = 6, "13" = 6)

  # set up data for use
  if ((as.numeric(ma) > 0) & as.numeric(ma) %% 2 == 0) {
    out <- tbl |>
      dplyr::mutate(keyword = stringr::str_to_title(keyword)) |>
      dplyr::filter(keyword == term) |>
      dplyr::mutate(ma_raw = slider::slide_dbl(hits, mean, .before = ma_ba-1, .after = ma_ba, .complete = TRUE),
                    ma_raw = slider::slide_dbl(ma_raw, mean, .before = 1, .after = 0, .complete = TRUE),
                    ma_raw = round(ma_raw, 1))
  } else {
    out <- tbl |>
      dplyr::mutate(keyword = stringr::str_to_title(keyword)) |>
      dplyr::filter(keyword == term) |>
      dplyr::mutate(ma_raw = slider::slide_dbl(hits, mean, .before = ma_ba, .after = ma_ba, .complete = TRUE),
                    ma_raw = round(ma_raw, 1))
  }

  # pull data for tooltip
  sub1 <- dplyr::case_when(stringr::str_detect(unique(out$time), "H") ~ glue::glue("{ma} minute"),
                           unique(out$time) == "now 1-d" ~ glue::glue("{as.numeric(ma)*8} minute"),
                           unique(out$time) == "now 7-d" ~ glue::glue("{ma} hour"),
                           unique(out$time) == "today 1-m" ~ glue::glue("{ma} day"),
                           unique(out$time) == "today 3-m" ~ glue::glue("{ma} day"),
                           unique(out$time) == "today 12-m" ~ glue::glue("{ma} week"),
                           unique(out$time) == "today+5-y" ~ glue::glue("{ma} week"),
                           unique(out$time) == "all" ~ glue::glue("{ma} month"),
                           TRUE ~ glue::glue("{ma} day"))
  sub_title <- ifelse(as.numeric(ma) == 0, "Raw search trends", glue::glue("Blue line is a rolling {sub1} moving average"))

  # plot
  p1 <- ggplot2::ggplot(out, ggplot2::aes(x = date, y = hits, group = 1,
                                     text = glue::glue("Keyword: {stringr::str_to_title(term)}\n",
                                                       "Hits: {hits}\n",
                                                       "MA: {ma_raw}"))) +
    ggplot2::geom_line(alpha = .5, size = .5) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = ma_raw), color = "blue", size = .7) +
    ggplot2::labs(x = "Timeframe", y = "Search Interest (0-100)") +
    theme_xf()

  # converting to plotly
  p1 <- plotly::ggplotly(p1, tooltip = "text") |>
    plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'image',
                                               height= NULL,
                                               width= NULL,
                                               scale= 3)) |>
    plotly::layout(margin = list(t = 105, b=130),
                   annotations = list(x = 1.0, y = -0.45,
                                      text = glue::glue("Source: Google\n",
                                                        "Note that search interest may not hit 100 as data is scaled\n",
                                                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}"),
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='right', align="right",
                                      font=list(size=10)),
                   title = list(text = paste0(stringr::str_to_title(term), ': search interest',
                                              '<br>',
                                              '<sup>',
                                              sub_title,
                                              '</sup>')))

}
