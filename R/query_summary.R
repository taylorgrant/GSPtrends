#' Summarizing Raw Google Trends Data
#'
#' @param tbl Raw data pulled via the gtrendsR package
#'
#' @return list with two tables (wide summary and summary stats) and basic line plot
#' @export
#'
#' @examples
#' \dontrun{
#' query_summary(tbl)
#' }
query_summary <- function(tbl) {

  # wide summary table
  out <- tbl |>
    dplyr::mutate(keyword = stringr::str_to_title(keyword)) |>
    tidyr::pivot_wider(
      names_from = keyword,
      values_from = hits) |>
    dplyr::select(-c(gprop, geo, category, time))

  if (stringr::str_detect(tbl$time[1], "now")) {

    out <- out |>
      dplyr::mutate(timeframe = format(as.POSIXct(date), format = "%H:%M:%S"),
                    date = as.Date(date)) |>
      dplyr::relocate(timeframe, .after = "date")
  }

  # introductory line plot
  line_pal <- c('#e6194B', '#3cb44b',  '#4363d8', '#f58231', '#42d4f4',
                '#469990', '#dcbeff', '#9A6324', '#800000', '#aaffc3', '#ffe119',
                '#000075', '#a9a9a9')

  # state or us
  if (stringr::str_detect(tbl$geo[1], "-")) {
    state_name <- states[states$abb == gsub(".*\\-", "", tbl$geo[1]),]$name
    txt <- glue::glue("Source: Google\nTrends timeframe: {range(tbl$date)[1]} - {range(tbl$date)[2]} ",
               "\nSearch type: {unique(tbl$gprop)}; ",
               "Category: {categories[categories$id == unique(tbl$category),]$name}; State: {state_name} ")
  } else {
    txt <- glue::glue("Source: Google\nTrends timeframe: {range(tbl$date)[1]} - {range(tbl$date)[2]} ",
                      "\nSearch type: {unique(tbl$gprop)}; ",
                      "Category: {categories[categories$id == unique(tbl$category),]$name}")
  }

  p1 <- tbl |>
    dplyr::mutate(keyword = stringr::str_to_title(keyword)) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = hits, group = keyword, color = keyword,
                                 text = glue::glue("Date: {date}\nKeyword: {keyword}\nHits: {hits}"))) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = line_pal,
                                name = NULL) +
    theme_xf() +
    ggplot2::labs(x = "Timeframe", y = "Search Volume (0-100)",
                  title = "Google Trends - Raw Data")

  if (stringr::str_detect(tbl$time[1], "now")) {
    p1 <- p1 +
      ggplot2::scale_x_datetime()
  } else {
    p1 <- p1 +
      ggplot2::scale_x_date()
  }

  p1 <- plotly::ggplotly(p1, tooltip = "text") |>
    plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                    filename= 'image',
                                                    height= NULL,
                                                    width= NULL,
                                                    scale= 3)) |>
    plotly::layout(margin = list(t=105,b=130),
                   annotations = list(x = 1.05, y = -0.45,
                          text = txt,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', align="right",
                          font=list(size=10)))

  # summary stat table
  out2 <- tbl |>
    dplyr::mutate(keyword = stringr::str_to_title(keyword)) |>
    dplyr::group_by(keyword) |>
    dplyr::summarise(n = dplyr::n(),
                     min = round(min(hits), 2),
                     avg = round(mean(hits),2),
                     max = round(max(hits),2),
                     s = round(stats::sd(hits),2)) |>
    dplyr::mutate(margin = stats::qt(0.975, df = n-1)*s/sqrt(n),
                  low = round(avg - margin,2),
                  hi = round(avg + margin, 2)) |>
    dplyr::select(-margin) |>
    tidyr::unite(col = "ci", c("low", "hi"), sep = " - ") |>
    dplyr::arrange(dplyr::desc(avg))

  list(table = out,
       plot = p1,
       table2 = out2)

  }

