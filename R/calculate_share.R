#' Calculate share of search
#'
#'@description Calculates the share of search for each search term and date period.
#' Share is calculated for raw hits data as well as several moving averages (via Shiny sliderInput). Z-scores are also calculated for each time period and Keyword.
#'
#' @param tbl Data frame output from Google Trends API
#' @param ma Moving average to apply via Shiny sliderInput
#' @param pal Color palette selected by user in Shiny
#'
#' @return list containing 3 plots (share line, share area, zscore area) and 3 tables of data used
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_share(tbl)}
calculate_share <- function(tbl, ma, pal) {

  options(warn=-1) # kill verbose warnings

  # slider input
  ma_ba <- switch(ma, "0" = 0, "2" = 1, "3" = 1, "4" = 2, "5" = 2,
                  "6" = 3, "7" = 3, "8" = 4, "9" = 4, "10" = 5, "11" = 5,
                 "12" = 6, "13" = 6)

  line_pal <- c('#e6194B', '#3cb44b',  '#4363d8', '#f58231', '#42d4f4',
                '#469990', '#dcbeff', '#9A6324', '#800000', '#aaffc3', '#ffe119',
                '#000075', '#a9a9a9')

  pal1 <- palette_choice(pal)

  # set up the data for use in tables and plots
  out <- tbl |>
    dplyr::group_by(date) |>
    dplyr::mutate(raw_share = hits/sum(hits)) |>
    dplyr::group_by(keyword) |>
    dplyr::mutate(mov_avg = glue::glue("ma({ma})"))

  if ((as.numeric(ma) > 0) & as.numeric(ma) %% 2 == 0) {
    out <- out |>
      dplyr::mutate(ma_raw = slider::slide_dbl(hits, mean, .before = ma_ba-1, .after = ma_ba, .complete = TRUE),
                    ma_raw = slider::slide_dbl(ma_raw, mean, .before = 1, .after = 0, .complete = TRUE)) |>
      dplyr::group_by(date) |>
      dplyr::mutate(ma_share = ma_raw/sum(ma_raw)) |>
      dplyr::group_by(keyword) |>
      dplyr::mutate(z_ma_raw = scale(ma_raw),
                    keyword = stringr::str_to_title(keyword),
                    ma_raw = round(ma_raw, 1),
                    z_ma_raw = round(z_ma_raw, 2)) |>
      dplyr::ungroup()
  } else {
    out <- out |>
      dplyr::mutate(ma_raw = slider::slide_dbl(hits, mean, .before = ma_ba, .after = ma_ba, .complete = TRUE)) |>
      dplyr::group_by(date) |>
      dplyr::mutate(ma_share = ma_raw/sum(ma_raw)) |>
      dplyr::group_by(keyword) |>
      dplyr::mutate(z_ma_raw = scale(ma_raw),
                    keyword = stringr::str_to_title(keyword),
                    ma_raw = round(ma_raw, 1),
                    z_ma_raw = round(z_ma_raw, 2)) |>
      dplyr::ungroup()
  }
  out

  max <- out[out$date == max(out[stats::complete.cases(out),]$date),]
  sub1 <- dplyr::case_when(stringr::str_detect(unique(out$time), "H") ~ glue::glue("{ma} minute"),
                          unique(out$time) == "now 1-d" ~ glue::glue("{as.numeric(ma)*8} minute"),
                          unique(out$time) == "now 7-d" ~ glue::glue("{ma} hour"),
                          unique(out$time) == "today 1-m" ~ glue::glue("{ma} day"),
                          unique(out$time) == "today 3-m" ~ glue::glue("{ma} day"),
                          unique(out$time) == "today 12-m" ~ glue::glue("{ma} week"),
                          unique(out$time) == "today+5-y" ~ glue::glue("{ma} week"),
                          unique(out$time) == "all" ~ glue::glue("{ma} month"),
                          TRUE ~ glue::glue("{ma} day"))
  sub_title <- ifelse(as.numeric(ma) == 0, "Raw search trends", glue::glue("Trends data is smoothed with a rolling {sub1} moving average"))

  # share of search - line plot
  p1 <- ggplot2::ggplot(out,
                        ggplot2::aes(x = date, y = ma_share,
                                     group = keyword, color = keyword,
                                     text = glue::glue("Date: {date}\nKeyword: {keyword}\nShare: ",
                                                       "{scales::percent(ma_share, accuracy = 1)}"))) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_color_manual(values = pal1[seq.int(1, length(pal1), length.out = length(unique(out$keyword)))],
                                name = NULL) +
    ggplot2::labs(x = "Timeframe", y = "Share of search (%)") +
    theme_xf()

  if (stringr::str_detect(tbl$time[1], "now")) {
    p1 <- p1 +
      ggplot2::scale_x_datetime()
  } else {
    p1 <- p1 +
      ggplot2::scale_x_date()
  }

  # state or us
  if (stringr::str_detect(tbl$geo[1], "-")) {
    state_name <- states[states$abb == gsub(".*\\-", "", tbl$geo[1]),]$name
    p1txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                      "the greatest search share went to {max[which.max(max$ma_share),]$keyword} ",
                      "with {scales::percent(max[which.max(max$ma_share),]$ma_share)} \n",
                      "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}; State: {state_name}")
    p2txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                        "the greatest search share went to {max[which.max(max$ma_share),]$keyword} ",
                        "with {scales::percent(max[which.max(max$ma_share),]$ma_share)} \n",
                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}; State: {state_name}")
    p3txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                        "{max[which.max(max$ma_share),]$keyword} had the greatest share of search ",
                        "with {scales::percent(max[which.max(max$ma_share),]$ma_share)}.\n",
                        "Compared to its historical search volume over this timeframe, ",
                        "consumer search interest in {max[which.max(max$ma_share),]$keyword} ",
                        "is {ifelse(max[which.max(max$ma_share),]$z_ma_raw > 0, 'above average.', 'below average.')}\n",
                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}; State: {state_name}")
  } else {
    p1txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                        "the greatest search share went to {max[which.max(max$ma_share),]$keyword} ",
                        "with {scales::percent(max[which.max(max$ma_share),]$ma_share)} \n",
                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]};")
    p2txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                        "the greatest search share went to {max[which.max(max$ma_share),]$keyword} ",
                        "with {scales::percent(max[which.max(max$ma_share),]$ma_share)} \n",
                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}")
    p3txt <- glue::glue("Source: Google\nRead as: As of {max[which.max(max$ma_share),]$date} ",
                        "{max[which.max(max$ma_share),]$keyword} had the greatest share of search ",
                        "with {scales::percent(max[which.max(max$ma_share),]$ma_share)}.\n",
                        "Compared to its historical search volume over this timeframe, ",
                        "consumer search interest in {max[which.max(max$ma_share),]$keyword} ",
                        "is {ifelse(max[which.max(max$ma_share),]$z_ma_raw > 0, 'above average.', 'below average.')}\n",
                        "Trends timeframe: {range(out$date)[1]} to {range(out$date)[2]}")
  }

  # converting to plotly
  p1 <- plotly::ggplotly(p1, tooltip = "text") |>
    plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'rename_image',
                                               height= NULL,
                                               width= NULL,
                                               scale= 3)) |>
    plotly::layout(margin = list(t = 105, b=130),
                   annotations = list(x = 1.05, y = -0.45,
                                      text = p1txt,
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='right', align="right",
                                      font=list(size=10)),
                   title = list(text = paste0('Share of search',
                                              '<br>',
                                              '<sup>',
                                              sub_title,
                                              '</sup>')))

  # share of search - area plot
  p2 <- ggplot2::ggplot(out, ggplot2::aes(x = date, y = ma_share,
                                          group = keyword, fill = keyword,
                                          text = glue::glue("Date: {date}\nKeyword: {keyword}\nShare: ",
                                                            "{scales::percent(ma_share, accuracy = 1)}"))) +
    ggplot2::geom_area(stat = "identity",
                       color = 'white',
                       size = .1,
                       alpha = .9) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = pal1[seq.int(1, length(pal1), length.out = length(unique(out$keyword)))],
                                name = NULL) +
    ggplot2::labs(x = "Timeframe", y = "Share of search (%)") +
    theme_xf(grid = FALSE)

  if (stringr::str_detect(tbl$time[1], "now")) {
    p2 <- p2 +
      ggplot2::scale_x_datetime()
  } else {
    p2 <- p2 +
      ggplot2::scale_x_date()
  }
  # converting to plotly
  p2 <- plotly::ggplotly(p2, tooltip = "text") |>
    plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'rename_image',
                                               height= NULL,
                                               width= NULL,
                                               scale= 3)) |>
    plotly::layout(margin = list(t = 105, b=130),
                   annotations = list(x = 1.05, y = -0.45,
                                      text = p2txt,
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='right', align="right",
                                      font=list(size=10)),
                   title = list(text = paste0('Share of search',
                                              '<br>',
                                              '<sup>',
                                              sub_title,
                                              '</sup>')))


  # z-score - area
  zsub_title <- ifelse(as.numeric(ma) == 0, "", glue::glue("Rolling {sub1} change in Google search; noise and seasonality removed"))

  p3 <- ggplot2::ggplot(out, ggplot2::aes(x = date, y = z_ma_raw,
                                          group = keyword, fill = keyword,
                                          text = glue::glue("Date: {date}\nKeyword: {keyword}\nZ-Score: ",
                                                            "{z_ma_raw}"))) +
    ggplot2::geom_area(stat = "identity",
                       color = 'white',
                       size = .05,
                       alpha = .9) +
    ggplot2::scale_fill_manual(values = pal1[seq.int(1, length(pal1), length.out = length(unique(out$keyword)))],
                               name = NULL) +
    ggplot2::labs(x = "Timeframe", y = glue::glue("Rolling {sub1} change in search activity (z-score, stacked)")) +
    theme_xf(grid = FALSE)

  if (stringr::str_detect(tbl$time[1], "now")) {
    p3 <- p3 +
      ggplot2::scale_x_datetime()
  } else {
    p3 <- p3 +
      ggplot2::scale_x_date()
  }
  # converting to plotly (using config to set higher res image download)
  p3 <- plotly::ggplotly(p3, tooltip = "text") |>
    plotly::config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'rename_image',
                                               height= NULL,
                                               width= NULL,
                                               scale= 3)) |>
    plotly::layout(margin = list(t=105,b=130),
                   annotations = list(x = 1.05, y = -0.45,
                                      text = p3txt,
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='right', align="right",
                                      font=list(size=10)),
                   title = list(text = paste0('Consumer interest',
                                              '<br>',
                                              '<sup>',
                                              zsub_title,
                                              '</sup>'), align = 'left'))

  # table output
  tbl_out <- function(metric) {
    if (metric == "ma_raw") {
      cap <- ifelse((as.numeric(ma) == 0), "Raw search volume", glue::glue("Rolling {sub1} moving average of search volume"))
      } else if (metric == "ma_share") {
        cap <- ifelse((as.numeric(ma) == 0), "Raw share of search", glue::glue("Rolling {sub1} moving average of share of search"))
        } else {
          cap <- ifelse((as.numeric(ma) == 0), "Raw change in Google search volume (Z-Score)",
                        glue::glue("Rolling {sub1} change in Google search volume (Z-Score)"))
    }

    out <- out |>
      dplyr::mutate(ma_share = scales::percent(ma_share, accuracy = 1)) |>
      dplyr::select(date, keyword, dplyr::all_of(metric)) |>
      tidyr::pivot_wider(names_from = keyword,
                         values_from = metric)

    if (stringr::str_detect(tbl$time[1], "now")) {
      out <- out |>
        dplyr::mutate(timeframe = format(as.POSIXct(date), format = "%H:%M:%S"),
                      date = as.Date(date)) |>
        dplyr::relocate(timeframe, .after = "date")
    } else {
      out <- out |>
        dplyr::mutate(date = as.Date(date))
    }
    out |>
      DT::datatable(
        extensions = "Buttons",
        filter = list(position = "top", clear = FALSE),
        rownames = FALSE,
        options = list(
          # searching = FALSE,
          dom = "Blftp",
          buttons = c('excel')),
        caption = cap)
  }

  ma_raw_table <- tbl_out('ma_raw')
  ma_share_table <- tbl_out('ma_share')
  zma_raw <- tbl_out('z_ma_raw')


  list(share_line = p1,
       share_area = p2,
       zscore_area = p3,
       ma_raw_table = ma_raw_table,
       ma_share_table = ma_share_table,
       zma_raw_table = zma_raw)
}
