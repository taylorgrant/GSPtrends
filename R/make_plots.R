#' Graph the Google Trends data
#'
#' @description Multiple graphs with all keywords included in the data
#'
#' @param tbl Data frame returned by Google Trends API
#' @param var Variable of choice for graphing
#' @param directory Where to save the graphs
#' @param args Additional data for captions and labels
#'
#' @return png files saved to graph subfolders
#' @export
#'
#' @examples
#' \dontrun{
#' make_plots(tbl, var, directory, args)}
make_plots <- function(tbl, var, directory, args) {

  # y variables of interest
  yvar <- c("ma_share", "z_ma_raw", "ma_raw")
  ma_options <- c("ma3", "ma5", "ma7", "ma13")
  hit_options <- tibble::tibble(yvar = c("hits", "raw_share"), ma_agg = "ma3")

  # switch info for graphs
  switch_title <- switch(var, "hits" = "Raw Search Interest Over Time", "ma_raw" = "Smoothed Search Interest Over Time",
                         "raw_share" = "Raw Share of Search Over Time", "ma_share" = "Smoothed Share of Search Over Time",
                         "z_ma_raw" = "Smoothed Changes in Consumer Interest Over Time")
  switch_nm <- switch(var, "hits" = "raw_interest", "ma_raw" = "smoothed_interest",
                      "raw_share" = "raw_share", "ma_share" = "smoothed_share",
                      "z_ma_raw" = "z_score")

  # create tibble to pmap over
  plotinfo <- tidyr::crossing(yvar, ma_agg = ma_options) %>%
    dplyr::bind_rows(hit_options) %>%
    dplyr::filter(yvar == var) %>%
    dplyr::mutate(title = switch_title)

  gfunc <- function(tbl, yvar, ma_agg, title, args) {
    # disable warnings for MA graphs that lose values on either side of the MA
    options(warn=-1)

    # color palette
    pal <- c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00",
             "#ca6702", "#bb3e03", "#ae2012", "#9b2226")

    # create subtitles with MA length
    switch_subtitle <- switch(yvar, "hits" = NULL, "raw_share" = NULL,
                              "ma_share" = ma_subtitle(tbl, ma_agg),
                              "ma_raw" = ma_subtitle(tbl, ma_agg),
                              "z_ma_raw" = glue::glue("Rolling {gsub('\\ .*', '', ma_subtitle(tbl, ma_agg))} change in Google search"))

    caption <- glue::glue("Source: Google\nCategory searched: {args}\nTimeframe: {range(tbl$date)[1]} to {range(tbl$date)[2]}")
    zlang <- gsub('\\ .*', '', ma_subtitle(tbl, ma_agg))
    # to pass argument into ggplot
    quo_var <- rlang::sym(yvar)

    # plot version (line)
    p_line <- tbl %>%
      dplyr::filter(ma == ma_agg) %>%
      dplyr::mutate(ifelse(stringr::str_detect(keyword, "[[:upper:]]"), keyword, stringr::str_to_title(keyword))) %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = !!quo_var, group = keyword, color = keyword)) +
      ggplot2::geom_line(size = .8) +
      ggplot2::scale_color_manual(values = pal, name = NULL) +
      ggplot2::theme(legend.position = "right") +
      ggplot2::scale_x_datetime(
        sec.axis = ggplot2::dup_axis(~., name = NULL)
      ) +
      ggplot2::labs(x = NULL, y = "Search Interest (0-100)",
                    title = switch_title,
                    subtitle = switch_subtitle,
                    caption = caption)

    # plot version (area)
    p_area <- tbl %>%
      dplyr::filter(ma == ma_agg) %>%
      dplyr::mutate(ifelse(stringr::str_detect(keyword, "[[:upper:]]"), keyword, stringr::str_to_title(keyword))) %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = !!quo_var, group = keyword, fill = keyword)) +
      ggplot2::geom_area(stat = "identity",
                         color = 'white',
                         size = .1,
                         alpha = .9) +
      ggplot2::scale_fill_manual(values = pal,
                                 name = NULL) +
      ggplot2::labs(x = NULL, y = "% Share of Search Interest",
                    title = switch_title,
                    subtitle = switch_subtitle,
                    caption = caption)

    if (yvar == "hits") {
      p1 <- p_line
      fname <- glue::glue("{switch_nm}.png")
      ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "line"),
                      dpi = "print", height = 8, width = 9, device = "png")
    } else if (yvar == "ma_raw") {
      p1 <- p_line
      fname <- glue::glue("{switch_nm}_{ma_agg}.png")
      ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "line"),
                      dpi = "print", height = 8, width = 9, device = "png")
    } else if (yvar == "raw_share") {
      p1 <- p_area +
        ggplot2::scale_y_continuous(labels = scales::percent)
      fname <- glue::glue("{switch_nm}.png")
      ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "area"),
                      dpi = "print", height = 8, width = 9, device = "png")

      p2 <- p_line +
        ggplot2::scale_y_continuous(labels = scales::percent)
      fname <- glue::glue("{switch_nm}.png")
      ggplot2::ggsave(filename = fname, plot = p2, path = file.path(directory, "line"),
                      dpi = "print", height = 8, width = 9, device = "png")
    } else if (yvar == "ma_share") {
      p1 <- p_area +
        ggplot2::scale_y_continuous(labels = scales::percent)
      fname <- glue::glue("{switch_nm}_{ma_agg}.png")
      ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "area"),
                      dpi = "print", height = 8, width = 9, device = "png")

      p2 <- p_line +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("% Share of Search Interest")
      fname <- glue::glue("{switch_nm}_{ma_agg}.png")
      ggplot2::ggsave(filename = fname, plot = p2, path = file.path(directory, "line"),
                      dpi = "print", height = 8, width = 9, device = "png")
    } else {
      p1 <- p_area +
        ggplot2::ylab(glue::glue("Rolling {zlang} change in search activity (z-score, stacked)"))
      fname <- glue::glue("{switch_nm}_{ma_agg}.png")
      ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "area"),
                      dpi = "print", height = 8, width = 9, device = "png")
    }
  }
  purrr::pmap(list(tbl = list(tbl), yvar = plotinfo$yvar,
                   ma_agg = plotinfo$ma_agg, title = plotinfo$title,
                   args), gfunc)
}
