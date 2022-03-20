#' Plots for individual keywords
#'
#' @description Plots for each keyword; raw data with a hard-coded MA5
#'
#' @param tbl Data frame returned by Google Trends API
#' @param args Various arguments compiled from user input
#' @param directory Location to save graphs, provided by user
#'
#' @return png files saved to specified directory
#' @export
#'
#' @examples
#' \dontrun{
#' make_ind_plot(tbl, args, directory)}
make_ind_plot <- function(tbl, args, directory) {
  # pull for the MA (hard code the MA5)
  ma_agg <- "ma5"
  ma_sub <- ma_subtitle(tbl, ma_agg)

  # get the individual keywords
  terms <- tbl %>%
    dplyr::distinct(keyword) %>%
    dplyr::pull()

  gfunc2 <- function(key, args, directory) {
    p1 <- tbl %>%
      dplyr::filter(keyword == key) %>%
      dplyr::filter(ma == "ma5") %>%
      ggplot2::ggplot(aes(x = date, y = hits)) +
      ggplot2::geom_line(alpha = .5) +
      ggplot2::scale_x_datetime(date_breaks = "12 months",
                       date_labels = "%b %y") +
      ggplot2::geom_line(aes(x = date, y = ma_raw), color = "blue", size = 1) +
      ggplot2::labs(x = NULL, y = "Search Interest (0-100)",
           title = glue::glue("Search interest in {key}"),
           caption = glue::glue("Source: Google\nBlue line is the {ma_sub}\nCategory searched: {args}\nTimeframe: {range(tbl$date)[1]} to {range(tbl$date)[2]}"))
    fname <- glue::glue("{key}_line_{ma_agg}.png")
    ggplot2::ggsave(filename = fname, plot = p1, path = file.path(directory, "single_term"),
                    dpi = "print", height = 8, width = 9, device = "png")
  }
  purrr::pmap(list(terms, args, directory), gfunc2)
}
