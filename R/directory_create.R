#' Create directories and sub-directories
#'
#' @param main_dir 1st level folder where data is stored
#' @param arg_list Inputs from the user in list form
#' @param permiss Where to set up directory
#' @param data Data frame returned from Google Trends API
#' @param projname Main folder; defined by user input
#'
#' @return NULL folders are set up in specific location
#' @export
#'
#' @examples
#' \dontrun{
#' directory_create(main_dir, arg_list, permiss, data)}
directory_create <- function(projname, main_dir, arg_list, permiss, data) {
  # helper function
  dir_create <- function(x) ifelse(!dir.exists(x), dir.create(x, recursive = TRUE), FALSE)

  if (permiss == "I want to provide a new directory location") {
    main_dir <- readline(cat(crayon::cyan("Provide a new directory ("), crayon::red("DO NOT"), crayon::cyan("wrap in quotes):")))
    main_dir <- file.path(main_dir, projname)
    dir_create(main_dir)
    sub_dir <- file.path(main_dir, "interest_over_time", glue::glue("{length(arg_list$kw)}_terms-{arg_list$duration}-{arg_list$gprop}"))
    # subdirectories
    sub_graph <- file.path(sub_dir, 'graphs')
    sub_data <- file.path(sub_dir, 'data')
    c(sub_dir, sub_graph, sub_data) %>%
      purrr::walk(dir_create)
    c(file.path(sub_graph, "area"), file.path(sub_graph, "line"),
      file.path(sub_graph, "single_term")) %>%
      purrr::walk(dir_create)
    if (arg_list$duration %in% c("today+5-y", "all") |
        range(data$date)[2] - range(data$date)[1] >= 1095) {
      dir_create(file.path(sub_graph, 'decomposition'))
    }
    # write about.txt file to folder with names of search terms
    bodytext <- c("\nDate run:", as.character(Sys.time()), "\nSearch terms used for this analysis:", arg_list$kw,  "\nTimeframe:",arg_list$duration, "\nTrend type:", arg_list$gprop, "\nDates covered:", as.character(range(data$date)), "\nAll times PST/PDT")
    fileConn <- file(file.path(sub_dir, "about.txt"))
    writeLines(bodytext, fileConn)
    close(fileConn)
    directory <- list(sub_graph = sub_graph,
                      sub_data = sub_data)
  } else {
    dir_create(main_dir)
    sub_d <- glue::glue("{length(arg_list$kw)}_terms")
    sub_dir <- file.path(main_dir, "interest_over_time", glue::glue("{length(arg_list$kw)}_terms-{arg_list$duration}-{arg_list$gprop}"))
    # subdirectories
    sub_graph <- file.path(sub_dir, 'graphs')
    sub_data <- file.path(sub_dir, 'data')
    c(sub_dir, sub_graph, sub_data) %>%
      purrr::walk(dir_create)
    c(file.path(sub_graph, "area"), file.path(sub_graph, "line"),
      file.path(sub_graph, "single_term")) %>%
      purrr::walk(dir_create)
    # write about.txt file to folder with names of search terms
    bodytext <- c("\nDate run:", as.character(Sys.time()), "\nSearch terms used for this analysis:", arg_list$kw, "\nCategory used (if any):", arg_list$category_name, "\nTimeframe:",arg_list$duration, "\nTrend type:", arg_list$gprop, "\nDates covered:", as.character(range(data$date)),"\nAll times PST/PDT")
    fileConn <- file(file.path(sub_dir, "about.txt"))
    writeLines(bodytext, fileConn)
    close(fileConn)
    directory <- list(sub_graph = sub_graph,
                      sub_data = sub_data)
    if (arg_list$duration %in% c("today+5-y", "all") |
        range(data$date)[2] - range(data$date)[1] >= 1095) {
      dir_create(file.path(sub_graph, 'decomposition'))
    }
  }
  return(directory)
}
