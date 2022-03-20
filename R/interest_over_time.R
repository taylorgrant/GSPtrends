#' Total function to call Google Trends Interest Over Time
#'
#' @description User is put through a flow of questions. All answers are saved to a list of arguments that are then used to query the Google Trends API.
#'
#' Function automatically selects the working directory as the location to save all data and graphs, but user has option to change directory location.
#'
#' @return Nothing is returned. Graphs and Excel workbooks are saved to directory locations.
#' @export
#'
#' @examples
#' \dontrun{
#' interest_over_time()}
interest_over_time <- function() {

  data(categories, envir = environment())
  # project name for building directory
  projname <- readline(cat(crayon::bgCyan("What is the name of the client or project?")))
  # keywords
  cat(crayon::bgCyan$bold("Enter the terms you'd like to query from Google Trends\n"))
  keywords <- readline(cat(crayon::cyan("Queries (separate each with a comma): ")))
  kw <- bare_combine(keywords)
  kw <- unique(kw)

  # date range (provided for info)
  d_range <- tibble::tibble(Name = c("now 1-H", "now 4-H", "now 1-d", "now 7-d", "today 1-m", "today 3-m",
                                     "today 12-m", "today+5-y", "all", 'Specify range'),
                            `Time Range` = c("Last 1 hour", "Last 4 hours", "Last 24 hours", "Last 7 days",
                                             "Last month", "Last 3 months", "Last 12 months", "Last 5 years",
                                             "2004 to present", "User defined"),
                            `Time Interval` = c("1 minute", "1 minute", "8 minutes", "1 hour",
                                                "1 day", "1 day", "7 days", "7 days", "1 month", "Depends"))
  tf <- c("now 1-H", "now 4-H", "now 1-d", "now 7-d", "today 1-m", "today 3-m",
          "today 12-m", "today+5-y", "all", 'Specify range')
  print(d_range)
  duration <- utils::select.list(title = cat(crayon::bgCyan("\nTake a look above. What is the time frame/date range of interest?\n")),
                                 choices = tf, multiple = FALSE)
  if (duration == "Specify range") {
    duration <- readline(cat(crayon::bgCyan("Please enter a range in format of: yyyy-mm-dd yyyy-mm-dd: ")))
    if (lubridate::year(gsub("\\ .*", "", duration)) < 2004) {
      stop("Google Trends data only begins in 2004. Please start over.")
    }
  }

  # what to search for
  gprop <- utils::select.list(title = cat(crayon::bgCyan("Where to search?")),
                              choices = c("web", "news", "images", "froogle", "youtube"),
                              multiple = FALSE)
  # category
  category <- utils::select.list(title = cat(crayon::bgCyan("Would you like to specify a category?")),
                                 choices = c("Yes", "No"), multiple = FALSE)
  if (category == "Yes") {
    term <- readline(cat(crayon::bgCyan("Enter a search term to narrow down the category:")))
    cs <- cat_search(term)
    cat_choice <- utils::select.list(title = cat(crayon::bgCyan("Do any of these work?")),
                                     choices = c(cs$name, "None"), multiple = FALSE)

    if (cat_choice == "None") {
      category <- utils::select.list(title = cat(crayon::bgCyan("Would you like to search again?")),
                                     choices = c("Yes", "No"), multiple = FALSE)
      if (category == "Yes") {
        term <- readline(cat(crayon::white("Enter a search term to narrow down the category")))
        cs <- cat_search(term)
        cat_choice <- utils::select.list(title = cat(crayon::bgCyan("Do any of these work?")),
                                         choices = c(cs$name, "None"), multiple = FALSE)

        if (cat_choice == "None") {
          cat(crayon::white("Ignoring category and running generic search...\n"))
          category <- 0
          cat_choice <- "None"
        } else {
          category <- cs[cs$name == cat_choice, ]$id
        }
      }
    } else {
      category <- cs[cs$name == cat_choice, ]$id
    }
  } else {
    category <- 0
    cat_choice <- "None"
  }

  # arguments go into a list
  arg_list <- list(kw = kw,
                   duration = duration,
                   gprop = gprop,
                   category = category,
                   category_name = cat_choice
  )

  # ask for a check on the work
  cat(crayon::bold("\nMake sure these inputs are correct:\n\n"))
  cat(crayon::cyan("The search terms are:"), crayon::yellow$bold(arg_list$kw))
  cat(crayon::cyan("\nThe date range is:"), crayon::yellow$bold(arg_list$duration))
  cat(crayon::cyan("\nWe're searching for trends on:"), crayon::yellow$bold(arg_list$gprop))
  cat(ifelse(arg_list$category == 0, crayon::cyan("\nWe're not using a category\n\n"),
             paste0(crayon::cyan("\nWe're using the following category for search context: "),
                    crayon::yellow$bold(cs[cs$name == cat_choice,]$name), "\n\n")))
  chk <- utils::select.list(title = cat(crayon::bgCyan("Is this correct?")),
                            choices = c("Yes", "No. Start over"), multiple = FALSE)
  if (chk == "No. Start over") {
    stop("Starting over...\n")
  }
  # query google trends
  out <- pull_gtr(arg_list)

  cat(crayon::cyan("You ran searches for", crayon::yellow$bold(length(unique(out$keyword))), "keywords, returning", crayon::yellow$bold(out %>% dplyr::count(keyword) %>% dplyr::distinct(n)),"results for each term.\n"))
  cat(crayon::cyan("The range of your query was", crayon::yellow$bold(range(out$date)[1]), "to", crayon::yellow$bold(range(out$date)[2]), "\n"))
  cat(crayon::cyan("The gap in time between datapoints is a", tolower(as.character(crayon::yellow$bold(capture.output(out$date[2] - out$date[1]))))))

  # directories
  main_dir <- file.path(here::here(), projname)
  cat(crayon::cyan("\n\nData and graphs are going to be put in a new folder in the following location:\n"),
      main_dir)
  permiss <- utils::select.list(title = cat(crayon::bgCyan("\n\nIs this okay?")),
                                choices = c("Yes", "I want to provide a new directory location"), multiple = FALSE)
  # create main and subs
  directory <- directory_create(projname, main_dir, arg_list, permiss, out)

  # data processing
  calc_out <- share_calc(out)
  corr_tbl <- gtr_corr(out, directory$sub_graph)

  # graphs
  cat(crayon::cyan("Creating graphs of the data...\n"))
  plotvars <- c("hits","raw_share", "ma_share", "z_ma_raw", "ma_raw")
  purrr::pwalk(list(tbl = list(calc_out), var = plotvars,
                    directory = directory$sub_graph,
                    args = arg_list$category_name), make_plots)
  make_ind_plot(calc_out, arg_list$category_name, directory$sub_graph)

  # time series decomposition
  if (arg_list$duration %in% c("today+5-y", "all") |
      range(out$date)[2] - range(out$date)[1] >= 1095) {
    decomp(out, directory, projname)
  }

  # wrap up and save data
  cat(crayon::cyan("Saving the data to an Excel file...\n"))
  data_out <- data_process(out, calc_out, corr_tbl, arg_list)
  writexl::write_xlsx(data_out, path = file.path(directory$sub_data, glue::glue("{projname}_excel_output.xlsx")))
  cat(crayon::cyan("All done.\nCheck your folders", crayon::yellow$bold(directory[1]), "for graphs and\n", crayon::yellow$bold(directory[2]), "for data.\n"))

}
