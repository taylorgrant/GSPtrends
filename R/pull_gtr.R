#' Query Google Trends API
#'
#' @description
#' Convenience wrapper for the \code{\link[gtrendsR]{gtrends}} function.
#'
#' If there are more than 5 terms to query, function will ensure that the most popular term is in each query.
#'
#' \strong{Note:} Max of 9 terms; geo is hard coded to the US for now
#'
#' @param arg_list list of arguments to use in API call; arguments provided by user through the interactive interface and automatically wrapped into a list
#'
#' @return Data frame returned from Google Trends API
#' @export
#'
#' @examples
#' \dontrun{
#' pull_gtr(arg_list)}
pull_gtr <- function(arg_list) {
  # split out search terms based on length (5 max)
  if (length(arg_list$kw) > 9) {
    stop("This function isn't ready for more than 9 search queries yet. Apologies")
  } else if (length(arg_list$kw) > 5 && length(arg_list$kw) <= 9) {
    kw1 <- arg_list$kw[1:5]
    kw2 <- arg_list$kw[6:length(arg_list$kw)]


    # QUERY TRENDS API --------------------------------------------------------------
    # run first search
    cat(crayon::cyan("Running the first query...\n"))
    s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time %>%
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))

    # find top performer
    top_s1 <- find100(s1)

    # add to original second query
    kw2 <- c(top_s1$keyword, kw2)
    # run second search
    cat(crayon::cyan("Running the second query...\n"))
    s2 <- gtrendsR::gtrends(keyword = dput(kw2),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time %>%
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))

    # check to make sure the same keyword is the 100
    top_s2 <- find100(s2)

    if (top_s1$keyword == top_s2$keyword) {
      cat(crayon::cyan("\nThe", crayon::yellow$bold(top_s1$keyword), "query had the peak search volume in both searches...\n"))
      # drop redundant keyword
      s2 <- s2 %>%
        dplyr::filter(keyword != top_s1$keyword)

      # bind both searches together
      # out <<- s1 %>%
      out <- s1 %>%
        dplyr::bind_rows(s2)
      if (stringr::str_detect(arg_list$duration, "now")) {
        attr(out, "tzone") <- "GMT"
        out <- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
        # out <<- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
      }
    } else {
      cat(crayon::cyan("\nThe", crayon::yellow$bold(top_s1$keyword), "query had the peak search volume in the 1st search...\n"))
      cat(crayon::cyan("The", crayon::yellow$bold(top_s2$keyword), "query had the peak search volume in the 2nd search.\n"))
      cat(crayon::cyan("Re-running with", crayon::yellow$bold(top_s2$keyword), "in both...\n"))
      # drop top_s1 and add top_s2
      kw1_b <- c(top_s2$keyword, kw1[-which(kw1 == top_s1$keyword)])
      # add top_s1 to 2nd search
      kw2_b <- kw2

      # re-run first search
      s1 <- gtrendsR::gtrends(keyword = dput(kw1_b),
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time %>%
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))

      # re-run second search
      s2 <- gtrendsR::gtrends(keyword = dput(kw2_b),
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time %>%
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))

      # put them together
      s2 <- s2 %>%
        dplyr::filter(keyword != top_s2$keyword)

      # out <<- s1 %>%
      out <- s1 %>%
        dplyr::bind_rows(s2)
      if (stringr::str_detect(arg_list$duration, "now")) {
        attr(out, "tzone") <- "GMT"
        # out <<- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
        out <- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
      }
    }
    return(out)
  } else {
    kw1 <- arg_list$kw
    cat(crayon::cyan("Searching now for:\n"))
    s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time %>%
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))
    # what's the top of the 5 or less
    top_s1 <- find100(s1)
    cat(crayon::cyan("The", crayon::yellow$bold(top_s1$keyword), "query had the peak search volume...\n"))
    # out <<- s1
    out <- s1
    if (stringr::str_detect(arg_list$duration, "now")) {
      attr(out, "tzone") <- "GMT"
      # out <<- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
      out <- out %>% dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
    }
  }
  return(out)
}
