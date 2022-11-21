#' Query Google Trends API
#'
#' @description
#' Convenience wrapper for the \code{\link[gtrendsR]{gtrends}} function.
#'
#' If there are more than 5 terms to query, function will ensure that the most popular term is in each query.
#'
#' @param arg_list list of arguments to use in API call; arguments provided by user through the Shiny app and wrapped into a list
#'
#' @return Data frame returned from Google Trends API
#' @export
#'
#' @examples
#' \dontrun{
#' pull_gtr(arg_list)}
pull_gtr <- function(arg_list) {
  # # check to make sure that no more than 13 search terms
  # stopifnot(length(arg_list$kw) <= 13)
  # # split out search terms based on length (5 max) hold out 1, split on 4
  if (length(arg_list$kw) > 5) {
    keywords <- split(arg_list$kw[2:length(arg_list$kw)], ceiling(seq_along(arg_list$kw[2:length(arg_list$kw)])/4))
  } else {
    keywords <- "single"
  }


  if (length(keywords) == 3) {

    cat("Running the first query...\n")
    kw1 <- c(arg_list$kw[1], keywords[[1]])
    s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))

    # find top performer
    top_s1 <- s1[s1$hits == 100,]

    # second run
    cat("Running the second query...\n")
    kw2 <- c(unique(top_s1$keyword), keywords[[2]])
    s2 <- gtrendsR::gtrends(keyword = dput(kw2),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))

    # find top performer
    top_s2 <- s2[s2$hits == 100,]

    # if top s1 & top s2 DO NOT match:
    if (unique(top_s1$keyword) != unique(top_s2$keyword)) {
      cat("\nThe", unique(top_s1$keyword), "query had the peak search volume in the 1st search...\n")
      cat("The", unique(top_s2$keyword), "query had the peak search volume in the 2nd search.\n")
      cat("Re-running with", unique(top_s2$keyword), "in both...\n")
      # re-run first search with new top performer
      kw1 <- c(unique(top_s2$keyword), kw1[-which(kw1 == unique(top_s1$keyword))])
      s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time |>
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))
      # find top performer
      top_s1 <- s1[s1$hits == 100,]
    }
    # run third search
    cat("Running the third query...\n")
    kw3 <- c(unique(top_s1$keyword), keywords[[3]])
    s3 <- gtrendsR::gtrends(keyword = dput(kw3),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))
    # find top performer
    top_s3 <- s3[s3$hits == 100,]

    # if s1 and s3 DO match:
    if (unique(top_s1$keyword) == unique(top_s3$keyword)) {
      cat("\nThe", unique(top_s1$keyword), "query had the peak search volume in all searches...\n")
      # drop redundant keyword from other search
      s2 <- s2[s2$keyword != top_s1$keyword,]
      s3 <- s3[s3$keyword != top_s1$keyword,]
      # bind together
      out <- dplyr::bind_rows(s1, s2, s3)
    } else {
      cat("The", unique(top_s3$keyword), "query had the peak search volume in the 3nd search.\n")
      cat("Re-running with", unique(top_s3$keyword), "in both...\n")
      # re-run first search with new top performer
      kw1 <- c(unique(top_s3$keyword), kw1[-which(kw1 == unique(top_s1$keyword))])
      s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time |>
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))
      # re-run second search with new top performer
      kw2 <- c(unique(top_s3$keyword), kw2[-which(kw2 == unique(top_s2$keyword))])
      s2 <- gtrendsR::gtrends(keyword = dput(kw2),
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time |>
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))
        # drop redundant keyword from other search
        s1 <- s1[s1$keyword != unique(top_s3$keyword),]
        s2 <- s2[s2$keyword != unique(top_s3$keyword),]
        out <- dplyr::bind_rows(s1, s2, s3)
        }
  } else if (length(keywords) == 2) {
    cat("Running the first query...\n")
    kw1 <- c(arg_list$kw[1], keywords[[1]])
    s1 <- gtrendsR::gtrends(keyword = dput(kw1),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))
    # find top performer
    top_s1 <- s1[s1$hits == 100,]

    # second run
    cat("Running the second query...\n")
    kw2 <- c(unique(top_s1$keyword), keywords[[2]])
    s2 <- gtrendsR::gtrends(keyword = dput(kw2),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))

    # find top performer
    top_s2 <- s2[s2$hits == 100,]

    # if top s1 & top s2 DO match:
    if (unique(top_s1$keyword) == unique(top_s2$keyword)) {
      cat("\nDone...", unique(top_s2$keyword), "had the peak search volume...\n")
      s1 <- s1[s1$keyword != unique(top_s2$keyword),]
      out <- dplyr::bind_rows(s1, s2)
    } else {
      cat("\nThe", unique(top_s1$keyword), "query had the peak search volume in the 1st search...\n")
      cat("The", unique(top_s2$keyword), "query had the peak search volume in the 2nd search.\n")
      cat("Re-running with", unique(top_s2$keyword), "in both...\n")
      # re-run first search with new top performer
      kw1 <- c(unique(top_s2$keyword), kw1[-which(kw1 == unique(top_s1$keyword))])
      s1 <- gtrendsR::gtrends(keyword = kw1,
                              geo = "US",
                              time = arg_list$duration,
                              gprop = arg_list$gprop,
                              category = arg_list$category)$interest_over_time |>
        dplyr::mutate(hits = as.numeric(hits),
                      hits = ifelse(is.na(hits), 0, hits))
      # find top performer
      top_s1 <- s1[s1$hits == 100,]
    }
    s1 <- s1[s1$keyword != unique(top_s2$keyword),]
    out <- dplyr::bind_rows(s1, s2)
  } else {
    cat("Running query...\n")
    print(Sys.time())
    out <- gtrendsR::gtrends(keyword = dput(arg_list$kw),
                            geo = "US",
                            time = arg_list$duration,
                            gprop = arg_list$gprop,
                            category = arg_list$category)$interest_over_time |>
      dplyr::mutate(hits = as.numeric(hits),
                    hits = ifelse(is.na(hits), 0, hits))
  }
  if (stringr::str_detect(arg_list$duration, "now")) {
    attr(out, "tzone") <- "GMT"
    out <- out |>  dplyr::mutate(date = lubridate::with_tz(date, "America/Los_Angeles"))
  }
  out

}



