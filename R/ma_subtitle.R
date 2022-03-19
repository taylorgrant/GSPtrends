#' Determine length of Moving Average
#'
#' @description Calculate the duration of several moving averages that are applied to data returned from Google Trends API.
#'
#' Trends data uses different time windows depending on the duration searched. This function determines the length and determines the right unit of time.
#' @param tbl Data frame of Google Trends data from the API
#' @param agg The lengths of the moving averages applied to the data
#'
#' @return String detailing the length of the moving average
#' @export
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{
#' ma_subtitle(tbl, "ma7")
#' }
ma_subtitle <- function(tbl, agg) {
  tmp_dat <- tbl %>%
    dplyr::filter(ma == agg)

  # pull the numeric MA aggregation
  ma_numeric <- as.numeric(stringr::str_replace(agg, "ma", ""))

  # depending on the time difference, use different units and messages
  if (units(difftime(tmp_dat$date[2], tmp_dat$date[1])) == "mins") {
    ma_sub <- stringr::str_trim(stringr::str_replace(capture.output(difftime(tmp_dat$date[2], tmp_dat$date[1], units = "mins")*ma_numeric),
                                   "Time difference of", ""))
    ma_sub <- glue::glue(stringr::str_replace(stringr::str_sub(ma_sub, 1, nchar(ma_sub)-1), " ", "-"), " moving average")
  } else if (units(difftime(tmp_dat$date[2], tmp_dat$date[1])) == "hours") {
    ma_sub <- stringr::str_trim(stringr::str_replace(capture.output(difftime(tmp_dat$date[2], tmp_dat$date[1], units = "hours")*ma_numeric),
                                   "Time difference of", ""))
    ma_sub <- glue::glue(stringr::str_replace(stringr::str_sub(ma_sub, 1, nchar(ma_sub)-1), " ", "-"), " moving average")
  } else if (units(difftime(tmp_dat$date[2], tmp_dat$date[1])) == "days" &&
             as.numeric(tmp_dat$date[2] - tmp_dat$date[1]) < 7) {
    ma_sub <- stringr::str_trim(stringr::str_replace(capture.output(difftime(tmp_dat$date[2], tmp_dat$date[1], units = "days")*ma_numeric),
                                   "Time difference of", ""))
    ma_sub <- glue::glue(stringr::str_replace(stringr::str_sub(ma_sub, 1, nchar(ma_sub)-1), " ", "-"), " moving average")
  } else if (units(difftime(tmp_dat$date[2], tmp_dat$date[1])) == "days" &&
             as.numeric(tmp_dat$date[2] - tmp_dat$date[1]) >= 7 &&
             as.numeric(tmp_dat$date[2] - tmp_dat$date[1]) < 28 ) {
    ma_sub <- stringr::str_trim(stringr::str_replace(capture.output(difftime(tmp_dat$date[2], tmp_dat$date[1], units = "weeks")*ma_numeric),
                                   "Time difference of", ""))
    ma_sub <- glue::glue(stringr::str_replace(stringr::str_sub(ma_sub, 1, nchar(ma_sub)-1), " ", "-"), " moving average")
  } else {
    ma_sub <- glue::glue("{ma_numeric}-month moving average")
  }
  return(ma_sub)
}
