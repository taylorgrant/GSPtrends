#' Run Google Trends app
#'
#' @return Can choose to export data and plots through Shiny
#' @export
#'
#' @examples
#' \dontrun{
#' runApp()
#' }
runApp <- function(){
  appDir <- system.file("shiny-app", package = "GSPtrends")
  if (appDir == "") {
    stop("Could not find directory. Try reinstalling `GSPtrends`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
