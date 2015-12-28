#' Run interactive FreqProf example (Shiny App)
#'
#' @export
#' @examples
#' runExample()
runExample <- function() {
  appDir <- system.file("inst", "shinyapp", package = "FreqProf")
  if (appDir == "") {
    stop("Could not find example directory. Try reinstalling `FreqProf`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}