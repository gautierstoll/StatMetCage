#' shiny application
#' @export
StatShiny = function(){
  appDir <- system.file("shiny-examples", package = "StatMetCage")
  if (appDir == "") {
    stop("Could not find shiny-exmaples sub directory", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}