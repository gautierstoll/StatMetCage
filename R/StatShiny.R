#' shiny application
#' @export
StatShiny = function(){
  appDir <- system.file("ShinyApp", package = "StatMetCage")
  if (appDir == "") {
    stop("Could not find ShinyApp sub directory", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}