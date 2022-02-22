library(shiny)
library(shinyFiles)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("ResStatMetabo"),
  fluidRow(
    column(4,
    tags$p("Select raw data file."),
    shinyFilesButton("rawFile", "Raw Data File", "Select a raw data file",multiple = F, viewtype = "detail"),
    tags$hr(),
    tags$p("Select annotation file"),
    shinyFilesButton("annotFile", "Annotation File", "Select an annotation file",multiple = F, viewtype = "detail")),
    column(4,"Observations",
              tableOutput("Observations")),
    column(4,"Annotations",tableOutput("Annotations"))
    
    )
  )


# Define server logic 
server <- function(input, output,session) {
  volumes = c(home = fs::path_home(),wd = fs::path_wd(),getVolumes()())
  shinyFileChoose(input, "rawFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("csv"))
  shinyFileChoose(input, "annotFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("csv"))
  output$Observations =  renderTable({
    if (is.integer(input$rawFile)) {c(" ")} else {
    print((parseFilePaths(volumes, input$rawFile))$datapath)
    RawData = new("RawMetaboData",fileName = parseFilePaths(volumes, input$rawFile)$datapath)
    names(RawData@data)}
  },colnames=F)
  output$Annotations = renderTable({
    if (is.integer(input$annotFile)) {c(" ")} else {
      print((parseFilePaths(volumes, input$annotFile))$datapath)
      AnnotData = read.csv(file = parseFilePaths(volumes, input$annotFile)$datapath)
      names(AnnotData)}
  },colnames=F)
  
}

shinyApp(ui = ui, server = server)