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
    shinyFilesButton("annotFile", "Annotation File", "Select an annotation file",multiple = F, viewtype = "detail"),
    tags$hr(),
    actionButton("RunAnalysis","Run Analysis")),
    column(4,"Observations",
              tableOutput("Observations")),
    column(4,"Annotations",tableOutput("Annotations"))
    
    )
  )


# Define server logic 
server <- function(input, output,session) {
  volumes = c(home = fs::path_home(),wd = fs::path_wd(),getVolumes()())
  shinyFileChoose(input, "rawFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("csv"))
  shinyFileChoose(input, "annotFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("xlsx"))
  MetaboContainer = reactiveValues()
  output$Observations =  renderTable({
    if (is.integer(input$rawFile)) {c(" ")} else {
    print((parseFilePaths(volumes, input$rawFile))$datapath)
    MetaboContainer$RawData = new("RawMetaboData",fileName = parseFilePaths(volumes, input$rawFile)$datapath)
  testCol = which(is.element(c("Date","Time","Animal No."),names(MetaboContainer$RawData@data)))
  if (length(testCol) < 3) {MetaboContainer$RawData = NULL;c("Missing elements",c("Date","Time","Animal No.")[-testCol])} else
    {names(MetaboContainer$RawData@data)[!is.element(names(MetaboContainer$RawData@data),c("Date","Time","Animal No."))]}}
  },colnames=F)
  output$Annotations = renderTable({
    if (is.integer(input$annotFile)) {c(" ")} else {
      print((parseFilePaths(volumes, input$annotFile))$datapath)
      MetaboContainer$Annot = readxl::read_excel(path = parseFilePaths(volumes, input$annotFile)$datapath)
      testCol = which(is.element(c("Date","Time","Animal","Group"),names(MetaboContainer$Annot)))
      if (length(testCol) < 4) {MetaboContainer$Annot = NULL; c("Missing elements",c("Date","Time","Animal","Group")[-testCol])} else
      names(MetaboContainer$Annot)[!is.element(names(MetaboContainer$Annot),c("Date","Time","Animal","Group"))]}
  },colnames=F)
  observeEvent(input$RunAnalysis,{
    if (!is.null(MetaboContainer$Annot) & !is.null(MetaboContainer$RawData)) {
      
    }
  })
}

shinyApp(ui = ui, server = server)