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
  noObservations = c("Date","Time","Animal No.","Box","Ref.SFlow","Ref.O2","Ref.CO2","Flow","S.Flow",
                     "Temp","O2","CO2","dO2","dCO2","VO2(1)","VO2(2)","VCO2(1)","VCO2(2)","H(1)","H(2)")
  print(paste("no",noObservations))
  volumes = c(home = fs::path_home(),wd = fs::path_wd(),getVolumes()())
  shinyFileChoose(input, "rawFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("csv"))
  shinyFileChoose(input, "annotFile", defaultRoot = "home",roots = volumes, session = session,filetypes = c("xlsx"))
  output$Observations =  renderTable({
    if (is.integer(input$rawFile)) {c(" ")} else {
      MetaboContainer$RawData = new("RawMetaboData",fileName = parseFilePaths(volumes, input$rawFile)$datapath)
      MetaboContainer$OutDir=unlist(gsub(parseFilePaths(volumes, input$rawFile)$name,"",parseFilePaths(volumes, input$rawFile)$datapath,fixed=T))
      print(MetaboContainer$OutDir)
      testCol = which(is.element(c("Date","Time","Animal No."),names(MetaboContainer$RawData@data)))
      if (length(testCol) < 3) {MetaboContainer$RawData = NULL;c("Missing elements",c("Date","Time","Animal No.")[-testCol])} else
      {names(MetaboContainer$RawData@data)[!is.element(names(MetaboContainer$RawData@data),c("Date","Time","Animal No."))]}}
  },colnames=F)
  output$Annotations = renderTable({
    if (is.integer(input$annotFile)) {c(" ")} else {
      MetaboContainer$Annot = readxl::read_excel(path = parseFilePaths(volumes, input$annotFile)$datapath)
      testCol = which(is.element(c("Date","Time","Animal","Group"),names(MetaboContainer$Annot)))
      if (length(testCol) < 4) {MetaboContainer$Annot = NULL; c("Missing elements",c("Date","Time","Animal","Group")[-testCol])} else
        names(MetaboContainer$Annot)[!is.element(names(MetaboContainer$Annot),c("Date","Time","Animal","Group"))]}
  },colnames=F)
  observeEvent(input$RunAnalysis,{
    if (!is.null(MetaboContainer$Annot) & !is.null(MetaboContainer$RawData)) {
      MetaboContainer$Analysis = 
        new("AnalysisMetaboData",
            rawData = MetaboContainer$RawData,
            date = "Date",
            time = "Time",
            animal = "Animal No.",
            obs = setdiff(names(MetaboContainer$RawData@data),noObservations),
            annotation =MetaboContainer$Annot,
            annotGroups = names(which(!sapply(MetaboContainer$Annot[!is.element(names(MetaboContainer$Annot),c("Animal","Date","Time"))],
                                              function(x){is.numeric(x)}))))
      print("Analysis created")
      print(names(MetaboContainer$RawData@data))
      print(noObservations)
      for (obs in names(setdiff(names(MetaboContainer$RawData@data),noObservations))) {
        print(paste("start analysis of ",obs,sep = ""))
        ResStat = new("ResStatMetabo",anMetData = MetaboContainer$Analysis,observation = obs,model = "quadratic",group = "Group")
        pdf(file = paste(MetaboContainer$OutDir,"obs.pdf",sep = ""))
        metaboPlot(ResStat,type="data.model")
            dev.off()
            print("pdf created")
      }
    }
  })
}

shinyApp(ui = ui, server = server)