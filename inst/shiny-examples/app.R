library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("ResStatMetabo"),
  sidebarLayout(
    sidebarPanel("Raw Data",
                 fileInput("RawFile", "Choose CSV File", accept = ".csv"),
                 ),
    mainPanel("Analysis",textOutput("StatOutput"))
   ## mainPanel("Observations",uiOutput("Observations")
              ##checkboxGroupInput(
      ##"choiceObservation",
      ##label = "Select observations",
      ##choices = names(tableOutput("Observations")),
      ##selected = NULL,
      ##inline = FALSE,
      ##width = NULL,
      ##choiceNames = NULL,
      ##choiceValues = NULL
    #)
    )
  )


# Define server logic 
server <- function(input, output) {
  ##output$Observations <- renderUI({
  ##    file <- input$RawFile
  ##    ext <- tools::file_ext(file$datapath)
  ##    req(file)
  ##    validate(need(ext == "csv", "Please upload a csv file"))
  ##    RawData = new("RawMetaboData",fileName = file$datapath)
  ##    checkboxGroupInput("obsChoice",label = "Select observations",choices = names(RawData@data))
  ##  })
  output$StatOutput =  renderText({
    file <- input$RawFile
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    RawData = new("RawMetaboData",fileName = file$datapath)
    
  })
  
}

shinyApp(ui = ui, server = server)