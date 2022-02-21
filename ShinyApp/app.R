library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("ResStatMetabo"),
  sidebarLayout(
    sidebarPanel("Raw Data",
                 fileInput("RawFile", "Choose CSV File", accept = ".csv"),
                 checkboxGroupInput(
                   "ObservationList",
                   label = tableOutput("Observations"),
                   choices = NULL,
                   selected = NULL,
                   inline = FALSE,
                   width = NULL,
                   choiceNames = NULL,
                   choiceValues = NULL
                 )),
    mainPanel("Observations",tableOutput("Observations"))
  )
)

# Define server logic 
server <- function(input, output) {
  output$Observations <- renderTable({
      file <- input$RawFile
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      RawMetaData = new("RawMetaboData",fileName = file$datapath)
      names(RawMetaData@data)
    })
  
}

shinyApp(ui = ui, server = server)