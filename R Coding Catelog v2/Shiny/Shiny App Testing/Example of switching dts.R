library(shiny)

ui <- fluidPage(
  selectInput("cropId", "Crop Type",
              c("Corn [bu/ac]" = "corn",
                "Potato [cwt/ac]" = "pota")),
  # Select the county  
  selectInput("County", label = "County", choices = c("County 1", "County 2", "County 3"),
              selected = NULL, multiple = FALSE),
  tableOutput("Crop_data")
)

server <- function(input, output, session) {
  
  NASS_Corn_data <- data.frame(Var1 = rnorm(5), 
                               Var2 = c("This", "is", "corn", "information", "!"))
  NASS_Potato_data <- data.frame(Var1 = rnorm(5), 
                                 Var2 = c("This", "is", "potato", "information", "!"))
  
  
  # This returns the correct dataset
  datasetInput <- reactive({
    if (input$cropId == "corn"){
      dataset <- NASS_Corn_data
    }
    else if (input$cropId == "pota"){
      dataset <- NASS_Potato_data
    }
    return(dataset)
  })
  
  #This shows you the correct dataset
  output$Crop_data <- renderTable ({
    datasetInput()
  })
  
  
  
}

shinyApp(ui = ui, server = server)
