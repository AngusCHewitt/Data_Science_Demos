library(shiny)
library(tidyverse)
library(data.table)

load("/Users/angushewitt/Desktop/AFL test case/AFL Apps/Data/Mixed Mods/Key_Forward_dt.Rdata")
load("/Users/angushewitt/Desktop/AFL test case/AFL Apps/Data/Mixed Mods/Forward_Frwd_PT_Ruck_dt.Rdata")


##-- user interface
ui <- fluidPage(
  ##-- tab title
  titlePanel("Dynamically generated user interface components"),
  
  # Depending on input$input_type, we'll generate a different
  selectInput("GameDay_Position", "Player GameDay Role next Match", choices = c( "Key_Forward",  "Forward_Frwd_PT_Ruck")),

  # Create a new DT table
  DT::dataTableOutput("table")
)

##-- server
server <- function(input, output, session) {
  
  
  # This returns the correct dataset
  datasetInput <- reactive({
    if (input$GameDay_Position == "Key_Forward"){
      
      dataset <- Key_Forward_dt
      
    return(dataset)
      } 
      
    else if (input$GameDay_Position == "Forward_Frwd_PT_Ruck"){
      dataset <- Forward_Frwd_PT_Ruck_dt
    }
    return(dataset)
  })
  
  # output dynamic tab dep on dataset input selection
   output$table <- DT::renderDataTable({DT::datatable(
     
     
     datasetInput() , selection = "single",
     filter = 'top', options = list(searching = FALSE, 
       pageLength = 10, autoWidth = TRUE)
     )
     }
     )
     }
  
  

shinyApp(ui, server)


#rm(list = ls())  # caution: delete all objects in .GlobalEnv
#gc()  # free system memory
