##-- monitor the performance of the afl mods ability to proj prob of success difference between ER and actuals
library(tidyverse)#tidydata
library(DT)#rshinyadditionalfuns
library(shiny)#rshinyfuns
library(shinydashboard)#Rshinydashboardfuns
library(shinyWidgets)#CSSandJavafuns
library(readxl) # read excel files
library(qcc) # conrol charts

## afl ER table
data <- read_excel("~/Desktop/Odds Machine/AFL App Dev/AFL perf charts & table/Data/AFL Weekly & Cum Returns.xlsx",
                   sheet = "Seasons Perf chart")


function(input, output, session) {
  

  output$control_Chart <- renderPlot({
  
  ## control chart
  qcc(data$Diff_ER_Acutals, sizes = 1, type = "xbar.one", center = 0) 
    
  })
  
  
  output$Cusum_Chart <- renderPlot({
    
  ##-- visual the cum performance of the afl mod
  cusum(data$Diff_ER_Acutals, sizes = 1, center = 0, decision.interval = 2)
  
  })
  
  ##-- output DT interactive search data table
  output$Table_Actuals <- renderTable({
    
    ##-- select player identifiers and rename data frame
    data 
  })
  
}