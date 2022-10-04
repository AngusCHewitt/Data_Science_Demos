library(tidyverse)#tidydata
library(DT)#rshinyadditionalfuns
library(shiny)#rshinyfuns
library(shinydashboard)#Rshinydashboardfuns
library(shinyWidgets)#CSSandJavafuns

fluidPage(

  verticalLayout(
    plotOutput("control_Chart"),
    plotOutput("Cusum_Chart"),
    tableOutput("Table_Actuals")))