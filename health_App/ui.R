library(shiny)
library(shinydashboard)#Rshinydashboardfuns
library(shinyWidgets)#CSSandJavafuns
library(plotly) # interactive plots 
library(DT) # dynamic tbs 

load("data/app_Sample_dt.rds") # sample dt 


fluidPage(
    
    
    ##-- adjust background colors - light green fade 
    shiny::titlePanel("Predictive & Descripitive Outputs"),
    
    tabsetPanel( 
      tabPanel("patient Lookup", ## patient dynmaic lookup tb 
      radioButtons("Select_Tb_vars", "Select Patient Info", c("Demographics", "Health Indictors") , "Demographics", inline = TRUE), # "" ump
               
    
     DTOutput("patient_Lookup_tb") # patient tb 
      ),         

      tabPanel("preds", # batting team bb velo mosaic 
      selectInput("select_mod_ID", "patient ID", app_Sample_dt$ID , app_Sample_dt$ID[1]), # select patient ID 
      
      tableOutput("pred_Table")
      )         
      )
      ) # end app 

