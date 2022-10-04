# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(tidyverse)
library(shiny)
library(data.table)

load("/Users/angushewitt/Desktop/AFL test case/AFL Apps/Data/Mixed Mods/Key_Forward_dt.Rdata")
load("/Users/angushewitt/Desktop/AFL test case/AFL Apps/Data/Mixed Mods/Forward_Frwd_PT_Ruck_dt.Rdata")


ui <- fluidPage(
  tabsetPanel(tabPanel("tab1", 
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    
    column(4,
           selectInput("player_ID",
                       "player_ID:",
                       c("All",
                         unique(as.character(Key_Forward_dt$ID))))),
    column(4,
           selectInput("Career_Pos",
                       "Career_Positions_Meaningful_GD_Roles:",
                       c("All",
                         unique(as.character(Key_Forward_dt$Career_Positions_Meaningful_GD_Roles))))),
    column(4,
           selectInput("GameDay_Height",
                       "GD_Height_Categories:",
                       c("All",
                         unique(as.character(Key_Forward_dt$GD_Height_Categories))))),
    
    column(4,
           selectInput("Cnt_HGS_Favs_Team",
                       "Cnt_HGS_Favs_Team:",
                       c("All",
                         unique(as.character(Key_Forward_dt$Cnt_Favs_Team_Level))))),
    
    column(4,
           selectInput("Cnt_HGS_Favs_Opposition",
                       "Cnt_HGS_Favs_Opposition:",
                       c("All",
                         unique(as.character(Key_Forward_dt$Cnt_Favs_Opp_Level))))),
    
    column(4,
           selectInput("Key_Def_Opposition",
                       "Key_Def_Opposition:",
                       c("All",
                         unique(as.character(Key_Forward_dt$Cnt_Elite_Great_Key_Defs_Opp_Level)))))
    
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
  ),
  
  tabPanel("tab2", 
           # Create a new Row in the UI for selectInputs
           fluidRow(
             
             column(4,
                    selectInput("player_ID",
                                "player_ID:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$ID))))),
             column(4,
                    selectInput("Career_Pos",
                                "Career_Positions_Meaningful_GD_Roles:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$Career_Positions_Meaningful_GD_Roles))))),
             column(4,
                    selectInput("GameDay_Height",
                                "GD_Height_Categories:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$GD_Height_Categories))))),
             
             column(4,
                    selectInput("Cnt_HGS_Favs_Team",
                                "Cnt_HGS_Favs_Team:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$Cnt_Favs_Team_Level))))),
             
             column(4,
                    selectInput("Cnt_HGS_Favs_Opposition",
                                "Cnt_HGS_Favs_Opposition:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$Cnt_Favs_Opp_Level))))),
             
             column(4,
                    selectInput("Key_Def_Opposition",
                                "Key_Def_Opposition:",
                                c("All",
                                  unique(as.character(Forward_Frwd_PT_Ruck_dt$Cnt_Elite_Great_Key_Defs_Opp_Level)))))
             
           ),
           # Create a new row for the table.
           DT::dataTableOutput("table2")
           )         
           )
           )



server <- function(input, output, session) {
  
  


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(
    
    options = list(searching = FALSE, pageLength = 10, autoWidth = TRUE),
    {
    
    if (input$player_ID != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$ID == input$player_ID,]
    }
    
    if (input$Career_Pos != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$Career_Positions_Meaningful_GD_Roles == input$Career_Pos,]
    }
    
    if (input$GameDay_Height != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$GD_Height_Categories == input$GameDay_Height,]
    }
    
    if (input$Cnt_HGS_Favs_Team != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$Cnt_Favs_Team_Level == input$Cnt_HGS_Favs_Team,]
    }
    
    if (input$Cnt_HGS_Favs_Opposition != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$Cnt_Favs_Opp_Level == input$Cnt_HGS_Favs_Opposition,]
    }
    
    if (input$Key_Def_Opposition != "All") {
      Key_Forward_dt <- Key_Forward_dt[Key_Forward_dt$Cnt_Elite_Great_Key_Defs_Opp_Level == input$Key_Def_Opposition,]
    }
    
    Key_Forward_dt %>%
      select(ID, HGS_Probability, HGS_Odds) %>%
      rename("HGS_Probability (%)" = HGS_Probability,
             "HGS_Odds (n / 1)" = HGS_Odds) -> Key_Forward_dt
  }))
  
  
  # Filter data based on selections
  output$table2 <- DT::renderDataTable(DT::datatable(
    
    options = list(searching = FALSE, pageLength = 10, autoWidth = TRUE),
    {
      
      if (input$player_ID != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$ID == input$player_ID,]
      }
      
      if (input$Career_Pos != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$Career_Positions_Meaningful_GD_Roles == input$Career_Pos,]
      }
      
      if (input$GameDay_Height != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$GD_Height_Categories == input$GameDay_Height,]
      }
      
      if (input$Cnt_HGS_Favs_Team != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$Cnt_Favs_Team_Level == input$Cnt_HGS_Favs_Team,]
      }
      
      if (input$Cnt_HGS_Favs_Opposition != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$Cnt_Favs_Opp_Level == input$Cnt_HGS_Favs_Opposition,]
      }
      
      if (input$Key_Def_Opposition != "All") {
        Forward_Frwd_PT_Ruck_dt <- Forward_Frwd_PT_Ruck_dt[Forward_Frwd_PT_Ruck_dt$Cnt_Elite_Great_Key_Defs_Opp_Level == input$Key_Def_Opposition,]
      }
      
      Forward_Frwd_PT_Ruck_dt %>%
        select(ID, HGS_Probability, HGS_Odds) %>%
        rename("HGS_Probability (%)" = HGS_Probability,
               "HGS_Odds (n / 1)" = HGS_Odds) -> Forward_Frwd_PT_Ruck_dt
    }))
  
}



shinyApp(ui, server)


