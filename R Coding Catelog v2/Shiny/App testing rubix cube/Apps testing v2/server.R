##-- Server, data loading, manipulation and obj building occurs
library(tidyverse) # tidydata
library(DT) # r shiny additional funs
library(shiny) # Rshiny base package
library(shinydashboard) # Rshiny dashboard funs
library(vcd) #mosaic plots
library(plotly) # interactive plots
library(lme4) #mixed models
library(splines) #natural splines

source(file = "/Users/angushewitt/Desktop/AFL App Dev/Weekly Data Refresh Workflows/Features building & Preds dts/Load GameDay Mixed Models.R") # load gameday models
load("/Users/angushewitt/Desktop/AFL App Dev/Apps/Data/baseline_Prediction_dt.Rdata") #baselimeprd dt - AFL stats last rec game
load("/Users/angushewitt/Desktop/AFL Datasets/Misc/meaningful_var_interactions_tab.RData") # rubik cube coefs


# Tab 3 -- Rubik cube (scenario models)
function(input, output, session) 
  
{
  
  
  ##-- meaingfull gD and car positions combinations tab
  ##-- need to include observe or reactive fun to tell server to grab value from ui
  meaningful_Car_pos_Vector <- reactive({ meaningful_var_interactions_tab %>%
    ungroup() -> meaningful_var_interactions_tab
  
  meaningful_var_interactions_tab %>%
    filter(GameDay_Role == input$Rubik_gd_Role_id) -> meaningful_var_interactions_tab_subset
  
  
    meaningful_var_interactions_tab_subset %>%
    select(Career_Position) -> meaningful_Car_pos_Vector
  
     })
  
  ##-- filter to player select by user
  ##-- mutate in selections mod for adjustable mod coefs
  output$rubik_Prob <-  renderTable ({ 
  ##-- need to include observe or reactive fun to tell server to grab value from ui


       ##--model coefs which the punter and manpulate
     baseline_Prediction_dt %>%
       ungroup() %>%
       filter(ID == input$Rubik_player_ID) %>%
       mutate(GD_Height_Categories = input$Rubik_gd_Height_id) %>%
       mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$Rubik_opp_Fantasy_ratio_id) %>%
       mutate(Cnt_Favs_Team_Level = as.numeric(input$Rubik_favs_Team_level_id)) %>%
       mutate(Cnt_Favs_Opp_Level = as.numeric(input$Rubik_favs_Opp_level_id)) %>%
       mutate(GD_TOG_Categories = as.numeric(input$Rubik_TOG_id)) %>%
       mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$Rubik_car_Pos_id %in% meaningful_Car_pos_Vector()$Career_Position, input$Rubik_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
       mutate(lagged_Venue_clusters = input$Rubik_clust_Venues_id) ->  baseline_Prediction_dt_ID
     
    ##-- feed in user selectoions into GD mods
      baseline_Prediction_dt_ID %>%
      mutate(HGS_Probability = case_when(input$Rubik_gd_Role_id == "Key_Forward"  ~ predict(Key_Forward_Mod, newdata = baseline_Prediction_dt_ID, type =  "response"),
                                         input$Rubik_gd_Role_id == "Forward_&_Frwd/PT_Ruck" ~ predict(Forward_Frwd_PT_Ruck_Mod, newdata = baseline_Prediction_dt_ID, type =  "response"))) %>%
      mutate(HGS_Probability = round(HGS_Probability,4)) %>%
      mutate(HGS_Probability_Odds = case_when(input$Rubik_gd_Role_id == "Key_Forward" ~  exp(predict(Key_Forward_Mod, newdata = baseline_Prediction_dt_ID )),
                                              input$Rubik_gd_Role_id == "Forward_&_Frwd/PT_Ruck" ~  exp(predict(Key_Forward_Mod, newdata = baseline_Prediction_dt_ID )))) %>%
      mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
      mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
      select(HGS_Probability, HGS_Odds) -> baseline_Prediction_dt_output
      
      })
      
      
}


##-------------------- Tab 3: Pred rubik cube ----------------##


