library(tidyverse) # tidydata
library(DT) # r shiny additional funs
library(shiny) # r shiny funs
library(shinydashboard) # Rshiny dashboard funs
library(data.table) # big data
library(plotly) # interactive plot
library(shinyWidgets) # CSS and Java funs

load("/Users/angushewitt/Desktop/AFL Datasets/Misc/rubik_Mod_coef.Rdata") # rubik cube coefs
load("/Users/angushewitt/Desktop/AFL App Dev/Apps/Data/baseline_Prediction_dt.Rdata") #baselimeprd dt - AFL stats last rec game


#View(broom.mixed::tidy(Ruckman_Mod))
##-- ui optionofrubik cube

fluidPage(
  tabsetPanel(tabPanel("Key Forward",
            
  ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
  selectInput("key_Frwd_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)
  selectInput("key_Frwd_car_Pos_id", "Key Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions),
  selectInput("key_Frwd_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
  selectInput("key_Frwd_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
  selectInput("key_Frwd_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
  selectInput("key_Frwd_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
  selectInput("key_Frwd_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
  selectInput("key_Frwd_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),

  ##-- output fpr each Gd role, tiles and stats  
  fluidRow(
    valueBoxOutput("rubik_Prob_key_Frwd"),
    valueBoxOutput("rubik_ODDS_key_Frwd"))
  ),
  tabPanel("Frwd or Frwd PT Ruck", 
           # Create a new Row in the UI for selectInputs
             
           ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
    selectInput("Frwd_PT_ruck_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)
    selectInput("Frwd_PT_ruck_car_Pos_id", "Key Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions),
    selectInput("Frwd_PT_ruck_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
    selectInput("Frwd_PT_ruck_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
    selectInput("Frwd_PT_ruck_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
    selectInput("Frwd_PT_ruck_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
    selectInput("Frwd_PT_ruck_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
    selectInput("Frwd_PT_ruck_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),
    
    fluidRow(
      valueBoxOutput("rubik_Prob_Frwd_PT_ruck"),
      valueBoxOutput("rubik_ODDS_Frwd_PT_ruck"))
  ),
           
  tabPanel("Half Forward", 
           # Create a new Row in the UI for selectInputs
           
           ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
           selectInput("Half_Forward_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)
           selectInput("Half_Forward_car_Pos_id", "Key Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions),
           selectInput("Half_Forward_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
           selectInput("Half_Forward_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
           selectInput("Half_Forward_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
           selectInput("Half_Forward_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
           selectInput("Half_Forward_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
           selectInput("Half_Forward_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),
           
           fluidRow(
             valueBoxOutput("rubik_Prob_Half_Forward"),
             valueBoxOutput("rubik_ODDS_Half_Forward"))
           
           
  ),
  
  tabPanel("Mid / Frwd Hybrid", 
           # Create a new Row in the UI for selectInputs
           
           ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
           selectInput("Mid_Frwd_Hybrid_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)
           selectInput("Mid_Frwd_Hybrid_car_Pos_id", "Key Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions),
           selectInput("Mid_Frwd_Hybrid_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
           selectInput("Mid_Frwd_Hybrid_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
           selectInput("Mid_Frwd_Hybrid_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
           selectInput("Mid_Frwd_Hybrid_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
           selectInput("Mid_Frwd_Hybrid_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
           selectInput("Mid_Frwd_Hybrid_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),
           
           fluidRow(
             valueBoxOutput("rubik_Prob_Mid_Frwd_Hybrid"),
             valueBoxOutput("rubik_ODDS_Mid_Frwd_Hybrid"))
           
           
  ),
  
  tabPanel("Ruckman", 
           # Create a new Row in the UI for selectInputs
           
           ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
           selectInput("Ruckman_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)
           selectInput("Ruckman_car_Pos_id", "Key Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions),
           selectInput("Ruckman_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
           selectInput("Ruckman_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
           selectInput("Ruckman_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
           selectInput("Ruckman_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
           selectInput("Ruckman_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
           selectInput("Ruckman_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),
           selectInput("Ruckman_cnt_In_game", "Number Ruckman in Team", rubik_Mod_coef[[5]]$Cnt_Ruckman_Team_Level),
           
           fluidRow(
             valueBoxOutput("rubik_Prob_Ruckman"),
             valueBoxOutput("rubik_ODDS_Ruckman"))
           
          
  )
  )
  )
  
  
##---------------- Rubik Cube --------------------##



##-- when adding new gad role into the rubix tab remember to include the drop downs and DT all in the Same tabpanel

  #verticalLayout(
  #     tableOutput("Player_model_Coefs"))
  #    )





