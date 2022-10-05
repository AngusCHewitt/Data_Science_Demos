library(tidyverse) # tidydata
library(DT) # r shiny additional funs
library(shiny) # r shiny funs
library(shinydashboard) # Rshiny dashboard funs
library(data.table) # big data
library(plotly) # interactive plot
library(shinyWidgets) # CSS and Java funs

load("/Users/angushewitt/Desktop/AFL Datasets/Misc/rubik_Mod_coef.Rdata") # rubik cube coefs
load("/Users/angushewitt/Desktop/AFL App Dev/Apps/Data/baseline_Prediction_dt.Rdata") #baselimeprd dt - AFL stats last rec game


rubik_Mod_coef
#View(broom.mixed::tidy(Ruckman_Mod))


##-- All rubik cube options for all GD models, keep within the same ui to save punter switching between tabs
##-- stack coefs in order of impoartance
fluidPage(
  tabPanel("Scenario Models",
            
  ##-- Key frwd Dropdown options fopr rubix cude model (coefs)
  selectInput("Rubik_player_ID", "Player ID", baseline_Prediction_dt$ID), # repalce with dt with just id from baseline dataset (add stepp 11)

  selectInput("Rubik_gd_Role_id", "GameDay Role", rubik_Mod_coef[[8]]$GameDay_Role, "Key_Forward"),
  selectInput("Rubik_car_Pos_id", "Meaningful Career Positions", rubik_Mod_coef[[9]]$Career_Positions, "Key_Forward"),
  selectInput("Rubik_gd_Height_id", "GameDay Height Categories", rubik_Mod_coef[[7]]$GD_Height_Categories),
  
  selectInput("Rubik_favs_Team_level_id", "Count of Favorites Team Playing For", rubik_Mod_coef[[2]]$Cnt_Favs_Team_Level),
  selectInput("Rubik_favs_Opp_level_id", "Count of Favorites Opposition", rubik_Mod_coef[[3]]$Cnt_Favs_Opp_Level),
  
  selectInput("Rubik_opp_Fantasy_ratio_id", "Opposition Fantasy Ratio", rubik_Mod_coef[[1]]$team_Cum_fantasy_Ratio_per_season_Categories_Opp),
  selectInput("Rubik_TOG_id", "Time on Ground Categories", rubik_Mod_coef[[4]]$GD_TOG_Categories),
  
  selectInput("Rubik_clust_Venues_id", "Next Match Venue Clusters", rubik_Mod_coef[[6]]$lagged_Venue_clusters),
  selectInput("Rubik_cnt_Ruckman_Team_id", "Count Ruckman in Team", rubik_Mod_coef[[5]]$Cnt_Ruckman_Team_Level),
  
  
  ##-- output fpr each Gd role, tiles and stats  
  #  actionButton("slot_machine", "Press Button"),
  tableOutput("rubik_Prob")
  )
  )
  
  
  
##---------------- Rubik Cube --------------------##



##-- when adding new gad role into the rubix tab remember to include the drop downs and DT all in the Same tabpanel

  #verticalLayout(
  #     tableOutput("Player_model_Coefs"))
  #    )





