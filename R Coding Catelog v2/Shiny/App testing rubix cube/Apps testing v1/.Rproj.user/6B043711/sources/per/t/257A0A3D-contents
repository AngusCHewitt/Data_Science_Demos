##-- Server, data loading, manipulation and obj building occurs
library(tidyverse) # tidydata
library(DT) # r shiny additional funs
library(shiny) # Rshiny base package
library(shinydashboard) # Rshiny dashboard funs
library(vcd) #mosaic plots
library(plotly) # interactive plots
library(lme4) #mixed models
library(splines) #natural splines

#load("Data/GD_roles_since_2016_Car_Positions_lagged.Rdata")##-- load Gameday and Career position actual probs cnt tab
#load("/Users/angushewitt/Desktop/AFL Datasets/Misc/rubik_Mod_coef.Rdata") # rubik cube coefs
source(file = "/Users/angushewitt/Desktop/AFL App Dev/Weekly Data Refresh Workflows/Features building & Preds dts/Load GameDay Mixed Models.R") # load gameday models
load("/Users/angushewitt/Desktop/AFL App Dev/Apps/Data/baseline_Prediction_dt.Rdata") #baselimeprd dt - AFL stats last rec game
load("/Users/angushewitt/Desktop/AFL Datasets/Misc/meaningful_var_interactions_tab.RData") # rubik cube coefs



#help(package = "DT")
#help(package = "shiny")

##-- each player last recorded game
#load("Data/baseline_Prediction_dt.Rdata")
#glimpse(baseline_Prediction_dt)


# Tab 3 -- Rubik cube (scenario models)
function(input, output, session) 
{

  ##-- need to include observe or reactive fun to tell server to grab value from ui
  observe({
    
    meaningful_var_interactions_tab %>%
      filter(GameDay_Role == "Key_Forward") -> meaningful_var_interactions_tab_key_frwd
    
    ##-- filter to player select by user
    ##-- mutate in selections mod for adjustable mod coefs
    baseline_Prediction_dt %>%
      filter(ID == input$key_Frwd_player_ID) %>%
      mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$key_Frwd_car_Pos_id %in% c(meaningful_var_interactions_tab_key_frwd$Career_Position), # limited career posiition to mean car pos levels for mod> 
                                                                                             input$key_Frwd_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
      mutate(GD_Height_Categories = input$key_Frwd_gd_Height_id) %>%
      mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$key_Frwd_opp_Fantasy_ratio_id) %>%
      mutate(Cnt_Favs_Team_Level = as.numeric(input$key_Frwd_favs_Team_level_id)) %>%
      mutate(Cnt_Favs_Opp_Level = as.numeric(input$key_Frwd_favs_Opp_level_id)) %>%
      mutate(GD_TOG_Categories = as.numeric(input$key_Frwd_TOG_id)) %>%
      mutate(lagged_Venue_clusters = input$key_Frwd_clust_Venues_id) ->  Key_Forward_dt  
    
    Key_Forward_dt %>%
      ungroup() %>%
      mutate(HGS_Probability = if_else(two_Or_less_Games_id == "more_2_games", predict(Key_Forward_Mod, newdata = Key_Forward_dt, type =  "response"),
                                       predict(Key_Forward_Mod_2_less_Games, newdata = Key_Forward_dt, type =  "response"))) %>%
      mutate(HGS_Probability = round(HGS_Probability,4)) %>%
      mutate(HGS_Probability_Odds = if_else(two_Or_less_Games_id == "more_2_games", exp(predict(Key_Forward_Mod, newdata = Key_Forward_dt)),
                                            exp(predict(Key_Forward_Mod_2_less_Games, newdata = Key_Forward_dt)))) %>%
      mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
      mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
      select(ID, HGS_Probability, HGS_Odds) -> Key_Forward_dt
    
    
##-- key frwd rubix cube value box, displaying prob and odds using manpulated moef coefs feeed into GD mod  
  output$rubik_Prob_key_Frwd<- renderValueBox({
    valueBox(str_c(Key_Forward_dt$HGS_Probability * 100, " ", "%"),
             "Probability of being Highest Goal Scorer"
    )
  })
  
  output$rubik_ODDS_key_Frwd<- renderValueBox({
    valueBox(str_c(Key_Forward_dt$HGS_Odds, " to 1"),
      "Odds (Failure / Success)"
      )
  })
  })
  
  observe({
  ##-- Forward_&_Frwd/PT_Ruck --## 
  meaningful_var_interactions_tab %>%
    filter(GameDay_Role == "Forward_&_Frwd/PT_Ruck") -> meaningful_var_interactions_tab_Frwd_PT_ruck
  
  ##-- filter to player select by user
  ##-- mutate in selections mod for adjustable mod coefs
  baseline_Prediction_dt %>%
    filter(ID == input$Frwd_PT_ruck_player_ID) %>%
    mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$Frwd_PT_ruck_car_Pos_id %in% c(meaningful_var_interactions_tab_Frwd_PT_ruck$Career_Position), # limited career posiition to mean car pos levels for mod> 
                                                          input$Frwd_PT_ruck_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
    mutate(GD_Height_Categories = input$Frwd_PT_ruck_gd_Height_id) %>%
    mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$Frwd_PT_ruck_opp_Fantasy_ratio_id) %>%
    mutate(Cnt_Favs_Team_Level = as.numeric(input$Frwd_PT_ruck_favs_Team_level_id)) %>%
    mutate(Cnt_Favs_Opp_Level = as.numeric(input$Frwd_PT_ruck_favs_Opp_level_id)) %>%
    mutate(GD_TOG_Categories = as.numeric(input$Frwd_PT_ruck_TOG_id)) %>%
    mutate(lagged_Venue_clusters = input$Frwd_PT_ruck_clust_Venues_id) ->  Frwd_PT_ruck_dt  
  
  Frwd_PT_ruck_dt %>%
    ungroup() %>%
    mutate(HGS_Probability = if_else(two_Or_less_Games_id == "more_2_games", predict(Forward_Frwd_PT_Ruck_Mod, newdata = Frwd_PT_ruck_dt, type =  "response"),
                                     predict(Forward_Frwd_PT_Ruck_Mod_2_less_Games, newdata = Frwd_PT_ruck_dt, type =  "response"))) %>%
    mutate(HGS_Probability = round(HGS_Probability,4)) %>%
    mutate(HGS_Probability_Odds = if_else(two_Or_less_Games_id == "more_2_games", exp(predict(Forward_Frwd_PT_Ruck_Mod, newdata = Frwd_PT_ruck_dt)),
                                          exp(predict(Forward_Frwd_PT_Ruck_Mod_2_less_Games, newdata = Frwd_PT_ruck_dt)))) %>%
    mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
    mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
    select(ID, HGS_Probability, HGS_Odds) -> Frwd_PT_ruck_dt
  
  
  ##-- key frwd rubix cube value box, displaying prob and odds using manpulated moef coefs feeed into GD mod  
  output$rubik_Prob_Frwd_PT_ruck<- renderValueBox({
    valueBox(str_c(Frwd_PT_ruck_dt$HGS_Probability * 100, " ", "%"),
             "Probability of being Highest Goal Scorer"
    )
  })
  
  output$rubik_ODDS_Frwd_PT_ruck<- renderValueBox({
    valueBox(str_c(Frwd_PT_ruck_dt$HGS_Odds, " to 1"),
             "Odds (Failure / Success)"
    )
  })
  })
  
  observe({
    ##-- Half_Forward--## 
    meaningful_var_interactions_tab %>%
      filter(GameDay_Role == "Half_Forward") -> meaningful_var_interactions_tab_Half_Forward
    
    ##-- filter to player select by user
    ##-- mutate in selections mod for adjustable mod coefs
    baseline_Prediction_dt %>%
      filter(ID == input$Half_Forward_player_ID) %>%
      mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$Half_Forward_car_Pos_id %in% c(meaningful_var_interactions_tab_Half_Forward$Career_Position), # limited career posiition to mean car pos levels for mod> 
                                                            input$Half_Forward_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
      mutate(GD_Height_Categories = input$Half_Forward_gd_Height_id) %>%
      mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$Half_Forward_opp_Fantasy_ratio_id) %>%
      mutate(Cnt_Favs_Team_Level = as.numeric(input$Half_Forward_favs_Team_level_id)) %>%
      mutate(Cnt_Favs_Opp_Level = as.numeric(input$Half_Forward_favs_Opp_level_id)) %>%
      mutate(GD_TOG_Categories = as.numeric(input$Half_Forward_TOG_id)) %>%
      mutate(lagged_Venue_clusters = input$Half_Forward_clust_Venues_id) ->  Half_Forward_dt  
    
    
    ##-- GD mod feed in to produce prob / ODD of HGS, split by whether player had 2 or more gamesin  a seaosn
    Half_Forward_dt %>%
      ungroup() %>%
      mutate(HGS_Probability = if_else(two_Or_less_Games_id == "more_2_games", predict(Half_Forward_Mod, newdata = Half_Forward_dt, type =  "response"),
                                       predict(Half_Forward_Mod_2_less_Games, newdata = Half_Forward_dt, type =  "response"))) %>%
      mutate(HGS_Probability = round(HGS_Probability,4)) %>%
      mutate(HGS_Probability_Odds = if_else(two_Or_less_Games_id == "more_2_games", exp(predict(Half_Forward_Mod, newdata = Half_Forward_dt)),
                                            exp(predict(Half_Forward_Mod_2_less_Games, newdata = Half_Forward_dt)))) %>%
      mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
      mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
      select(ID, HGS_Probability, HGS_Odds) -> Half_Forward_dt
    
    
    ##-- key frwd rubix cube value box, displaying prob and odds using manpulated moef coefs feeed into GD mod  
    output$rubik_Prob_Half_Forward<- renderValueBox({
      valueBox(str_c(Half_Forward_dt$HGS_Probability * 100, " ", "%"),
               "Probability of being Highest Goal Scorer"
      )
    })
    
    output$rubik_ODDS_Half_Forward<- renderValueBox({
      valueBox(str_c(Half_Forward_dt$HGS_Odds, " to 1"),
               "Odds (Failure / Success)"
      )
    })
  })
  
  
  
  observe({
    ##--Mid_Frwd_Hybrid--## 
    meaningful_var_interactions_tab %>%
      filter(GameDay_Role == "Mid/Frwd_Hybrid") -> meaningful_var_interactions_tab_Mid_Frwd_Hybrid
    
    ##-- filter to player select by user
    ##-- mutate in selections mod for adjustable mod coefs
    baseline_Prediction_dt %>%
      filter(ID == input$Mid_Frwd_Hybrid_player_ID) %>%
      mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$Mid_Frwd_Hybrid_car_Pos_id %in% c(meaningful_var_interactions_tab_Mid_Frwd_Hybrid$Career_Position), # limited career posiition to mean car pos levels for mod> 
                                                            input$Mid_Frwd_Hybrid_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
      mutate(GD_Height_Categories = input$Mid_Frwd_Hybrid_gd_Height_id) %>%
      mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$Mid_Frwd_Hybrid_opp_Fantasy_ratio_id) %>%
      mutate(Cnt_Favs_Team_Level = as.numeric(input$Mid_Frwd_Hybrid_favs_Team_level_id)) %>%
      mutate(Cnt_Favs_Opp_Level = as.numeric(input$Mid_Frwd_Hybrid_favs_Opp_level_id)) %>%
      mutate(GD_TOG_Categories = as.numeric(input$Mid_Frwd_Hybrid_TOG_id)) %>%
      mutate(lagged_Venue_clusters = input$Mid_Frwd_Hybrid_clust_Venues_id) ->  Mid_Frwd_Hybrid_dt  
    
    
    ##-- GD mod feed in to produce prob / ODD of HGS, split by whether player had 2 or more gamesin  a seaosn
    Mid_Frwd_Hybrid_dt %>%
      ungroup() %>%
      mutate(HGS_Probability = if_else(two_Or_less_Games_id == "more_2_games", predict(Mid_Frwd_Hybrid_Mod, newdata = Mid_Frwd_Hybrid_dt, type =  "response"),
                                       predict(Mid_Frwd_Hybrid_Mod_2_less_Games, newdata = Mid_Frwd_Hybrid_dt, type =  "response"))) %>%
      mutate(HGS_Probability = round(HGS_Probability,4)) %>%
      mutate(HGS_Probability_Odds = if_else(two_Or_less_Games_id == "more_2_games", exp(predict(Mid_Frwd_Hybrid_Mod, newdata = Mid_Frwd_Hybrid_dt)),
                                            exp(predict(Mid_Frwd_Hybrid_Mod_2_less_Games, newdata = Mid_Frwd_Hybrid_dt)))) %>%
      mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
      mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
      select(ID, HGS_Probability, HGS_Odds) -> Mid_Frwd_Hybrid_dt
    
    
    ##-- key frwd rubix cube value box, displaying prob and odds using manpulated moef coefs feeed into GD mod  
    output$rubik_Prob_Mid_Frwd_Hybrid<- renderValueBox({
      valueBox(str_c(Mid_Frwd_Hybrid_dt$HGS_Probability * 100, " ", "%"),
               "Probability of being Highest Goal Scorer"
      )
    })
    
    output$rubik_ODDS_Mid_Frwd_Hybrid<- renderValueBox({
      valueBox(str_c(Mid_Frwd_Hybrid_dt$HGS_Odds, " to 1"),
               "Odds (Failure / Success)"
      )
    })
  })
  

  observe({
    ##-- Ruckman---## 
    meaningful_var_interactions_tab %>%
      filter(GameDay_Role == "Ruckman") -> meaningful_var_interactions_tab_Ruckman
    
    ##-- filter to player select by user
    ##-- mutate in selections mod for adjustable mod coefs
    baseline_Prediction_dt %>%
      filter(ID == input$Ruckman_player_ID) %>%
      mutate(Career_Positions_Meaningful_GD_Roles = if_else(input$Ruckman_car_Pos_id %in% c(meaningful_var_interactions_tab_Ruckman$Career_Position), # limited career posiition to mean car pos levels for mod> 
                                                            input$Ruckman_car_Pos_id, "The_Rest_Car_GD_Roles")) %>%
      mutate(GD_Height_Categories = input$Ruckman_gd_Height_id) %>%
      mutate(team_Cum_fantasy_Ratio_per_season_Categories_Opp = input$Ruckman_opp_Fantasy_ratio_id) %>%
      mutate(Cnt_Favs_Team_Level = as.numeric(input$Ruckman_favs_Team_level_id)) %>%
      mutate(Cnt_Favs_Opp_Level = as.numeric(input$Ruckman_favs_Opp_level_id)) %>%
      mutate(GD_TOG_Categories = as.numeric(input$Ruckman_TOG_id)) %>%
      mutate(lagged_Venue_clusters = input$Ruckman_clust_Venues_id) %>%
      mutate(Cnt_Ruckman_Team_Level = input$Ruckman_cnt_In_game) ->  Ruckman_dt  
    
    
    ##-- GD mod feed in to produce prob / ODD of HGS, split by whether player had 2 or more gamesin  a seaosn
    Ruckman_dt %>%
      ungroup() %>%
      mutate(HGS_Probability = if_else(two_Or_less_Games_id == "more_2_games", predict(Ruckman_Mod, newdata = Ruckman_dt, type =  "response"),
                                       predict(Ruckman_Mod_2_less_Games, newdata = Ruckman_dt, type =  "response"))) %>%
      mutate(HGS_Probability = round(HGS_Probability,4)) %>%
      mutate(HGS_Probability_Odds = if_else(two_Or_less_Games_id == "more_2_games", exp(predict(Ruckman_Mod, newdata = Ruckman_dt)),
                                            exp(predict(Ruckman_Mod_2_less_Games, newdata = Ruckman_dt)))) %>%
      mutate(HGS_Odds = if_else(1/HGS_Probability_Odds < 10, round(1/HGS_Probability_Odds, 2), round(1/HGS_Probability_Odds))) %>%       
      mutate(HGS_Odds = if_else(HGS_Odds > 10000, 10000, HGS_Odds)) %>% 
      select(ID, HGS_Probability, HGS_Odds) -> Ruckman_dt
    
    
    ##-- key frwd rubix cube value box, displaying prob and odds using manpulated moef coefs feeed into GD mod  
    output$rubik_Prob_Ruckman<- renderValueBox({
      valueBox(str_c(Ruckman_dt$HGS_Probability * 100, " ", "%"),
               "Probability of being Highest Goal Scorer"
      )
    })
    
    output$rubik_ODDS_Ruckman<- renderValueBox({
      valueBox(str_c(Ruckman_dt$HGS_Odds, " to 1"),
               "Odds (Failure / Success)"
      )
    })
  })
  


}


##-------------------- Tab 3: Pred rubik cube ----------------##



