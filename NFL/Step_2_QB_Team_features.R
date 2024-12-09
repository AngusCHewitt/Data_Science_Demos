###-- Upload PBP data from 2000 to most recent season and Scrape player positions and attributes from NFL.com
library(nflfastR) # fast load php and roster dt
library(tidyverse) # tidy dt
library(data.table) # big tables
library(nflreadr) # nfl tidyverse data sources

##-------------- Step 1 C - Join Position groups to PBP dataset -------------------## 

##-- load player roster with positional info and PBP dataset 2000-19 
load('data/Raw_dt/pbp_Seasons_2021_23.rds')# pbp data 2006-20

##-- filter to position = QBs, obs = 9,460 no NA's names, No trick plays with wr or rbs through the balls 
length(unique(pbp_Seasons_2021_23$qb_Rush_pass_ID))
# 597 unique qb ids


##-- sumamrise pbp data at a GD for qbs 
pbp_Seasons_2021_23 %>%
  filter(qb_Rush_pass_Full_position == "QB" & play_type %in% c("run", "pass")) %>% # only interesy in run and pass plays
  group_by(qb_Rush_pass_ID, game_id, qb_Rush_pass_Full_nm, qb_Rush_pass_Team,  game_date, season, week, season_type, posteam_type, defteam) %>%
  summarise( pass_Completions = sum(if_else( complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE), ##-- tot pass comps, tds and attempts
             passing_TDs = sum(if_else( pass_touchdown == 1 & play_type == "pass", 1 , 0),  na.rm = TRUE), # pass tds
             pass_Attempts = sum(if_else( play_type == "pass", 1, 0),  na.rm = TRUE), # tot pass attempts
             
             total_Pass_yards =  sum(if_else( play_type == "pass", passing_yards, 0),  na.rm = TRUE), # total pass yards
             total_Air_yards =  sum(if_else( play_type == "pass", air_yards, 0),  na.rm = TRUE), # total ari yards
             total_Air_yards_Comp_passes =  sum(if_else( play_type == "pass" & complete_pass == 1, air_yards, 0),  na.rm = TRUE), # tot air yards comp passes
             total_YAC =  sum(if_else( play_type == "pass", yards_after_catch, 0),  na.rm = TRUE), # yards after the catch
             
             attempt_Checkdown_pass = sum(if_else( air_yards <= 1 & play_type == "pass",1,0),  na.rm = TRUE), ##-- pass attemps by pass length
             attempt_Short_pass = sum(if_else( air_yards >  1 & air_yards <=  10 & play_type == "pass",1,0),  na.rm = TRUE),
             attempt_Intermediate_pass = sum(if_else( air_yards >  10 & air_yards <=  20 & play_type == "pass",1,0),  na.rm = TRUE),
             attempt_Deep_pass = sum(if_else( air_yards > 20 & play_type == "pass",1,0),  na.rm = TRUE), # deep pass attempts
             
             comp_Checkdown_pass = sum(if_else( air_yards <= 1 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE), ##-- pass comps by poass length
             comp_Short_pass = sum(if_else( air_yards >  1 & air_yards <=  10 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
             comp_Intermediate_pass = sum(if_else( air_yards >  10 & air_yards <=  20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
             comp_Deep_pass = sum(if_else( air_yards > 20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
             pass_Intercepted = sum(if_else( interception & play_type == "pass", 1,0), na.rm = TRUE), # raiot of td tointecept good perf indicator
             
             qb_Pos_designed_Rushes =  sum(if_else( play_type == "run" & qb_scramble == 0 & yards_gained > 0, 1, 0),  na.rm = TRUE), # split rushing yards into desinged rusn and scramble drill plays
             qb_Pos_designed_Rushes_small_gain =  sum(if_else( play_type == "run" & qb_scramble == 0 & yards_gained > 0 & yards_gained <= 5, 1, 0),  na.rm = TRUE), # split rushing yards into desinged rusn and scramble drill plays
             qb_Pos_designed_Rushes_intermediate_gain =  sum(if_else( play_type == "run" & qb_scramble == 0 & yards_gained > 5 & yards_gained <= 10, 1, 0),  na.rm = TRUE), # split rushing yards into desinged rusn and scramble drill plays
             qb_Pos_designed_Rushes_explosive_gain =  sum(if_else( play_type == "run" & qb_scramble == 0 & yards_gained > 10, 1, 0),  na.rm = TRUE), # split rushing yards into desinged rusn and scramble drill plays
             qb_Pos_designed_Rushes_yards =  sum(if_else( play_type == "run" & qb_scramble == 0 & yards_gained > 0, yards_gained, 0),  na.rm = TRUE), 
             
             qb_Pos_scrambles =  sum(if_else( play_type == "run" & qb_scramble == 1 & yards_gained > 0, 1, 0),  na.rm = TRUE), # scramble drills
             qb_Pos_scrambles_small_gain =  sum(if_else( play_type == "run" & qb_scramble == 1 & yards_gained > 0 & yards_gained <= 5, 1, 0),  na.rm = TRUE),
             qb_Pos_scrambles_intermediate_gain =  sum(if_else( play_type == "run" & qb_scramble == 1 & yards_gained > 5 & yards_gained <= 10, 1, 0),  na.rm = TRUE),
             qb_Pos_scrambles_explosive_gain =  sum(if_else( play_type == "run" & qb_scramble == 1 & yards_gained > 10, 1, 0),  na.rm = TRUE),
             qb_Pos_scrambles_Yards =  sum(if_else( play_type == "run" & qb_scramble == 1 & yards_gained > 0, yards_gained, 0),  na.rm = TRUE),
             qb_Rushing_tds =  sum(if_else( touchdown == 1 & play_type == "run",1,0),  na.rm = TRUE),
           
             qb_Explosive_rushes =  sum(if_else( play_type == "run" & yards_gained > 10, 1, 0),  na.rm = TRUE), ## both scramble and rush yards
             qb_Intermediate_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 5 & yards_gained <= 10, 1, 0),  na.rm = TRUE),
             qb_Small_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 0 & yards_gained <= 5, 1, 0),  na.rm = TRUE),
             qb_Positive_rushes =  sum(if_else( play_type == "run" & yards_gained > 0, 1, 0),  na.rm = TRUE),
             qb_Pos_rushing_Yards =  sum(if_else( play_type == "run" & yards_gained > 0, yards_gained, 0),  na.rm = TRUE), 
             
             no_Run_pass_Snaps = sum(if_else(!(is.na(play_id)),1,0))) -> QB_GameDay_stats_2021_23


dim(QB_GameDay_stats_2021_23)
#9,940 43

##-- find the max of each game will give you the primary qb
##-- take the first row for each pbp
first_obs_fun <- function(df) {
  df[1,]%>%
    rename("id_Primary_qb" = qb_Rush_pass_ID) %>%
    select(id_Primary_qb)}

##-- need to assign one each game ID in team, only one qb type, create a var for number qb player in 1 game (info for later model) 2006_01_MIA_PIT
##-- qualioty check qb which takes most snapes
QB_GameDay_stats_2021_23 %>%
  arrange(game_id, qb_Rush_pass_Team, desc(no_Run_pass_Snaps), desc(pass_Attempts), desc(total_Pass_yards)) %>%
  group_by(game_id, qb_Rush_pass_Team) %>%
  nest() %>%
  mutate(most_Qb_snaps = map(data, ~first_obs_fun(.))) %>% # remove dup min values
  unnest(data, most_Qb_snaps) %>%
  mutate(primary_QB = if_else(qb_Rush_pass_ID == id_Primary_qb,"primary_QB", "secondary_QB")) %>%
  select(-id_Primary_qb) -> QB_GameDay_stats_2021_23



dim(QB_GameDay_stats_2021_23)
# 9940, 47

table(QB_GameDay_stats_2021_23$primary_QB)
# 8,581 1,359

##-- take the first row for each pbp
first_obs_fun <- function(df) {
  df[1,]%>%
    rename("first_QB_snap_Qb_id" = qb_Rush_pass_ID) %>%
    select(first_QB_snap_Qb_id)}


##-- id thestarting qb to label injury and benched qbs
pbp_Seasons_2021_23 %>%
  filter(qb_Rush_pass_Full_position == "QB" & play_type %in% c("run", "pass")) %>% # only interesy in run and pass plays
  arrange(game_id, play_id, qb_Rush_pass_ID) %>%
  group_by(game_id, qb_Rush_pass_Team) %>%
  nest() %>%
  mutate(id_QB_take_First_snap = map(data, ~first_obs_fun(.)))%>%
  select(-data) %>%
  unnest(id_QB_take_First_snap) -> first_Snap_qb_tb


##-- left join first qb label to qb gameday dt
QB_GameDay_stats_2021_23 %>%
  left_join(first_Snap_qb_tb, by = c("game_id", "qb_Rush_pass_Team")) -> QB_GameDay_stats_2021_23


##-- label qb starter 
QB_GameDay_stats_2021_23 %>%
  mutate(starting_QB = if_else(qb_Rush_pass_ID == first_QB_snap_Qb_id,"starting_QB", "backup_QB")) %>%
  select(-first_QB_snap_Qb_id) -> QB_GameDay_stats_2021_23


dim(QB_GameDay_stats_2021_23)
# 9268 47

##-- team def level datasets --##

##-- intested in only run+pass plays
pbp_Seasons_2021_23 %>%
  filter(play_type %in% c("run", "pass")) -> pbp_Seasons_2021_23


dim(pbp_Seasons_2021_23)
# 503183 372


##-- team and opp level stats 
pbp_Seasons_2021_23 %>%
  mutate(defteam_type = if_else(posteam_type == "home", "away", "home")) %>%
  group_by(game_date, defteam, defteam_type) %>%
  summarise(           pass_Attempts_Opp_level = sum(if_else( play_type == "pass", 1, 0),  na.rm = TRUE),
                       rush_Attempts_Opp_level =  sum(if_else( play_type == "run", 1, 0),  na.rm = TRUE), 
                       total_Run_pass_plays_Opp_level = (pass_Attempts_Opp_level + rush_Attempts_Opp_level),
                       
                       total_Pass_yards_Opp_Level =  sum(if_else( play_type == "pass", passing_yards, 0),  na.rm = TRUE),
                       total_Air_yards_Opp_Level =  sum(if_else( play_type == "pass", air_yards, 0),  na.rm = TRUE),
                       total_Air_yards_Comp_passes_Opp_Level =  sum(if_else( play_type == "pass" & complete_pass == 1, air_yards, 0),  na.rm = TRUE),
                       total_YAC_Opp_Level =  sum(if_else( play_type == "pass", yards_after_catch, 0),  na.rm = TRUE),
                       
                       total_Rush_yards_Opp_Level =  sum(if_else(play_type == "run", rushing_yards, 0),  na.rm = TRUE),
                       total_Yards_Opp_level = total_Pass_yards_Opp_Level + total_Rush_yards_Opp_Level,
                       Pass_percentage_Opp_Level = pass_Attempts_Opp_level / (pass_Attempts_Opp_level + rush_Attempts_Opp_level),
                       
                       comp_Checkdown_pass = sum(if_else( air_yards <= 1 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE), ##-- pass comps by poass length
                       comp_Short_pass = sum(if_else( air_yards >  1 & air_yards <=  10 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       comp_Intermediate_pass = sum(if_else( air_yards >  10 & air_yards <=  20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       comp_Deep_pass = sum(if_else( air_yards > 20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       pass_Intercepted = sum(if_else( interception & play_type == "pass", 1,0), na.rm = TRUE),  # raiot of td tointecept good perf indicator
                       
                       Explosive_rushes =  sum(if_else( play_type == "run" & yards_gained > 10, 1, 0),  na.rm = TRUE), ## both scramble and rush yards
                       Intermediate_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 5 & yards_gained <= 10, 1, 0),  na.rm = TRUE),
                       Small_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 0 & yards_gained <= 5, 1, 0),  na.rm = TRUE)) -> def_level_GD_stats

dim(def_level_GD_stats)
# 8584 21

##-- team and opp level stats 
pbp_Seasons_2021_23 %>%
  group_by(game_date, posteam, posteam_type) %>%
  summarise(           pass_Attempts_Team_level = sum(if_else( play_type == "pass", 1, 0),  na.rm = TRUE), # pass attempts
                       rush_Attempts_Team_level =  sum(if_else( play_type == "run", 1, 0),  na.rm = TRUE), # run attempts
                       total_Run_pass_plays_Team_level = (pass_Attempts_Team_level + rush_Attempts_Team_level), # total pass and run snaps
                       
                       total_Pass_yards_Team_Level =  sum(if_else( play_type == "pass", passing_yards, 0),  na.rm = TRUE), # tot pass yards
                       total_Air_yards_Team_Level =  sum(if_else( play_type == "pass", air_yards, 0),  na.rm = TRUE), # total air yards
                       total_Air_yards_Comp_passes_Team_Level =  sum(if_else( play_type == "pass" & complete_pass == 1, air_yards, 0),  na.rm = TRUE), # tot air yards comp pases
                       total_YAC_Team_Level =  sum(if_else( play_type == "pass", yards_after_catch, 0),  na.rm = TRUE), # YAC
                       
                       total_Rush_yards_Team_Level =  sum(if_else(play_type == "run", rushing_yards, 0),  na.rm = TRUE), # total rush yards
                       total_Yards_Team_level = total_Pass_yards_Team_Level + total_Rush_yards_Team_Level, # total yards
                       Pass_percentage_Team_Level = pass_Attempts_Team_level / (pass_Attempts_Team_level + rush_Attempts_Team_level), # percentage of pass attempts]
                       
                       comp_Checkdown_pass = sum(if_else( air_yards <= 1 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE), ##-- pass comps by poass length
                       comp_Short_pass = sum(if_else( air_yards >  1 & air_yards <=  10 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       comp_Intermediate_pass = sum(if_else( air_yards >  10 & air_yards <=  20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       comp_Deep_pass = sum(if_else( air_yards > 20 & complete_pass == 1 & play_type == "pass", 1, 0),  na.rm = TRUE),
                       pass_Intercepted = sum(if_else( interception & play_type == "pass", 1,0), na.rm = TRUE),  # raiot of td tointecept good perf indicator
                       
                       Explosive_rushes =  sum(if_else( play_type == "run" & yards_gained > 10, 1, 0),  na.rm = TRUE), ## both scramble and rush yards
                       Intermediate_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 5 & yards_gained <= 10, 1, 0),  na.rm = TRUE),
                       Small_gain_Rushes =  sum(if_else( play_type == "run" & yards_gained > 0 & yards_gained <= 5, 1, 0),  na.rm = TRUE),
                       
                       shotgun_pass_Attempts = sum(if_else( shotgun == 1 & play_type == "pass", 1, 0), na.rm = TRUE), ##-- id type of off could be a good rand var 
                       no_Huddle_pass_Attempts = sum(if_else( no_huddle == 1 & play_type == "pass" , 1, 0),  na.rm = TRUE),
                       direct_Snap_pass_Attempts = sum(if_else( qb_dropback == 1 & shotgun == 0 & play_type == "pass", 1, 0),  na.rm = TRUE)) -> Team_level_GD_stats

dim(Team_level_GD_stats)
# 8584 24

##-- save a actuals 
pbp_Seasons_2021_23 %>%
  arrange(season, week) %>%
  select(season, week) %>%
  unique() %>%
  mutate(obs = 1) %>%
  mutate(cum_Rounds = cumsum(obs)) %>%
  select(-obs) -> tot_Round_no_Tb


##-- left join cum mths 
QB_GameDay_stats_2021_23 %>%
  ungroup() %>%
  left_join(tot_Round_no_Tb, by = c("season", "week")) -> QB_GameDay_stats_2021_23

dim(QB_GameDay_stats_2021_23)
#9942 46

##-- add lagged round number so that lagged aftersaving the final actual dt
pbp_Seasons_2021_23 %>%
  arrange(season, week) %>%
  select(season, week) %>%
  unique() %>%
  mutate(obs = 1) %>%
  mutate(cum_Rounds = cumsum(obs)) %>%
  mutate(cum_Rounds = lag(cum_Rounds)) %>%
  select(-obs) -> tot_Round_no_Tb


##-- Qb, team and def level var actuals GD stats
save(QB_GameDay_stats_2021_23, file = "data/Curated_dts/Step_2_qb_GD_Stats_2021_23.rds") ##-- qb level var
save(def_level_GD_stats, file =  "data/Curated_dts/Step_2_def_level_GD_stats_2021_23.rds") ##== def team actuals, last obs, H/A
save(Team_level_GD_stats, file =  "data/Curated_dts/Step_2_team_level_GD_stats_2021_23.rds") ##== off team actuals, last obs, H/A
save(tot_Round_no_Tb, file =  "data/Curated_dts/Step_2_tot_Round_no_Tb.rds") ##== off team actuals, last obs, H/A



rm(list = ls())  # caution: delete all objects in .GlobalEnv
gc()  # free system memory


