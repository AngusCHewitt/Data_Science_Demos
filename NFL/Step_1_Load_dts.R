###-- Upload PBP data from 2000 to most recent season and Scrape player positions and attributes from NFL.com
library(nflfastR) # fast load php and roster dt
library(tidyverse) # tidy dt
library(data.table) # big tables
library(nflreadr) # nfl tidyverse data sources

##-- pbp data since yr 2000 to 2021
##-- snap count startys from 2013 season so only need lead up of 1 years prior ro calc 8 week rolling average 
future::plan("multisession")
seasons <- 2021:2023
pbp_Seasons_2021_23 <- nflfastR::load_pbp(seasons)

##-- nfl roster dataset for each season since 2000 to 2021
future::plan("multisession")
player_Positions_2021_23 <- load_rosters(2021:2023)

##--clean pbp team names and player descs
pbp_Seasons_2021_23 %>%
  clean_pbp() -> pbp_Seasons_2021_23


##-- nfl scheule, scores, env vars, money spreads
schedule_2021_23 <- load_schedules(2022:2021)


##--  epsn qbr ratiing 2012:2012
espn_Qbr_rating_2021_23 <- load_espn_qbr("nfl", 2021:2023, "weekly")


dim(pbp_Seasons_2021_23)
# [1] 770 304   372

#rm(pbp_Seasons_2000_20, player_Positions_2000_20)
#length(unique(str_c(pbp_Seasons_2021_23$game_id, pbp_Seasons_2021_23$play_id)))


##-- player positions broken down to one position per season (using team season)
##--some player who were cut by a team are not given gsis ids
player_Positions_2021_23 %>% 
  group_by(gsis_id, season, position, full_name,  birth_date, height, weight) %>%
  nest() %>%
  dplyr::select(-data) -> player_Positions_per_Yr


dim(player_Positions_per_Yr)
#51168 7

##-- join PBP data with posiotion datast with player that recorded a passer stat
##-- clean PBP has been applied to github data, IDs standardised other plays fixed 
pbp_Seasons_2021_23 %>%
  dplyr::filter(!(is.na(passer_id))) %>%
  left_join(player_Positions_per_Yr, by = c(  "passer_id" =  "gsis_id" ,
                                              "season" =  "season")) -> Passers

##-- join PBP data with posiotion datast with player that recorded a rusher stat
pbp_Seasons_2021_23 %>%
  dplyr::filter(!(is.na(rusher_id))) %>%
  left_join(player_Positions_per_Yr, by = c(  "rusher_id" =  "gsis_id" ,
                                              "season" =  "season")) -> Runners

##-- player id is only option for punt and kick returners
pbp_Seasons_2021_23 %>%
  dplyr::filter(!(is.na(punt_returner_player_id))) %>%
  left_join(player_Positions_per_Yr, by = c( "punt_returner_player_id" =  "gsis_id" ,
                                             "season" =  "season")) -> Punt_returners

pbp_Seasons_2021_23 %>%
  dplyr::filter(!(is.na(kickoff_returner_player_id))) %>%
  left_join(player_Positions_per_Yr, by = c( "kickoff_returner_player_id" =  "gsis_id" ,
                                             "season" =  "season")) -> kick_returners

##-- join PBP data with posiotion datast with player that recorded a receiver stat
pbp_Seasons_2021_23 %>%
  dplyr::filter(!(is.na(receiver_id))) %>%
  left_join(player_Positions_per_Yr, by = c( "receiver_id" =  "gsis_id" ,
                                             "season" =  "season")) -> Receivers



##-- join passer position info to pbp dt    
Passers %>%
  dplyr::select(game_id, play_id, position, full_name, birth_date, height, weight) %>%
  right_join(pbp_Seasons_2021_23, by = c("game_id", "play_id")) -> pbp_Seasons_2021_23




##-- rename passer position vars   
pbp_Seasons_2021_23 %>%
  rename("passer_Positions"  = "position",
         "passer_DOB"  = "birth_date",
         "passer_Weight"  = "weight",
         "passer_Height"  = "height",
         "passer_Full_name"  = "full_name") %>%
  mutate(passer_Team  = posteam)-> pbp_Seasons_2021_23 



##-- join runners position info to PBP_Passers_receiver dt    
Runners %>%
  dplyr::select(game_id, play_id, position, full_name, birth_date, height, weight) %>%
  right_join(pbp_Seasons_2021_23, by = c("game_id", "play_id")) -> pbp_Seasons_2021_23


##-- rename WR positionsal vars   
pbp_Seasons_2021_23 %>%
  rename("runner_Positions"  = "position",
         "runner_DOB"  = "birth_date",
         "runner_Weight"  = "weight",
         "runner_Height"  = "height",
         "runner_Full_name"  = "full_name") %>%
  mutate(runner_Team  = posteam)-> pbp_Seasons_2021_23



##-- join runners position info to PBP_Passers_receiver dt    
kick_returners %>%
  dplyr::select(game_id, play_id,  position, full_name, birth_date, height, weight) %>%
  right_join(pbp_Seasons_2021_23, by = c("game_id", "play_id")) -> pbp_Seasons_2021_23


##-- rename WR positionsal vars   
pbp_Seasons_2021_23 %>%
  rename("kick_Returner_Positions"  = "position",
         "kick_Returner_DOB"  = "birth_date",
         "kick_Returner_Weight"  = "weight",
         "kick_Returner_Height"  = "height",
         "kick_Returner_Full_name"  = "full_name") %>%
  mutate(kick_Returner_team  = return_team)-> pbp_Seasons_2021_23


##-- join receiver position info to PBP_Passers dt    
Receivers %>%
  dplyr::select(game_id, play_id, position, full_name, birth_date, height, weight) %>%
  right_join(pbp_Seasons_2021_23, by = c("game_id", "play_id")) -> pbp_Seasons_2021_23


##-- rename rec positionsal vars   
pbp_Seasons_2021_23 %>%
  rename("receiver_Positions"  = "position",
         "receiver_DOB"  = "birth_date",
         "receiver_Weight"  = "weight",
         "receiver_Height"  = "height",
         "receiver_Full_name"  = "full_name") %>%
  mutate(receiver_Team  = posteam)-> pbp_Seasons_2021_23 


##-- join runners position info to PBP_Passers_receiver dt    
Punt_returners %>%
  dplyr::select(game_id, play_id, position, full_name, birth_date, height, weight) %>%
  right_join(pbp_Seasons_2021_23, by = c("game_id", "play_id")) -> pbp_Seasons_2021_23



##-- rename WR positionsal vars   
pbp_Seasons_2021_23 %>%
  rename("punt_Returner_Positions"  = "position",
         "punt_Returner_DOB"  = "birth_date",
         "punt_Returner_Weight"  = "weight",
         "punt_Returner_Height"  = "height",
         "punt_Returner_Full_name"  = "full_name") %>%
  mutate(punt_Returner_team  = return_team)-> pbp_Seasons_2021_23




##-- Create and ID, Full Name & Position  for Runner and Receivers (sum both at Gameday level in Step 3)
##-- include skilled position plauer stats for rushing, rec, kick returns and punt returns
pbp_Seasons_2021_23 %>%
  mutate(runner_Receiver_id = case_when(!(is.na(rusher_id)) ~ rusher_id,
                                        !(is.na(receiver_id)) ~ receiver_id,
                                        !(is.na(punt_returner_player_id)) ~ punt_returner_player_id,
                                        !(is.na(kickoff_returner_player_id)) ~ kickoff_returner_player_id,
                                        TRUE ~ as.character(NA))) %>%
  mutate(runner_Receiver_full_Nm = case_when(!(is.na(rusher_id)) ~ runner_Full_name,
                                             !(is.na(receiver_id)) ~ receiver_Full_name,
                                             !(is.na(punt_returner_player_id)) ~ punt_Returner_Full_name,
                                             !(is.na(kickoff_returner_player_id)) ~ kick_Returner_Full_name,
                                             TRUE ~ as.character(NA))) %>%
  mutate(runner_Receiver_position = case_when(!(is.na(rusher_id)) ~ runner_Positions,
                                              !(is.na(receiver_id)) ~ receiver_Positions,
                                              !(is.na(punt_returner_player_id)) ~ punt_Returner_Positions,
                                              !(is.na(kickoff_returner_player_id)) ~ kick_Returner_Positions,
                                              TRUE ~ as.character(NA))) %>%
  mutate(runner_Receiver_team = case_when(!(is.na(rusher_id)) ~ runner_Team,
                                          !(is.na(receiver_id)) ~ receiver_Team,
                                          !(is.na(punt_returner_player_id)) ~ punt_Returner_team,
                                          !(is.na(kickoff_returner_player_id)) ~ kick_Returner_team,
                                          TRUE ~ as.character(NA))) -> pbp_Seasons_2021_23



##-- Create and ID, Full Name & Position for QB that pass and run
##-- QB passing and rushing stats used for cluster cohorts (car position and GD role)
pbp_Seasons_2021_23 %>%
  mutate(qb_Rush_pass_ID = case_when( !(is.na(passer_id)) ~ passer_id,
                                      !(is.na(rusher_id)) ~ rusher_id,
                                      TRUE ~ as.character(NA))) %>% 
  mutate(qb_Rush_pass_Full_nm = case_when( !(is.na(passer_id)) ~ passer_Full_name,
                                           !(is.na(rusher_id)) ~ runner_Full_name,
                                           TRUE ~ as.character(NA)))  %>% 
  mutate(qb_Rush_pass_Full_position = case_when(!(is.na(passer_id)) ~ passer_Positions,
                                                !(is.na(rusher_id)) ~ runner_Positions,
                                                TRUE ~ as.character(NA))) %>% 
  mutate(qb_Rush_pass_Team = case_when(!(is.na(passer_id)) ~ passer_Team,
                                       !(is.na(rusher_id)) ~ runner_Team,
                                       TRUE ~ as.character(NA))) -> pbp_Seasons_2021_23



##-- Sense Checks
dim(pbp_Seasons_2021_23)
#[1] 770 304    410


##-- save perm file in per 2021 data file
save(pbp_Seasons_2021_23, file = 'data/Raw_dt/pbp_Seasons_2021_23.rds')# pbp data 2023-20
save(player_Positions_2021_23, file =   'data/Raw_dt/player_Positions_2021_23.rds') # roster dt 2023-20
save(schedule_2021_23, file =   'data/Raw_dt/schedule_2021_23.rds') # scheule, scores, env 2012:2021
save(espn_Qbr_rating_2021_23, file =   'data/Raw_dt/espn_Qbr_rating_2021_23.rds') # weekly snap cnts


rm(list = ls())  # caution: delete all objects in .GlobalEnv
gc()  # free system memory


table(pbp_Seasons_2021_23$qb_Rush_pass_ID > 20)
summary(schedule_2021_23$temp)


