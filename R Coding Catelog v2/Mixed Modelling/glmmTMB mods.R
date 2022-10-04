##-- target snap cant obs with na snap cnt values 
skilled_Position_gd_Stats_2013_20 %>%
  filter(is.na(offense_snaps)) %>%
  mutate(season_int = as.integer(season - 2000)) -> obs_NA_snaps_Cnts 

##-- is obs with snap values
##- add total number off an st snap in the entire game
skilled_Position_gd_Stats_2013_20 %>%
  filter(!(is.na(offense_snaps))) %>%
  group_by(game_id, runner_Receiver_team) %>%
  mutate(max_Off_snaps = max(offense_snaps)) %>%
  mutate(max_ST_snaps = max(st_snaps)) %>%
  mutate(tot_Off_snap_In_game = round(max_Off_snaps / (offense_pct/100))) %>%
  mutate(tot_St_snap_In_game =  round(max_ST_snaps / (st_pct/100))) %>%
  mutate(season_int = as.integer(season - 2000)) %>%
  mutate(offense_snaps = as.integer(offense_snaps)) %>%
  mutate(offense_Percentage_tot = offense_pct/100) %>% 
  mutate(offense_Percentage_tot = offense_pct/100) -> skilled_Position_gd_Stats_2013_20

summary(skilled_Position_gd_Stats_2013_20)


dim(skilled_Position_gd_Stats_2013_20)
#[1] 39071    5



##-- baseline Mixed Mods
player_snaps_mod <-  glmmTMB(offense_pct ~   tot_Off_snap_In_game +  (rushing_Attempts + passing_Targets | runner_Receiver_position),
                             data = skilled_Position_gd_Stats_2013_20,family =  betabinomial(link = "logit"))



hist(skilled_Position_gd_Stats_2013_20$offense_pct)

plot(player_snaps_mod)
ranef(player_snaps_mod)

anova(player_snaps_mod)

?glmmTMB


##- Cross vals create 10 test samples = 20 % of data
##=-- kfolds spliuts data into smaller and smaller groups whilst crossv_mc split data into 2 groups reps "x" times
skilled_Position_gd_Stats_2013_20 %>%
  crossv_mc(5) -> GameDay_Cv


GameDay_Cv %>% 
  mutate(train = map(train, as_tibble)) %>%
  mutate(test = map(test, as_tibble)) %>%
  mutate(model = map(train, ~  glmer(offense_snaps ~  tot_Off_snap_In_game +  (rushing_Attempts + passing_Targets | runner_Receiver_position),
                                     data = skilled_Position_gd_Stats_2013_20, family = "poisson",
                                     control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0L))) %>%
  mutate(preds = map2(test, model,  add_predictions, type = "response")) -> test

test %>%
  select(preds) %>%
  unnest(preds) %>%
  mutate(res = pred - offense_snaps) %>%
  select(res) %>%
  summary()


summary(skilled_Position_gd_Stats_2013_20$offense_snaps)

