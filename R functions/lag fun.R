
##-- select mod var and lag vars
lag_dt_fun <- function(df) {
  
  df %>%  
    gather("var", "value", Dim.1:Dim.3) %>%
    group_by(var) %>%  
    mutate(value = lag(value)) %>%
    spread(var, value) -> df 
}


##-- lag dim vars, does give youoption of keeping acutals and lagged vars, lag by pitcher (may change teams during season)
team_HA_level_pitching_SB_gamePK %>%
  arrange(Starter_Bullpen_Ids, game_date, game_pk) %>%
  group_by(Starter_Bullpen_Ids) %>%
  nest() %>%
  mutate(lag_vars = map(data, ~lag_dt_fun(.))) %>%
  select(-data) %>%
  select(lag_vars) %>%
  ungroup() %>%
  unnest()  -> team_HA_level_pitching_SB_gamePK


