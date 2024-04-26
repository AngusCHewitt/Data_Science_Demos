
##-- no need to split Home and Away variable transformations as the model calculations are no longer group by Home and Away
SP_game_HA_Level_performance_Dt %>%
  select(game_date, game_pk, Starter_Bullpen_Ids, person_full_name, Pitchers_Team, home_Team_cate,
         rolling_HA_Walks_per_9, rolling_HA_Strikeouts_per_9, innings_Pitched,
         rolling_HA_soft_Ground_ball_per_9:rolling_HA_hard_LD_ball_per_9) %>%
  gather("var", "value", -game_date, -game_pk, -Starter_Bullpen_Ids, -person_full_name, -Pitchers_Team, -home_Team_cate, -innings_Pitched) %>%
  group_by(var) %>%
  mutate(var_Quants = quantile(value, 0.995)) %>% # use 99th percentile to cleanse outliers
  mutate(value = if_else(value > var_Quants, var_Quants + abs(log(value/var_Quants)), value)) %>%
  select(-var_Quants) %>%
  nest() %>%
  mutate(nat_Spline = map(data, ~nat_Splines_fun(.$value)*10)) %>%
  ungroup() %>%
  unnest() %>%
  select(-value) %>%
  spread(var, nat_Spline) -> SP_game_HA_Level_performance_Dt_ns_Trans
