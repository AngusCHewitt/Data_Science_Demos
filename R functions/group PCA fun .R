
##-- fot PCA model to cont vars 
fit_PCA_mod <- function(df) {
  
  df %>%
    select(rolling_HA_BABIP, rolling_HA_WHIP,
           rolling_HA_FIP,
           rolling_HA_soft_Contact) -> cont_vars
  
  ##-- fit PCA model, output PCA comps iwth enough info value 
  PCA_Model <- PCA(cont_vars, graph = FALSE, ncp = length(cont_vars) - 1) 
  
  ##- out PCA coords
  PCA_cluster_dt <- data.frame(PCA_Model$ind$coord)
}

##-- Gruop at pitch label nd home teamc cate and git PCA model to explanatory Pitchers vars
team_HA_level_pitching_SB_gamePK %>%
  arrange(game_date, game_pk, Pitchers_Team) %>%
  group_by(Pitcher_lables, home_Team_cate) %>%
  nest() %>%
  mutate(PCA_mod = map(data, ~fit_PCA_mod(.))) %>%
  unnest() %>%
  ungroup() -> team_HA_level_pitching_SB_gamePK  




