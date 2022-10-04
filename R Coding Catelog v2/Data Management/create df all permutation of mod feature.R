##--- perm of every feature combination 
AFL_cont_Features_current_Season %>%
  tidyr::expand(GameDay_Role, Career_Positions_Meaningful_GD_Roles, GD_Height_Categories, GD_TOG_Categories,  
                Cnt_Favs_Team_Level, Cnt_Favs_Opp_Level, team_Cum_fantasy_Ratio_per_season_Categories_Opp, lagged_Venue_clusters ) -> rubik_Cube_dt