
##-- sum max snap of each game
skilled_Position_gd_Stats_2013_20 %>%
  group_by(game_id, runner_Receiver_team) %>%
  summarise(max_Off_snaps = max(offense_snaps, na.rm = TRUE),
            max_ST_snaps = max(st_snaps, na.rm = TRUE)) -> sum_tab

##-- left join max to each game 
skilled_Position_gd_Stats_2013_20 %>%
  left_join(sum_tab, by = c("game_id",
                            "runner_Receiver_team")) -> skilled_Position_gd_Stats_2013_20


skilled_Position_gd_Stats_2013_20 %>%
  select(rushing_Attempts, passing_Targets, kickoff_Returns, punt_Returns, offense_snaps:max_ST_snaps) -> mod_dataset


##   (for the imputation step)
 nb <- estim_ncpPCA(mod_dataset,ncp.max=5) ## Time consuming, nb = 2

## Imputation
res.comp <- imputePCA(mod_dataset,ncp=4)

summary(res.comp$completeObs[,5] -  res.comp$fittedX[,5])




## Imputation
res.comp <- imputePCA(orange,ncp=2)


