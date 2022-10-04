##-- calc cum geometric mean
AFL_cate_Features_current_Season %>%
  arrange(ID,Date) %>%
  select(ID, Date, Season, lagged_SOG, lagged_Goals, lagged_Behinds, lagged_Marks.Inside.50, lagged_Goal_scoring_Fatasy_scores, 
         lagged_Clearances, lagged_Marks_Out50, lagged_Uncontested.Possessions, lagged_Bounces, 
         lagged_Contested.Possessions_minus_CM, lagged_Contested.Marks, lagged_TOG, lagged_Hit.Outs, lagged_Goal.Assists ) %>%
  gather("var","value",-ID, -Date, -Season) %>%
  group_by(ID, var, Season) %>%
  nest() %>%
  mutate(cummean = map(data, ~cummean(.$value))) %>%
  unnest() %>%
  mutate(cummean = if_else(is.na(cummean), value, cummean)) %>%
  mutate(cummean_nm = str_c("GeoMean_Lagged","_",substr(var, 8,40))) %>%
  ungroup() %>%
  select(-var,-value) %>%
  spread(cummean_nm, cummean) -> GeoMean_vars


##-- calc cum geometric cum sd * 2 (95% of ND distrn)
##-- first gamne ofn each round SD = 0, mixed mod have acc for this 
AFL_cate_Features_current_Season %>%
  arrange(ID,Date) %>%
  select(ID, Date, Season, lagged_SOG, lagged_Goals, lagged_Behinds, lagged_Marks.Inside.50, lagged_Goal_scoring_Fatasy_scores, 
         lagged_Clearances, lagged_Marks_Out50, lagged_Uncontested.Possessions, lagged_Bounces, 
         lagged_Contested.Possessions_minus_CM, lagged_Contested.Marks, lagged_TOG, lagged_Hit.Outs, lagged_Goal.Assists) %>%
  gather("var","value",-ID, -Date, -Season) %>%
  group_by(ID, var, Season) %>%
  nest() %>%
  mutate(cumsd = map(data, ~sqrt(cumvar(.$value)*2)))  %>%
  unnest() %>%
  mutate(cumsd = if_else(is.na(cumsd), 0, cumsd)) %>%
  mutate(cumsd_nm = str_c("GeoSD_Lagged","_",substr(var, 8,40))) %>%
  ungroup() %>%
  select(-var,-value) %>%
  spread(cumsd_nm, cumsd) -> SD_vars


##-- calc cum median
AFL_cate_Features_current_Season %>%
  arrange(ID,Date) %>%
  select(ID, Date, Season, lagged_SOG, lagged_Goals, lagged_Marks.Inside.50, lagged_Goal_scoring_Fatasy_scores, 
         lagged_Disposals, lagged_Contested.Possessions_minus_CM, lagged_Hit.Outs, lagged_Wingman_stats, lagged_Mid_stats ) %>%
  gather("var","value",-ID, -Date, -Season) %>%
  group_by(ID, var, Season) %>%
  nest() %>%
  mutate(cummedian = map(data, ~cumquant(.$value, 0.5))) %>%
  unnest() %>%
  mutate(cummedian = if_else(is.na(cummedian), value, cummedian)) %>%
  mutate(cummedian_nm = str_c("GeoMedian_Lagged","_",substr(var, 8,40))) %>%
  ungroup() %>%
  select(-var,-value) %>%
  spread(cummedian_nm, cummedian) -> cum_Median_vars


##-- calc cum 90th percentile
##-- first gamne ofn each round SD = 0, mixed mod have acc for this 
AFL_cate_Features_current_Season %>%
  arrange(ID,Date) %>%
  select(ID, Date, Season, lagged_SOG, lagged_Goals, lagged_Marks.Inside.50, lagged_Goal_scoring_Fatasy_scores, 
         lagged_Disposals, lagged_Contested.Possessions_minus_CM, lagged_Hit.Outs, lagged_Wingman_stats, lagged_Mid_stats ) %>%
  gather("var","value",-ID, -Date, -Season) %>%
  group_by(ID, var, Season) %>%
  nest() %>%
  mutate(cum_90_quant = map(data, ~cumquant(.$value, 0.90)))  %>%
  unnest() %>%
  mutate(cum_90_quant = if_else(is.na(cumsd), 0, cumsd)) %>%
  mutate(cum_90_quant_nm = str_c("Geo_90_quant_Lagged","_",substr(var, 8,40))) %>%
  ungroup() %>%
  select(-var,-value) %>%
  spread(cum_90_quant_nm, cum_90_quant) -> range_Upper_vars


##-- calc cum 10th percentile
##-- first gamne ofn each round SD = 0, mixed mod have acc for this 
AFL_cate_Features_current_Season %>%
  arrange(ID,Date) %>%
  select(ID, Date, Season, lagged_SOG, lagged_Goals, lagged_Marks.Inside.50, lagged_Goal_scoring_Fatasy_scores, 
         lagged_Disposals, lagged_Contested.Possessions_minus_CM, lagged_Hit.Outs, lagged_Wingman_stats, lagged_Mid_stats ) %>%
  gather("var","value",-ID, -Date, -Season) %>%
  group_by(ID, var, Season) %>%
  nest() %>%
  mutate(cum_10_quant = map(data, ~cumquant(.$value, 0.10)))  %>%
  unnest() %>%
  mutate(cum_10_quant = if_else(is.na(cumsd), 0, cumsd)) %>%
  mutate(cum_10_quant_nm = str_c("Geo_10_quant_Lagged","_",substr(var, 8,40))) %>%
  ungroup() %>%
  select(-var,-value) %>%
  spread(cum_10_quant_nm, cum_10_quant) -> range_Lower_vars




##-- Combine lagged SD and Mean into One var (additive) rep range part of the model median being central tendency
AFL_cate_Features_current_Season %>%
  mutate(GeoSD_Mean_Lagged_Marks.Inside.50 = GeoSD_Lagged_Marks.Inside.50 + GeoMean_Lagged_Marks.Inside.50,
         GeoSD_Mean_Lagged_SOG = GeoSD_Lagged_SOG + GeoMean_Lagged_SOG,
         GeoSD_Mean_Lagged_Goals = GeoSD_Lagged_Goals + GeoMean_Lagged_Goals,
         GeoSD_Mean_Lagged_Goal_scoring_Fatasy_scores = GeoSD_Lagged_Goal_scoring_Fatasy_scores + GeoMean_Lagged_Goal_scoring_Fatasy_scores,
         GeoSD_Mean_Lagged_Contested.Possessions_minus_CM = GeoSD_Lagged_Contested.Possessions_minus_CM + GeoMean_Lagged_Contested.Possessions_minus_CM,
         GeoSD_Mean_Lagged_Contested.Marks = GeoSD_Lagged_Contested.Marks + GeoMean_Lagged_Contested.Marks,
         GeoSD_Mean_Lagged_Hit.Outs = GeoSD_Lagged_Hit.Outs + GeoMean_Lagged_Hit.Outs,
         GeoSD_Mean_Lagged_Behinds = GeoSD_Lagged_Behinds + GeoMean_Lagged_Behinds,
         GeoSD_Mean_Lagged_Goal.Assists = GeoSD_Lagged_Goal.Assists + GeoMean_Lagged_Goal.Assists,
         GeoSD_Mean_Lagged_Clearances = GeoSD_Lagged_Clearances + GeoMean_Lagged_Clearances,
         GeoSD_Mean_Lagged_Marks_Out50 = GeoSD_Lagged_Marks_Out50 + GeoMean_Lagged_Marks_Out50,
         GeoSD_Mean_Lagged_Uncontested.Possessions = GeoSD_Lagged_Uncontested.Possessions + GeoMean_Lagged_Uncontested.Possessions,
         GeoSD_Mean_Lagged_Bounces = GeoSD_Lagged_Bounces + GeoMean_Lagged_Bounces) -> AFL_cate_Features_current_Season


