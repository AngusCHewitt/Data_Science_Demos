##-- glm mod allocate prob of HGS for each player to determine how many favs going into each match
##-- test cnt player prob < .1 or .2, cnt at team and opp level
glm_favs_mod <-  glmer(HGS_Binary ~ I(Goal_scoring_Fatasy_scores_centre + Goal_scoring_Fatasy_scores_pos_sd + Goal_scoring_Fatasy_scores_neg_sd) + 
                         first_Game_season_Factor + Career_Position + GameDay_Role +
                         + (1 | yr_int) , data = AFL_Actuals_Features_2021, family = binomial(link = "logit"),
                       control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0L)

summary(glm_favs_mod)
ranef(glm_favs_mod)

