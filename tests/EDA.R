library(tidyverse)
#library(effects) # visualise models effects
#library(ggeffects) # plot mixd effects
library(splines) # natrtual spline NL relationship
#library(vcd)
#library(rattle) # cate binning funs
library(sjPlot) #mixed effects plots
#library(GGally) # visual pairs
#library(FactoMineR) # PCA mods
#library(factoextra) # PCa extars
#library(corrplot) # auto corr plot
#library(lubridate)
#library(cluster) # cluster funs
#library(broom) # idy model sum dt
library(modelr) # addm pred to df 
#library(statmod) # glm quanile residauls 
library(missMDA) # replace NA values using PCA
library(lme4) # mixed models 


liver_dt <- read_csv("data/indian_liver_patient.csv")

# 1 = Liver disease
# 2 = No “”
liver_dt %>%
  mutate(target_Var = if_else(Dataset < 2, 1L, 0L)) -> liver_dt

prop.table(table(liver_dt$target_Var, liver_dt$Gender),2)
# 0 == 167 no liver disease
# 1 == 416 liver desease 
# 
# # explanatory var hists  
# summary(liver_dt$Age)
# hist(liver_dt$Alkaline_Phosphotase)
# hist(log(liver_dt$Age+1))
# 
# 
# ## total protein disnrn very similar between 0/1
# liver_dt %>%
#   ggplot(aes(x = Total_Protiens, fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# 
# 
# ## total Direct_Bilirubin disnrn very similar between 0/1
# ## liver siease group has a very low total billiubin, health group much higher variance 
# liver_dt %>%
#   ggplot(aes(x = log(Total_Bilirubin + 1), fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# 
# ## total Direct_Bilirubin disnrn very similar between 0/1
# ## same as above
# liver_dt %>%
#   ggplot(aes(x = log(Direct_Bilirubin + 1), fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# ## total Direct_Bilirubin disnrn very similar between 0/1
# ## again alkaine phoshhotase is low in the liver disease group, much more variance in 0
# liver_dt %>%
#   ggplot(aes(x = log(Alkaline_Phosphotase + 1), fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# ## same as above 
# liver_dt %>%
#   ggplot(aes(x = log(Alamine_Aminotransferase + 1), fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# ## ""
# liver_dt%>%
#   ggplot(aes(x = log(Aspartate_Aminotransferase + 1), fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# 
# ## distrn very simiar for total proteins 
# liver_dt %>%
#   ggplot(aes(x = Total_Protiens, fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# 
# ## liver diease multi modular 
# liver_dt %>%
#   ggplot(aes(x = Albumin, fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# ## liver diease multi modular 
# liver_dt %>%
#   ggplot(aes(x = Albumin_and_Globulin_Ratio, fill = as.factor(target_Var))) + geom_density(alpha = .2)
# 
# 
# ## include demo factor like age and gender
# prop.table(table(liver_dt$Gender, liver_dt$target_Var),1) # gender is a factor, males more loikely to have llver disease
# ## 441 M, 142 F 
# 
# ## target var ~ age
# ## hard to visualsie with other explanatory variables 
# liver_dt %>%
#   ggplot(aes(x = Age, y = as.factor(target_Var))) + geom_point() +  geom_smooth(method = "glm", se = F, 
#    method.args = list(family = "binomial"))
# 
# ## analysis corr between num explantory vars
# ## only tests which are measuring the same compound are highly corr ~ Total_Bilirubin & Direct_Bilirubin
# ## 1 interaction var looks possible with protien * Albumin
# liver_dt %>%
#   mutate(obs = 1:nrow(.)) %>%
#   gather("var", "value", Total_Bilirubin:Albumin_and_Globulin_Ratio) %>%
#   mutate(value = log(value + 1)) %>%
#   spread(var, value) %>%
#   select(Alamine_Aminotransferase:Total_Protiens) 
#   GGally::ggpairs()
#   
#     ## remove target vars
  liver_dt %>%
    select(-Dataset, -target_Var) -> liver_dt_sup


  ## replace missing ratio values
  values <- imputePCA(liver_dt_sup, ncp = 6, scale = TRUE,quanti.sup=1 , quali.sup=2,
            coeff.ridge = 1, threshold = 1e-06, seed = 102)
# 
#   ## corr between fitted values and actuals ratio == .994
#   liver_dt %>%
#     mutate(fitted_Albumin_and_Globulin_Ratio = values$fittedX[,9]) %>%
#     select(Albumin_and_Globulin_Ratio, fitted_Albumin_and_Globulin_Ratio ) %>%
#     GGally::ggpairs()

  ## replace missing na values with fitted PCA values 
  liver_dt %>%
    mutate(fitted_Albumin_and_Globulin_Ratio = values$fittedX[,9]) %>%
    mutate(Albumin_and_Globulin_Ratio = if_else(is.na(Albumin_and_Globulin_Ratio), 
    fitted_Albumin_and_Globulin_Ratio, Albumin_and_Globulin_Ratio)) ->  liver_dt
  
  ## log transform explanatory value 
  liver_dt %>%
    mutate(obs = 1:nrow(.)) %>%
    gather("var", "values", Total_Bilirubin:Albumin_and_Globulin_Ratio) %>%
    mutate(values = log(.$values + 1)) %>%
    spread(var, values) -> liver_Log_dt
  
  
  
  save(liver_dt, file = "data/liver_dt.rds")

  
  ## fit mixed model 
  ## optiaml mdoel, age stariifed by gender makes sense & suoport by the analytics and info factor to pred liver disease
  ## fixed effects all inmprove the pred power of the model 
  ## inserting log in lienar eq s the same effects as prior trasnformations, adv is you an input original values 
  ## in the app 
  fitted_Mixed_mod <- glmer(target_Var ~ ns(Direct_Bilirubin) + 
                              ns(Alkaline_Phosphotase) + ns(Alamine_Aminotransferase) + 
                              ns(Aspartate_Aminotransferase) + ( ns(Age, 2)  | Gender), 
                            family = binomial(link = "logit"), data = liver_Log_dt,
                            control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0L)
  
  
  #save(fitted_Mixed_mod, file = "data/fitted_Mixed_mod.rds")
  
  summary(fitted_GLM_mod)
  BIC(fitted_Mixed_mod)
  ranef(fitted_Mixed_mod)
  
  
  ## add fitted pred to liver log dt
  liver_Log_dt %>%
    mutate(pred = add_predictions(.,fitted_Mixed_mod, type = "response")$pred) -> liver_Log_dt
  
  
  save(liver_Log_dt, file = "data/liver_Log_dt.rds")
  
  
  ## percentile array, .01-.99, by = .01
  percentiles <- quantile(liver_Log_dt$pred, probs = seq(.01, .99, by = .01))
  
  ## create percentile dataset
  percentiles_DT <- data.frame(Percentiles = attributes(percentiles)$names,
                               values = percentiles)
  
  
  save(percentiles_DT, file = "data/percentiles_DT.rds")
  

  ggeffects::ggpredict(fitted_Mixed_mod) # reviewmoeffects
  plot_model(fitted_Mixed_mod, type = "resid") # fixed effects plots
  plot_model(fitted_Mixed_mod, type = "eff", terms = "Alkaline_Phosphotase") # residuals
  plot_model(fitted_Mixed_mod, type = "re") # residuals
  
  
  
  ##=-- kfolds spliuts data into smaller and smaller groups whilst crossv_mc split data into 2 groups reps "x" times
  ## adding ns(to cumobs,3) and adding as am interaction term within the fixed vars reduced to mod pred power, the latter substantionally. 
  liver_dt %>%
    crossv_mc(20) -> liver_dt_Cv
  
  ##--single model cv testing
  liver_dt_Cv %>% 
    mutate(train = map(train, as_tibble)) %>%
    mutate(test = map(test, as_tibble)) %>%
    mutate(glm_Mod_bat = map(train,  ~   glmer(target_Var ~  ns(Direct_Bilirubin) + 
                                               ns(Alkaline_Phosphotase) + ns(Alamine_Aminotransferase) + 
                                               ns(Aspartate_Aminotransferase) + ( ns(Age,2) | Gender), 
                                               family = binomial(link = "logit"), data = .,
                                               control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0L))) %>%
    mutate(glm_Pred_bat = map2(test, glm_Mod_bat, add_predictions, type = "response")) %>%
    unnest(glm_Pred_bat) %>%
    select(target_Var, pred)  %>%
    mutate(mod_logLik = if_else(target_Var == 0L, -log(1 -pred ), -log(pred ))) %>%
    group_by(target_Var) %>%
    summarise(lower = quantile(mod_logLik,.25, na.rm = TRUE),
              med = median(mod_logLik, na.rm = TRUE),
              upper= quantile(mod_logLik,.75, na.rm = TRUE),
              Cnt = sum(n())) 
  
  
  ## test model effects 
  data <- liver_dt[1,] 
  sample <- liver_dt[2:5,]
  combined_Newdata_sample <- rbind(data, sample)
  
  ## need atleast 5 obs to generate se.fit, take the first obs resultsm viz an eeorbar , max / min (0,1) 
  predict(fitted_Mixed_mod, newdata = combined_Newdata_sample, type = "response", se.fit = TRUE) # add standard errors 
  

  