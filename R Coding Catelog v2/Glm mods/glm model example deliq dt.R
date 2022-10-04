##-- EDA  delinquency dataset
library(tidyverse)
library(vcd)
library(effects) # visualise models effects
library(ggeffects) # plot mixd effects
library(splines) # natrtual spline NL relationship
library(rattle) # cate binning funs
library(sjPlot) #mixed effects plots
#library(caret)
library(GGally) # visual pairs
library(FactoMineR) # PCA mods
library(factoextra) # PCa extars
library(corrplot) # auto corr plot
library(lubridate)
library(rattle) # data science
library(cluster) # cluster funs
library(broom) # idy model sum dt
library(modelr) # addm pred to df 
library(statmod) # glm quanile residauls 

##-- need to detemine which vas can explain the which people will be likely to default and not
##-- obj it to find good desc and pred vars for likelihood of Delinquency, 
deliq_dt <- read_csv("data/teleco_dt.csv")
source("~/Desktop/R functions/Cluster analysis and mods/clara_Fun.R")


##----------------------------------- EDA -----------------------------------##



##-- log tranformationa of cont vars so perform cor, assoc & PCA analysis, mods expected data ~ ND
deliq_dt %>%
  mutate(obs = 1:nrow(deliq_dt)) %>%
  mutate(label = as.integer(label)) %>%
  gather("var", "value", daily_decr30:payback90) %>%
  mutate(value = if_else(value < 0,  -log(abs(value)+1),
                         log(value+1))) %>%
  spread(var,value) -> deliq_dt_logs


##-- visualise distrn before after log trans 
deliq_dt %>%
  select(msisdn, aon, daily_decr30, rental90, last_rech_amt_ma, cnt_ma_rech30) %>%
  gather("var","value", -msisdn, -aon) %>%
  ggplot(aes(y = value, fill = var)) + geom_boxplot() + facet_wrap(~var, scales = "free") + theme_classic() + theme(legend.position = "none")


##-- after
deliq_dt_logs %>%
  select(msisdn, aon, daily_decr30, rental90, last_rech_amt_ma, cnt_ma_rech30) %>%
  gather("var","value", -msisdn, -aon) %>%
  ggplot(aes(y = value, fill = var)) + geom_boxplot() + facet_wrap(~var, scales = "free") + theme_classic() + theme(legend.position = "none")


##-- selec cont vars
deliq_dt_logs %>%
  select(-label:-pdate, -aon, -obs, -cnt_da_rech30, -last_rech_date_da, -fr_da_rech30, -fr_da_rech90, -fr_ma_rech30, -fr_ma_rech90) -> cont_vars


##-- cor vars dataset > 0.5 abs cor  
##-- correlations pairs between all questions
corrplot::corrplot(cor(cont_vars)) # cor with dim and QAs


##-- fit PCa model 
PCA_Model <- PCA(cont_vars, graph = FALSE, ncp = 10)

##-- dimesions desc
#dimdesc(PCA_Model, axes = 1:3)
summary(PCA_Model)


##-- cor plaots of var cor with each and var conts within each dim
corrplot::corrplot(cor(PCA_Model$ind$coord)) # cor with dim and QAs


corrplot::corrplot(PCA_Model$var$contrib, is.corr = F) # contribution
corrplot::corrplot(PCA_Model$var$cos2, is.corr = F) # qualtiy measure 


##-- contribution of each var to each dim
##--- take an eigenvalue >= 1, the loading (cors) reflect the same results as the contribution figures, that is the most imoport var has highest abs values 
fviz_pca_var(PCA_Model, col.var = "contrib" , axes = c(1, 2)) ## pca chart with cont as color density 
fviz_contrib(PCA_Model, "var", axes = 1:2) # ref line 5.83% == uniform distrn (if all question provide equal amount of info)
fviz_cos2(PCA_Model, "var", top = 17, axes = 1:2) ## quality of the vars in each dims > .6 soncdered good resp
fviz_pca_biplot(PCA_Model, "var", result = "contrib", axes = 1:2, top = 4) # a more zoned in version of PCA plot 
fviz_eig(PCA_Model, "eigenvalue")##-- eigenvalues for each dims
plot(PCA_Model, choix = "var", shadow = TRUE, select = "contrib 10") # only include top n vars lables in term cont
get_eigenvalue(PCA_Model) # eign values in dataframe


##-------- QA 1 - Cluster client into meaningful risk profiles ------------##

##-- clara cluster model labell PCA co ord obs
##-- go with the first 10 dims contains 90% all the varation
PCA_cluster_dt <- data.frame(PCA_Model$ind$coord)


##-- label cluserts
cluster_labels <-  clara_Loop_fun(PCA_cluster_dt, K = 3)

##-- add ccluster labels to original dataset
deliq_dt_logs %>%
  mutate(clustering = cluster_labels$clustering) -> deliq_dt_logs

##-- cbind PCA data to the logs dataset feed into the glm model
deliq_dt_logs <- cbind(PCA_cluster_dt, deliq_dt_logs)


##-- to nest nest as a cluster level and get conf interval for there, fir similar mod for preds, goal this one desc stats 
glm_fit_fun <- function(df) { glm(label ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.6 + Dim.7 + Dim.8 + Dim.9 + Dim.10, data = df, family = "binomial")}


##-- nest glm  model at cluster level
deliq_dt_logs %>%
  group_by(clustering) %>%
  nest() %>%
  mutate(glm_Mod = map(data, ~glm_fit_fun(.))) %>%
  mutate(augment = map(glm_Mod, ~augment(., type.predict = "response"))) -> deliq_dt_logs_mods


##-- nested model stats 
summary(deliq_dt_logs_mods$glm_Mod[[1]])
plot_model(deliq_dt_logs_mods$glm_Mod[[3]], type = "pred") # mod pred intervals 
glance(deliq_dt_logs_mods$glm_Mod[[1]])
tidy(deliq_dt_logs_mods$glm_Mod[[1]])
boxplot((augment(deliq_dt_logs_mods$glm_Mod[[3]], type.predict = "response")$.fitted))

##-- review glm model residuals 
qres <- qresid(deliq_dt_logs_mods$glm_Mod[[3]]); qqnorm(qres, las=1); abline(0, 1)


##-- model passes goodness of fit ttest (over dispersion)
c(Df = df.residual( deliq_dt_logs_mods$glm_Mod[[3]] ),
  Resid.Dev = deviance( deliq_dt_logs_mods$glm_Mod[[3]] ),
  Pearson.X2 = sum( resid(deliq_dt_logs_mods$glm_Mod[[3]], type="pearson")^2 ))


##-- visulise each clusters distrn prob "S"   
aov_test <- aov(label ~ factor(clustering), data = deliq_dt_logs)
TukeyHSD(aov_test)
plot(TukeyHSD(aov_test))


##-- to nest nest as a cluster level and get conf interval for there, fir similar mod for preds, goal this one desc stats 
glm_fit_fun <- function(df) { glm(label ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.6 + Dim.7 + Dim.8 + Dim.9 + Dim.10, data = df, family = "binomial")}


##-- nest glm  model at cluster level
deliq_dt_logs %>%
  group_by(clustering) %>%
  nest() %>%
  mutate(glm_Mod = map(data, ~glm_fit_fun(.))) %>%
  mutate(augment = map(glm_Mod, ~augment(., type.predict = "response"))) -> deliq_dt_logs_mods


##-- visuliase the distn of the fitte models and top PCA vats for each cluster
deliq_dt_logs_mods %>%
  select(-glm_Mod) %>%
  unnest() -> deliq_dt_logs_mods

##-- upper and lowr qant funs 
upper_quantile_fun <- function(df) {quantile(df, 0.95, na.rm = TRUE)}
lower_quantile_fun <- function(df) {quantile(df, 0.05, na.rm = TRUE)}

##-- sum all vars in selcton with a variey of funs 
deliq_dt_logs_mods %>%
  group_by(clustering) %>%
  select(clustering, .fitted) %>%
  summarise_all(.,list(lower = lower_quantile_fun, median = median, upper = upper_quantile_fun)) -> distrn_tbs



