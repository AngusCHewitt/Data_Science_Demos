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
library(nnet) # pred mod

##-- need to detemine which vas can explain the which people will be likely to default and not
##-- obj it to find good desc and pred vars for likelihood of Delinquency, 
deliq_dt <- read_csv("data/teleco_dt.csv")
source("~/Desktop/R functions/Cluster analysis and mods/clara_Fun.R")


##----------------------------------- EDA -----------------------------------##

##-- remove dups mobile phone number by taking customer last tarns 
deliq_dt %>%
  mutate(pdate = as.Date(pdate)) %>%
  group_by(msisdn) %>%
  mutate(max_pDate = max(pdate)) %>%
  mutate(max_amnt_loans90 = max(amnt_loans90)) %>%
  mutate(max_cnts_loans90 = max(cnt_loans90)) %>%
  filter(pdate == max_pDate) %>%
  filter(max_amnt_loans90 == max_amnt_loans90) %>%
  mutate(cnt = n()) %>% # dup return conflicting target vars 
  filter(cnt < 2) -> deliq_dt
  

glimpse(deliq_dt)

##-- log tranformationa of cont vars so perform cor, assoc & PCA analysis, mods expected data ~ ND
deliq_dt %>%
  ungroup() %>%
  select(-cnt, -max_amnt_loans90, -max_cnts_loans90, -max_pDate) %>%
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
  select(-label:-pdate, -aon, -cnt_da_rech30, -last_rech_date_da, -fr_da_rech30, -fr_da_rech90, -fr_ma_rech30, -fr_ma_rech90) -> cont_vars


##-- cor vars dataset > 0.5 abs cor  
##-- correlations pairs between all questions
corrplot::corrplot(cor(cont_vars)) # cor with dim and QAs


##-- fit PCa model 
PCA_Model <- PCA(cont_vars, graph = FALSE, ncp = 10)

##-- save objs 
#save(PCA_Model, file = "data/PCA_Model.rds")

summary(PCA_Model)


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


##-- visual the distrn of 3 import vars with diff coords (PCA top 6 chart)
deliq_dt_logs %>%
  group_by(clustering) %>%
  summarise(prob_of_success = mean(label),
            total_clients = n(),
            percentage_total_clients = (n()/nrow(deliq_dt_logs))*100) -> prob_stats
  

##-- summarise mean,sd all var include in the, desc the 3 distrn clusters  
deliq_dt_logs %>%
  select(clustering,  medianamnt_ma_rech90, cnt_ma_rech90, amnt_loans90) %>%
  gather("var", "value", -clustering) %>%
  ggplot(aes(x = exp(value), fill = as.factor(clustering))) + geom_histogram(colour = "black") + facet_wrap(~var + clustering, scale = "free") +
  theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "grey50"))
  

##-- Q2 NNET of each cluster 

deliq_dt_logs %>%
  select(Dim.1:Dim.10, clustering) %>%
  mutate(clustering = as.factor(clustering)) -> deliq_dt_logs_nnet_dt

##-- set rand seed and set aside 30 percent of the pop
set.seed(104)
rand_samples <- sample_n(deliq_dt_logs_nnet_dt, size = nrow(deliq_dt_logs_nnet_dt)*0.9)

##-- fit nnet 
nnet_Fit <- nnet(rand_samples[,1:10], class.ind(rand_samples[,11]), size = 2, rang = 0.5,
            decay = 10e-4, maxit = 1000)


##-- perm file nnet pred acc
#save(nnet_Fit, file = "data/nnet_Fit.rds")
#save(rand_samples, file = "data/rand_samples.rds")


##-- convert rand smaple number into numeric vector
rand_sample_row_no <- as.numeric(row.names(rand_samples))

# test model acc
acc_tab <- table(Actuals = deliq_dt_logs_nnet_dt[-rand_sample_row_no,]$clustering, 
      Pred = max.col(predict(nnet_Fit, deliq_dt_logs_nnet_dt[-rand_sample_row_no,])))

##-- perm file nnet pred acc
#save(acc_tab, file = "data/acc_tab.rds")


load("data/acc_tab.rds")

tibble(acc_tab)


rm(list = ls())  # caution: delete all objects in .GlobalEnv
gc()  # free system memory





