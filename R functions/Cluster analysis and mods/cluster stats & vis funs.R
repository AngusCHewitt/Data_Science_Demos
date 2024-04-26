##------ Commonly used cluster funs -------##

CV_barplot_fun <- function(df) {
## fun to visualsise WSS
##-- analysis of coefficient of variation group by Var Names and clusetr lower the better (WSS)
df %>%
  gather("Var", "Value", -clustering) %>%
  group_by(clustering, Var) %>%
  summarise(CV = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE)) %>%
  mutate(CV = if_else(is.nan(CV),0,CV)) -> CV_table


##-- visulaise variables within in cluster (WSS)
CV_table %>%
  ggplot(aes(x = reorder(Var, CV), y = CV, fill = Var)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "") + theme_classic() + theme(legend.position = "none") + facet_wrap(~clustering) 
}



WSS_clust_Var_level_Fun <- function(df) {
  ##-- feed in df mixture moel vars with cluster numbers 

  ##-- calc WSS for each var within each cluster
  df %>%
    gather("Var", "Value", -clustering) %>%
    group_by(clustering, Var) %>%
    summarise(CV = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE)) %>%
    mutate(CV = if_else(is.nan(CV),0,CV)) %>%
    spread(clustering, CV) -> WSS_var_Cluster_level
  
  ##--  new CV medium plus range to determine how much within summer squares variation  within each cluster
  df %>%
    gather("var","val", -clustering) %>%
    group_by( clustering, var ) %>%
    summarise(cv = sd(val)/ mean(val)) %>%
    group_by(clustering) %>%
    summarise(q1 = quantile(cv,.25, na.rm = T),
              med = median(cv, na.rm = T),
              q2 = quantile(cv,.75, na.rm = T),
              range = q2 - q1) %>%
    select(clustering, med,range) -> WSS_cluster_Level
  
  ##-- return both obs as a lists
  list(WSS_var_Cluster_level, WSS_cluster_Level)
  
}



BSS_clust_Var_fun <- function(df) {
  ##-- cluster BSS using tukey pairwise fun
  ##-- feed in df with cluster var and cluster no called "clustering"
  df  %>%
    group_by(clustering) %>%
    summarise_all(.fun = mean) %>% ## calc the mean of each var for each cluster 
    gather("comp","val", -clustering) %>%
    group_by(comp) %>%
    nest() %>%
    mutate(AOV_diffs = map(data, ~ TukeyHSD(aov(.$val ~ as.factor(.$clustering))))) %>% ## calc diff betwee mean of each var 
    mutate(Sum_diffs = map(AOV_diffs, ~ (data.frame(pairs= row.names(.$`as.factor(.$clustering)`), abs(.$`as.factor(.$clustering)`))))) %>% # retrieve paired stats
    select(-AOV_diffs) %>%
    unnest(Sum_diffs) %>%
    spread(pairs, diff) -> BSS_pairs_tb
  BSS_pairs_tb
  
  ##-- add sd for each var fo analysis a standardise differeence between each pairing and var combo
  df %>%
    select(-clustering) %>%
    summarise_all(.fun = sd) %>%
    gather("comp","sd") -> clust_Var_sd
  
  ##--left join sd and calc standise var diff
  BSS_pairs_tb %>%
    left_join(clust_Var_sd, by = "comp")  %>%
    select(-data, -lwr, -upr, -p.adj) %>%
    gather("var","value",-comp, -sd) %>%
    mutate(value = value / sd) %>%
    spread(var,value)  -> BSS_pairs_tb_Var_Level
  
  ##-- summarise BSS at a cluster level (med and range diffs)
  BSS_pairs_tb_Var_Level %>%
    ungroup() %>%
    select(-sd, -comp) %>%
    gather("var", "value") %>%
    group_by(var) %>%
    summarise(q1 = quantile(value,.25, na.rm = T),
              med = median(value, na.rm = T),
              q2 = quantile(value,.75, na.rm = T),
              range = q2 - q1) -> BSS_pairs_tb_Clust_level
  BSS_pairs_tb_Clust_level
  
  ##return both obs as a list
  list(BSS_pairs_tb_Var_Level, BSS_pairs_tb_Clust_level)
}


clust_PCA_vis_Fun <- function(df, clusters) {
  ##--PCA vis of proposed cluster model
  ##-- feed in df with cluster var and cluster no called "clustering"
  require(factoextra) # clust fun
  
df %>%
    select(-clustering) -> numeric_data


object = list(data = numeric_data, cluster = clusters)

# visualise clusters
(P1 <- fviz_cluster(object, data = numeric_data,  geom = "text", main = "", ggtheme = theme_classic()))
}



param_Barplot_fun <- function(mixture_Mod, cluster_matrix) {
##-- barplot vi param 
##-- feed in mixture model , matrix with cluster vars

##- parmaters of mixture model  
param <- data.frame(parameters(mixture_Mod))

# Add the column with the type of crime
param <- param %>% 
  mutate(vars = colnames(cluster_matrix))


##-- Plot the clusters with their lambdas
param %>% 
  gather(cluster, lambdas, -vars) %>% 
  ggplot(aes(x = vars, y = lambdas, fill = vars)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") + coord_flip()
}

