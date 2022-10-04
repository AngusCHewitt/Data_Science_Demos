# fit with flexmix

colnames(HO_cluster_num_vars)

Play_stats_cluster_vars %>%
  select(Range_Goals, Range_Marks_Out50, Range_MarksI50, Range_CL,
         Range_Contested.Possessions, Range_Uncontest_Possessions,
         Range_One_Percenters, Range_Rebounds) -> fwd_clust

# data need to be coerced into a matrix
dt_matrix <- as.matrix(fwd_clust)

# run pos mixture model - 1-15 with 5 reps of NM algorthrym
set.seed(281)
mvnorm_mix_model <- stepFlexmix(dt_matrix ~ 1, 
                                 k = 1:10, 
                                 nrep = 5, 
                                 model = FLXMCmvnorm(diagonal = FALSE),
                                 control = list(minprior = 0.0001))

# find the best fitted model according to the lowest BIC value
best_fit <- getModel(mvnorm_mix_model, which = "AIC")

# plot poisson model components - lambdas 
plot(best_fit)

# review the model fit to the data
Play_stats_cluster_vars %>%
  mutate(cluster = factor(clusters(best_fit))) %>%
  filter(Current_player == "1") %>%
  select(cluster, Surname,First.name, Goals, Marks) %>%
  arrange(cluster) %>% data.frame() 

