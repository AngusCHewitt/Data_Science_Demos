##-- visualising lambda for poisson mixture model --##

params_lambdas <- data.frame(parameters(best_poisson_mm))

# Add the column with the type of crime
params_lambdas_crime <- params_lambdas %>% 
  mutate(crime = colnames(matrix_crimes))

# Plot the clusters with their lambdas
params_lambdas_crime %>% 
  gather(cluster, lambdas, -crime) %>% 
  ggplot(aes(x = crime, y = lambdas, fill = crime)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")