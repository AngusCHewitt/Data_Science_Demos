
psycho_dt <- read.csv("app/data//Psychometric_tb.csv")



## cor matricxs (view all vars and there possible x ~ y relationships )

psycho_dt[,-1] %>%
  gather("var", "value") %>%
  group_by(var) %>%
  nest() %>%
  mutate(Diffs = map(data, ~diff(.$value))) %>%
  select(-data) %>%
  unnest() %>%
  mutate(obs = 1:18) %>%
  spread(var, Diffs) -> psycho_dt_diffs



# correlogram with hclust reordering
corrplot::corrplot(cor(psycho_dt_diffs[,-1]), type="upper", order="hclust") # viz corplots

GGally::ggpairs(psycho_dt_diffs[,-1]) # viz col matrix



cols = c("#4c86ad", "#f5dfb3")
psycho_dt_diffs[,-1] %>%
  GGally::ggpairs(
    lower = list(
      continuous = GGally::wrap("points", col = cols[1],alpha=0.6),
      combo = GGally::wrap("box", fill = "white", col ="black")
    ),
    upper = list(
      continuous = GGally::wrap("cor", col = cols[1]),
      combo = GGally::wrap("facetdensity", col = "black")
    ),
    diag = list(
      continuous = GGally::wrap("barDiag", fill = cols[2], col ="black", bins = 18),
      discrete = GGally::wrap("barDiag", fill = cols[2], col ="black"))
  )