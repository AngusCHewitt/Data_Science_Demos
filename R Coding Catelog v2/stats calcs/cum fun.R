library(tidyverse)

g <- data.frame(Name=rep(c("A","B","C"),3),
                GPA=c(5,6,7,5,6,6,7,6,3))

g %>%
  group_by(Name) %>%
  mutate(cumu = lag(cummean(GPA), n = 0))


Gavg <- sqldf("select *, avg(GPA) as avg_v from g group by Name")

cumgmean(g$GPA)

g %>%
  group_by(Name) %>%
  summarise(cumquant(GPA,.90))