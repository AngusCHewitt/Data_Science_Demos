# functional programming emphasis on action (Hadley Wickham)

library("modelr")
library("tidyverse")
library("gapminder")

# wraps loops in a function
mean <- map_dbl(mtcars,mean)
median <- map_dbl(mtcars,median)

# remove objects from global env, seach path
rm(mean)
rm(median)

# store functions in list, apply functions each col elements
funs <- list(mean,median,sd)

# pass function through pipe, mtcars d.s. and output vectors doubles;
funs %>%
  map(~mtcars %>% map_dbl(.x))

View(gapminder)

# visual each country life exp
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# nest data of each year to create a flat file (by country)
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()
by_country

# view nest data for row 1
by_country$data[[1]]

# create linear mod using nested dataset var. 
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# pass nest column throu. linear function
models <- map(by_country$data, country_model)

# store mod as a in the nest dataset - related info to each var store together
by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

# add resid to nested dataset
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

# unnest residuals
resids <- unnest(by_country, resids)
resids

# visualise all resduals
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# factet by country
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)


# Use broom package to extract model stats - parameters 
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)




