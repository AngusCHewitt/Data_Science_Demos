library("modelr")
library("tidyverse")
library("gapminder")

data("cars", package = "caret")
glimpse(cars)

##- Cross vals act like pointers to the orginal dataset, fun 
cars %>%
  select(-Doors, -Chevy, -sedan) %>%
  crossv_kfold(10) -> cars_cv

cars_cv %>% 
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ lm(Price ~ Mileage + Cylinder + Cruise + Sound + 
                                   Leather + Buick + Cadillac + Pontiac + 
                                   Saab + Saturn + convertible + coupe + 
                                   hatchback + wagon, 
                                 data = .))) %>%
  mutate(rmse = map2_dbl(model, test, rmse)) %>%
  select(.id, rmse)



cars %>%
  select(-Doors, -Chevy, -sedan) %>%
  crossv_kfold(10) -> cars_cv_2

cars_cv_2 %>% 
  mutate(train = map(train, as_tibble)) %>%
  group_by(Cylinder) %>%
  nest()
  mutate(model = map(train, ~ lm(Price ~ Mileage  + Cruise + Sound + 
                                   Leather + Buick + Cadillac + Pontiac + 
                                   Saab + Saturn + convertible + coupe + 
                                   hatchback + wagon, 
                                 data = .))) %>%
  mutate(rmse = map2_dbl(model, test, rmse)) %>%
  select(.id, rmse)


# The following code replicates the analysis in the chapter but replaces the 
## function `country_model()` with a regression that includes the year squared.
lifeExp ~ poly(year, 2)

# Fit many lienar models
country_model <- function(df) {
  lm(lifeExp ~ poly(year - median(year), 2), data = df)
}


Mod_Quality <- function(df) {
  Mod_AIC <- AIC(logLik(df))
}


summary(lm(lifeExp ~ poly(year, 2), data = gapminder))



##-- mod life exp across years by country and continent
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, country_model))  %>%
  mutate(Log_AIC = map(model, Mod_Quality)) -> by_country


##-- add residuals to coutry dt  
by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country


##-- visual resid across years
unnest(by_country, resids) %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)
  

##-- visual r^2 for each continent 
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)
  


library("ggbeeswarm")
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()
 

 ##-- visualse outlier countries
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, ~lm(lifeExp ~ year, .))) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  unnest(data) %>%
  filter(r.squared < 0.25) %>%
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(color = country))
  



range(mtcars$mpg)
fivenum(mtcars$mpg)
boxplot.stats(mtcars$mpg)
  

 
mtcars %>%
  group_by(cyl) %>%
  summarise(q = list(quantile(mpg))) %>%
  unnest()
  

quantile(mtcars$mpg)
  

 
mtcars %>%
  group_by(cyl) %>%
  summarise_each(funs(list))
  
mtcars %>%
  group_by(cyl) %>%
  summarise_each(funs(list))
  

