# map many mods

# can use the ~ instaed of the 'function()"

# example of short hand anonymous. functions
map(.x= nested$data, .f = ~mean(.x$population) # list with d.f.
    
# fit linear mod
  gap_models <- gap_nested %>%
    mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x)))
)