
##-- select mod var and lag vars
lag_dt_fun <- function(df) {
  
  df %>%  
    gather("var", "value", Dim.1:Dim.3) %>%
    group_by(var) %>%  
    mutate(value = lag(value)) %>%
    spread(var, value) -> df 
}



