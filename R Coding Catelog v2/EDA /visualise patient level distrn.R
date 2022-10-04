# visualise distrn within regions and with hospitals

library(tidyverse) # tidy data

# tidy the data to allow for easier visuals
join_procs %>%
   select(obs, campname, Campus_type, ECCS, counta)%>%
   gather("Var","N",4:5) %>%
   spread(key = Var, value = N) %>%
   mutate( obs_cnt = 1) -> tidy_procs

# visulise distrn of each campus - dig data - summary stats compare distrn

p <- ggplot(data = tidy_procs, aes(x = Campus_type, y = log(ECCS)))

# medium, min and max
p + stat_summary(fun.ymin = min,
    fun.ymax = max,
    fun.y = median)

# nest the data to obtain  
tidy_procs %>%
 group_by(campname) %>%
 nest() -> hosp_level

# median f() 
med_fun <- function(df) {
med_counts <- median(df$counta)
med_counts }


# range f() 
range_fun <- function(df) {
range_counts <- max(df$counta) -  min(df$counta)
range_counts }

# sum no. esp f() 
sum_fun <- function(df) {
sum_counts <- sum(df$obs_cnt)
sum_counts }


# sum no. proc f() 
sum_procs_fun <- function(df) {
sum_proc_counts <- sum(df$counta)
sum_proc_counts }


# hosp classifications f() 
hosp_class <- function(df) {
hosp_class <- df$Campus_type[1]
hosp_class }

# ECCS median f() 
ECCS_med_fun <- function(df) {
ECCS_med_counts <- median(df$ECCS)
ECCS_med_counts }


# ESS range f() 
ECCS_range_fun <- function(df) {
ECCS_range_counts <- max(df$ECCS) -  min(df$ECCS)
ECCS_range_counts }


# map big data descriptive variables

 hosp_level %>%
 mutate( med_procs = map(data, med_fun)) %>%
 mutate( range_procs = map(data, range_fun)) %>%
 mutate( sum_esp = map(data, sum_fun)) %>%
 mutate( sum_procs = map(data, sum_procs_fun)) %>%
 mutate( hosp_class = map(data, hosp_class)) %>%
 mutate( med_ECCS = map(data, ECCS_med_fun)) %>%
 mutate( range_ECCS = map(data,ECCS_range_fun )) %>%
 unnest(med_procs,range_procs,sum_esp, sum_procs,hosp_class, med_ECCS, range_ECCS) -> add_vars


# visualise range of procs splice by hosp classes and no. esp
p <- ggplot(data = add_vars)

p + geom_point(aes(x =  hosp_class,   y = range_ECCS, size = sum_esp, color =  hosp_class  ))  


