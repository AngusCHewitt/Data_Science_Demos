##-- read in excel files 
library(readxl)
list <- excel_sheets("~/Downloads/entire mlb lineups for fangrpahs.xlsx")
df <- rbind(lapply(list, function(x) read_excel("~/Downloads/entire mlb lineups for fangrpahs.xlsx", sheet = x)))

##-- loop through excel docs and grpa info you need. 
df[[1]] %>%
  select(Name, Role, playerId) %>%
  mutate(Team = list[1])