
library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")
library("tidyquant")
library("timetk")
library("sweep")
library("lubridate")
library("broom")

# -- Purpose: Building features to illusrate relationships between SA3 Renal Counts & Features 
# for instance trend over time or Population Growth

# -- Question is in more appropriate to examine differences or cumulative sums ?
# i.e. Patient YOY Diffs or Yearly Patient Count & feature (should you use the indep. vars - diffs)

dataset <- read_csv("ANZDATA SA3 Renal Diaylsis Patients with DELWP Pop.csv")

str(dataset)

# plot line chart of all SA3's - Patients no. vs no. Years 

str(dataset)

dataset %>% 
 mutate(SA3  = as.factor(SA3 )) %>%
 ggplot(aes(x = Pop, y = Renal_Patients, colour = SA3 )) +
 geom_line() + theme_classic() + theme(legend.position="none") + ggtitle("No. renal dialysis patients vs Pop at Each SA3 (2006-2017)")

# flatten file to each row rep an SA3 - tibble nested with SA3 var. 
by_SA3 <- dataset %>%
  mutate(SA3 = as.factor(SA3)) %>%
  group_by(SA3,SA4,Location) %>% 
  nest()
by_SA3$data[[1]]
 
# Build a function to pass through nested dataset using the gapminder toolkit
# Lineat Mod Function

SA3_model <- function(df) {
  lm(Renal_Patients ~ Year:Pop, data = df)
}

# map the mod through the nest dataset to keep related things together
by_SA3 <- by_SA3 %>% 
  mutate(model_trend = map(data, SA3_model))
by_SA3$data[1]


# add correlaions between Pop ~ Renal Patients vars
Correls <- function(df) {
  correlation = as.vector(cor(df[2],df[4]))
}

by_SA3 <- by_SA3 %>% 
  mutate(Cor = map(data, Correls))
by_SA3$Cor[1]

# unnest correlations statistics 
Correls_ds <- by_SA3 %>% 
  unnest(Cor, .drop = TRUE)
Correls_ds

# Visualse Cor between Years ~ Renal Patients  
Correls_ds %>% 
  ggplot(aes(x = SA3,y =  Cor, colour = Location )) + 
  geom_jitter(width = 0.5,size=4,alpha = 0.8) + ylab("Correlations") + xlab("SA3's") + ggtitle("Correlations between population ~ dialysis at each SA3") + theme_classic() + theme(axis.text.x=element_blank())
              
              # create object with strong linear cor and neg cor 
Correls_ds %>%
  filter(Cor > 0.90 | Cor < -0.5) %>%
  arrange(Cor) -> ar_correl

View(ar_correl)

# Use the broom package to collect mod quality info.  
by_SA3 %>% 
  mutate(glance = map(model_trend, broom::glance)) %>% 
  unnest(glance)

# to keep the info. related to SA3 need to nest it into sa3 dataset 
glance <- by_SA3 %>% 
  mutate(glance = map(model_trend, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

# visualse how well time examines the var in patients 
glance %>% 
  ggplot(aes(SA3_NAME_2016, r.squared)) + 
  geom_jitter(width = 0.5,size=2,alpha = 0.5,colours="red") + theme(axis.text.x=element_blank())

View(glance)

