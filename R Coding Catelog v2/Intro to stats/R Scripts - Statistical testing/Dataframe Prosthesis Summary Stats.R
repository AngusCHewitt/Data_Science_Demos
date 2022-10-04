## read prosthesis file from default dictorary
pros <- read.csv("Prosthesis averages - Excludes ex gratia payments.csv")

## create data frame with summary stats for prosthesis payment categories 
pros_central_location <- plyr::ddply(pros, "Factor", plyr::summarise, mean = mean(Variable), sd = sd(Variable),median = median(Variable))


ggplot(pros, aes(x = Factor, y = Variable)) +
   geom_point(alpha=0.5)+
   geom_point(data = pros_central_location, aes(y = median),
              colour = 'red', size = 3)