
# Linear Regression Model -------------------------------------------------

# linear regression models look for relationships between variables and outcome
MyResponse <- UCS_Satellite_Database$Expected.Lifetime..yrs..
MyTerms <- UCS_Satellite_Database$Launch.Mass..kg..
  
launchMasstoLifeLM <- lm(MyResponse ~ MyTerms)
plot(launchMasstoLifeLM)

# k-means clustering --------------------------------------------------------------

ucsWithNA <- UCS_Satellite_Database[ , c("Launch.Mass..kg..","Expected.Lifetime..yrs..")]
ucsNoNA <- ucsWithNA[ complete.cases(ucsWithNA), ]

ucs_cluster <- kmeans(ucsNoNA, centers = 5)

# install.packages("cluster")
library(cluster)

clusplot(ucsNoNA, ucs_cluster$cluster,
         main = "Lifetime to Mass Clusters",
         xlab = "Launch Mass",
         ylab = "Life Expectancy")


