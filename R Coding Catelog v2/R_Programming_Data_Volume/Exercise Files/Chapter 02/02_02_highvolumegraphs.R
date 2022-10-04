# Description: Plots for use with high-volume data

hist(UCS_Satellite_Database$Expected.Lifetime..yrs..,
     main = "Expected Lifetime")

boxplot(UCS_Satellite_Database$Expected.Lifetime..yrs.. ~ UCS_Satellite_Database$Country.Org.of.UN.Registry,
        main = "Expected Lifetime by Country",
        ylab = "years",
        las = 2)

dotchart(table(UCS_Satellite_Database$Country.Org.of.UN.Registry), cex=.5)

smoothScatter(x = UCS_Satellite_Database$Expected.Lifetime..yrs..,
              y = UCS_Satellite_Database$Launch.Mass..kg..,
              main = "Expected Lifetime",
              xlab = "years",
              ylab = "launch mass")

# different function, same plot but with a smoothed curve
scatter.smooth(x = UCS_Satellite_Database$Expected.Lifetime..yrs..,
              y = UCS_Satellite_Database$Launch.Mass..kg..,
              main = "Expected Lifetime",
              xlab = "years",
              ylab = "launch mass")

