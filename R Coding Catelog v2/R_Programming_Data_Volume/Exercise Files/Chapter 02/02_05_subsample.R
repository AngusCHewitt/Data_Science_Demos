# Description: subsampling data

smoothScatter(x = UCS_Satellite_Database$Expected.Lifetime..yrs..,
              y = UCS_Satellite_Database$Launch.Mass..kg..,
              main = "Expected Lifetime",
              xlab = "years",
              ylab = "launch mass")

# sample() ------------------------------------------------------------------

useTheseRows <- sample(1:nrow(UCS_Satellite_Database), 100)
smallUCS <- UCS_Satellite_Database[useTheseRows , ]

smoothScatter(
  x = smallUCS$Expected.Lifetime..yrs..,
  y = smallUCS$Launch.Mass..kg..,
  main = paste(nrow(smallUCS), "Points: Expected Lifetime"),
  xlab = "years",
  ylab = "launch mass"
)

# approx() ------------------------------------------------------------------
approxPoints <- approx(UCS_Satellite_Database$Expected.Lifetime..yrs..,
                       UCS_Satellite_Database$Launch.Mass..kg..)

# original scatterplot + approxPoints as line
smoothScatter(
  x = UCS_Satellite_Database$Expected.Lifetime..yrs..,
  y = UCS_Satellite_Database$Launch.Mass..kg..,
  main = paste(length(approxPoints[["x"]]), "Points: Expected Lifetime"),
  xlab = "years",
  ylab = "launch mass"
)
lines(approxPoints)

# spline() ------------------------------------------------------------------
splinePoints <- spline(UCS_Satellite_Database$Expected.Lifetime..yrs..,
              UCS_Satellite_Database$Launch.Mass..kg..)

# original scatterplot combined with line plot for spline
smoothScatter(
  x = UCS_Satellite_Database$Expected.Lifetime..yrs..,
  y = as.numeric(UCS_Satellite_Database$Launch.Mass..kg..),
  main = paste(length(splinePoints[["x"]]), "Points: Expected Lifetime"),
  xlab = "years",
  ylab = "launch mass"
)
lines(splinePoints)




