# Description: Trellis graphs

# clean up data
ucsWithNA <-
  UCS_Satellite_Database[, c(
    "Launch.Mass..kg..",
    "Expected.Lifetime..yrs..",
    "Country.Org.of.UN.Registry",
    "Power..watts."
  )]
ucsWONR <- ucsWithNA[!startsWith(ucsWithNA$Country.Org.of.UN.Registry, c("NR")),]
ucsNoNA <- ucsWONR[complete.cases(ucsWONR),]


library(lattice)

xyplot(
  Launch.Mass..kg.. ~ Expected.Lifetime..yrs.. 
  | Country.Org.of.UN.Registry,
  data = ucsNoNA,
  main = "Launch Mass vs Lifetime vs Country",
  xlab = "years",
  ylab = "launch mass"
)

