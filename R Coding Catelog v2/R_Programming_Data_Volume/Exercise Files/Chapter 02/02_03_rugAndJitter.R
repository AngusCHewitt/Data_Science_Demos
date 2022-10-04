# Description: demonstrate rug and jitter

# rug ---------------------------------------------------------------------

# rug indicates where values actually sit
hist(UCS_Satellite_Database$Expected.Lifetime..yrs..,
     main = "Expected Lifetime with rug",
     xlab = "Years")
rug(UCS_Satellite_Database$Expected.Lifetime..yrs..,
    col = "red")


# jitter ------------------------------------------------------------------

# jitter provides a way to indicate multiple values on the same point
head(UCS_Satellite_Database$Expected.Lifetime..yrs..)

head(jitter(UCS_Satellite_Database$Expected.Lifetime..yrs..))

hist(UCS_Satellite_Database$Expected.Lifetime..yrs..,
     main = "Expected Lifetime with jitter",
     xlab = "Years")
rug(jitter(UCS_Satellite_Database$Expected.Lifetime..yrs..),
    col = "red")

