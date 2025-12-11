# Mathematical Constants
PI <- 3.14159265358979
PI2 <- 2 * PI

# Radians to degrees
RAD2DEG <- 180 / PI

# Degrees to radians
DEG2RAD <- PI / 180

# Radians to hours
RAD2HR <- 12 / PI

# Arcseconds to radians
ARCSEC2RAD <- 4.848136811095359935899141E-6

# Radians to arcseconds
RAD2ARCSEC <- 206264.79821012

# Seconds per Day
SECDAY <- 86400

# Hours to radians
HR2RAD <- PI / 12

# Radians to hours
RAD2HR <- 12 / PI

# Astronomical Constants

# Astronomical Unit in Kilometers
KM2AU <- 149597870.7

# Astronomical Unit in Meters
M2AU <- 1.495978707E11

# Speed of Light in KM per sec
CLIGHT <- 299792.458

# Speed of Light in AU / day
CAUD <- 173.144633

# Gaussian Gravitational Constant
GAUSSK <- 0.01720209895

# EMRAT
EMRAT <- 81.3005690741906

# MUC
MUC <- 2 * GAUSSK * GAUSSK / CAUD * CAUD

# Equatorial Radius of the Earth in meters
EARTHRADM <- 6378140.0

# Equatorial Radius of the Earth in kilometers
EARTHRADKM <- EARTHRADM / 1000

# Equatorial Radius of the Earth in AU
EARTHRADAU <- EARTHRADM / 1000 / KM2AU

# Flattening of the Earth's reference ellipsoid (IAU 1976)
FLAT <- 1 / 298.257

# Julian day for J2000
EPOCHJ2000 <- 2451545.0

# Days per julian century
DAYSJULCENT <- 36525

# Rotational angular velocity of the Earth in radians/second
ROTANGVELEARTH <- 7.29211511467e-5

MONTHNAMES <- c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

WEEKDAYNAMES <- c(
  "Sunday",
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday"
)

GREGORIANREFORMJDN <- 2299161

# DE441 Constants
DE441STARTEPOCH <- -3100015.5 # Start Epoch
DE441ENDEPOCH <- 8000016.5 # End Epoch
DE441DAYSPERBLOCK <- 32 # Days per block
DE441NUMSUBINTMERCURY <- 4 # Number of subintervals for Mercury
DE441NUMCOEFFMERCURY <- 14 # Number of coefficients for Mercury
DE441NUMSUBINTVENUS <- 2 # Number of subintervals for Venus
DE441NUMCOEFFVENUS <- 10 # Number of coefficients for Venus
DE441NUMSUBINTEMB <- 2 # Number of subintervals for EMB
DE441NUMCOEFFEMB <- 13 # Number of coefficients for EMB
DE441NUMSUBINTMARS <- 1 # Number of subintervals for Mars
DE441NUMCOEFFMARS <- 11 # Number of coefficients for Mars
DE441NUMSUBINTJUPITER <- 1 # Number of subintervals for Jupiter
DE441NUMCOEFFJUPITER <- 8 # Number of coefficients for Jupiter
DE441NUMSUBINTSATURN <- 1 # Number of subintervals for Saturn
DE441NUMCOEFFSATURN <- 7 # Number of coefficients for Saturn
DE441NUMSUBINTURANUS <- 1 # Number of subintervals for Uranus
DE441NUMCOEFFURANUS <- 6 # Number of coefficients for Uranis
DE441NUMSUBINTNEPTUNE <- 1 # Number of subintervals for Neptune
DE441NUMCOEFFNEPTUNE <- 6 # Number of coefficients for Neptune
DE441NUMSUBINTPLUTO <- 1 # Number of subintervals for Pluto
DE441NUMCOEFFPLUTO <- 6 # Number of coefficients for Pluto
DE441NUMSUBINTMOON <- 8 # Number of subintervals for the Moon
DE441NUMCOEFFMOON <- 13 # Number of coefficients for the Moon
DE441NUMSUBINTSUN <- 2 # Number of subintervals for the Sun
DE441NUMCOEFFSUN <- 11 # Number of coefficients for the Sun
DE441NUMSUBINTNUTATION <- 4 # Number of subintervals for the Nutations
DE441NUMCOEFFNUTATION <- 10 # Number of coefficients for the Nutations
DE441NUMSUBLUNARMANTLELIB <- 4 # Number of subintervals for the Lunar Mantle Librations
DE441NUMCOEFLUNARMANTLELIB <- 10 # Number of coefficients for the Lunar Mantle Librations

# DE440 Constants
DE440STARTEPOCH <- 2287184.5 # Start Epoch
DE440ENDEPOCH <- 2688976.5 # End Epoch
DE440DAYSPERBLOCK <- 32 # Days per block
DE440NUMSUBINTMERCURY <- 4 # Number of subintervals for Mercury
DE440NUMCOEFFMERCURY <- 14 # Number of coefficients for Mercury
DE440NUMSUBINTVENUS <- 2 # Number of subintervals for Venus
DE440NUMCOEFFVENUS <- 10 # Number of coefficients for Venus
DE440NUMSUBINTEMB <- 2 # Number of subintervals for EMB
DE440NUMCOEFFEMB <- 13 # Number of coefficients for EMB
DE440NUMSUBINTMARS <- 1 # Number of subintervals for Mars
DE440NUMCOEFFMARS <- 11 # Number of coefficients for Mars
DE440NUMSUBINTJUPITER <- 1 # Number of subintervals for Jupiter
DE440NUMCOEFFJUPITER <- 8 # Number of coefficients for Jupiter
DE440NUMSUBINTSATURN <- 1 # Number of subintervals for Saturn
DE440NUMCOEFFSATURN <- 7 # Number of coefficients for Saturn
DE440NUMSUBINTURANUS <- 1 # Number of subintervals for Uranus
DE440NUMCOEFFURANUS <- 6 # Number of coefficients for Uranis
DE440NUMSUBINTNEPTUNE <- 1 # Number of subintervals for Neptune
DE440NUMCOEFFNEPTUNE <- 6 # Number of coefficients for Neptune
DE440NUMSUBINTPLUTO <- 1 # Number of subintervals for Pluto
DE440NUMCOEFFPLUTO <- 6 # Number of coefficients for Pluto
DE440NUMSUBINTMOON <- 8 # Number of subintervals for the Moon
DE440NUMCOEFFMOON <- 13 # Number of coefficients for the Moon
DE440NUMSUBINTSUN <- 2 # Number of subintervals for the Sun
DE440NUMCOEFFSUN <- 11 # Number of coefficients for the Sun
DE440NUMSUBINTNUTATION <- 4 # Number of subintervals for the Nutations
DE440NUMCOEFFNUTATION <- 10 # Number of coefficients for the Nutations
DE440NUMSUBLUNARMANTLELIB <- 4 # Number of subintervals for the Lunar Mantle Librations
DE440NUMCOEFLUNARMANTLELIB <- 10 # Number of coefficients for the Lunar Mantle Librations
