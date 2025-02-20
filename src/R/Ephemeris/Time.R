
# Calculate delta-t in seconds to get an estimate of the difference between
# universal and dynamical time for any year in the past or future
DeltaT <- function(year, month)
{
  delta_t <- 0.0
  
  y <- year + (month - 0.5) / 12
  
  if (year < -500) {
    u <- (year - 1820) / 100
    delta_t <- -20 + (32 * u * u)
  } else if (year >= -500 & year < 500) {
    u <- y/100
    delta_t <- 10583.6 + u*(-1014.41 + u*(33.78311 + u*(-5.952053 + 
                                                          u*(-0.1798452 + u*(0.022174192 + u*(0.0090316521))))))
  } else if (year >= 500 & year < 1600) {
    u = (y - 1000)/100
    delta_t <- 1574.2 + u*(-556.01 + u*(71.23472 + u*(0.319781 + 
                                                        u*(-0.8503463 + u*(-0.005050998 + u*(0.0083572073))))))
  } else if (year >= 1600 & year < 1700) {
    t <- y - 1600
    delta_t <- 120 + t*(-0.9808 + t*(-0.01532 + t*(1/7129)))
  } else if (year >= 1700 & year < 1800) {
    t <- y - 1700
    delta_t <- 8.83 + t*(0.1603 + t*(-0.0059285 + t*(0.00013336 +
                                                       t*(-1/1174000))))
  } else if (year >= 1800 & year < 1860) {
    t <- y - 1800
    delta_t <- 13.72 + t*(-0.332447 + t*(0.0068612 + t*(0.00411116 +
                                                          t*(-0.00037436 + t*(0.0000121272 + t*(-0.0000001699 +
                                                                                                  t*(0.000000000875)))))))
  } else if (year >= 1860 & year < 1900) {
    t <- y - 1860
    delta_t <- 7.62 + t*(0.5737 + t*(-0.251754 + t*(0.01680668 +
                                                      t*(-0.0004473624 + t*(1/233174)))))
  } else if (year >= 1900 & year < 1920) {
    t <- y - 1900
    delta_t <- -2.79 + t*(1.494119 + t*(-0.0598939 + t*(0.0061966 +
                                                          t*(-0.000197))))
  } else if (year >= 1920 & year < 1941) {
    t <- y - 1920
    delta_t <- 21.20 + t*(0.84493 + t*(-0.076100 + t*(0.0020936)))
  } else if (year >= 1941 & year < 1961) {
    t <- y - 1950
    delta_t <- 29.07 + t*(0.407 + t*(-2/233)) + t*(1/2547)
  } else if (year >= 1961 & year < 1986) {
    t <- y - 1975
    delta_t <- 45.45 + t*(1.067 + t*(-1/260)) + t*(-1/718)
  } else if (year >= 1986 & year < 2005) {
    t <- y - 2000
    delta_t <- 63.86 + t*(0.3345 + t*(-0.060374 + t*(0.0017275 +
                                                       t*(0.000651814 + t*(0.00002373599)))))
  } else if (year >= 2005 & year < 2050) {
    t <- y - 2000
    delta_t <- 62.92 + t*(0.32217 + t*(0.005589))
  } else if (year >= 2050 & year < 2150) {
    t <- y - 2000
    delta_t <- -20 + 32 * ((y - 1829)/100) * ((y - 1829)/100) - 
      0.5628 * (2150 - y)
  } else {
    u <- (year - 1820) / 100
    delta_t <- -20 + (32 * u * u)
  }
  
  return (delta_t)
}

# Calculate the mean and apparent sidereal time at Greenwich in radians
# jd is the julian day number in universal time for the time of observation
GreenwichSiderealTime <- function(jd_ut, deltaT)
{
  T <- (jd_ut - EPOCHJ2000) / DAYSJULCENT
  
  # Calculate mean sidereal time in degrees
  gmst <- 280.46061837 + 360.98564736629 * (jd_ut - EPOCHJ2000) +
    (0.000387933 - (1/38710000) * T) * T * T
  
  # Convert result to the range 0 - 360 degrees
  gmst <- amodulo(gmst, 360)
  
  # Convert degrees to radians
  gmst <- gmst * DEG2RAD
  
  # Calculate dynamical time
  jd_td <- jd_ut + deltaT/SEC2DAY
  
  # Get the nutation angles in longitude and obliquity
  nut_angles <- nutationAngles(jd_td)
  
  # Get the mean and true obliquity of the ecliptic
  ob <- obliquity(jd_td, nut_angles)
  
  # Calculate apparent sidereal time
  gast <- gmst + nut_angles[[1]][1] * cos(ob[2])
  
  z <- c(gmst, gast)

  return (z)
}

# Convert sidereal time at Greenwich to local sidereal time at the observer
# location
# gst - sidereal time at Greenwich in radians
# obsLong - observer longitude in decimal degrees, +E, -W
# lst - local sidereal time in radians
GreenwichSiderealTimeToLocalSiderealTtime <- function(gst, obsLong)
{
  tz_adj <- obsLong / 15
  lst <- gst + tz_adj*HR2RAD
  
  if (lst < 0) {
    lst <- lst + PI2
  } else if (lst > PI2) {
    lst <- lst - PI2
  }
  
  return(lst)
}

# Convert local civil time to universal time with adjustments for time
# zone and daylight savings time
# decimal hour indicating the hour on the day of interest
# tz - time zone correction, negative if time zone is west of Greenwich,
# positive if east of Greenwich
# dst - daylight savings time, TRUE if daylight savings time is in effect
LocalCivilTimeToUniversalTime <- function(hour, dst, tz)
{
  hr <- hour
  
  # Adjust for daylight savings time
  if (dst == TRUE) {
    hr <- hr - 1
  }
  
  # Adjust for time zone
  hr <- hr - tz
  
  # Check boundary conditions
  if (hr > 24) {
    hr <- hr - 24
    print("The time is for the next day")
  } else if (hr < 0) {
    hr <- hr + 24
    print("The time is for the previous day")
  }
  
  return (hr)
}

# Convert universal time to local civil time with adjustments for time
# zone and daylight savings time
# decimal hour indicating the hour on the day of interest
# tz - time zone correction, negative if time zone is west of Greenwich,
# positive if east of Greenwich
# dst - daylight savings time, TRUE if daylight savings time is in effect
UniversalTimeToLocalCivilTime <- function(hour, dst, tz)
{
  hr <- hour
  
  # Adjust for time zone
  hr <- hr + tz
  
  # Check boundary conditions
  if (hr < 0) {
    hr <- hr + 24
    print("Time is for the next day")
  } else if (hr > 24) {
    hr <- hr - 24
    print("Time is for the previous day")
  }
  
  # Adjust for daylight savings time
  if (dst == TRUE) {
    hr <- hr + 1
  }
  
  return (hr)
}
