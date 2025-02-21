
# Functions to calculate the apparent place solar system bodies

# Calculate the apparent place of a planet by passing the function
# for calculating the planet's position and velocity
ApparentPlacePlanet <- function(jd, func1)
{
  # Calculate the epoch of observation
  t <- EpochOfObs(jd)
  
  # Extract the barycentric position and velocity of the Earth and convert from
  # KM and KM/sec to AU and AU/day
  earth_ssb <- PositionEarthSSB(t)
  earth_ssb <- earth_ssb / KM2AU
  
  # Extract the barycentric position and velocity of the planet and convert from
  # KM and KM/sec to AU and AU/day
  planet_ssb <- func1(t)
  planet_ssb <- planet_ssb / KM2AU
  
  # Calculate the geometric distance between the positions of the center of mass
  # of the planet and the Earth
  u <- planet_ssb - earth_ssb
  geom_dist <- VecNorm(u[,1])
  
  # Calculate the light travel time between the planet and Earth and update the
  # planet position and velocity
  tau <- geom_dist / CAUD
  planet_ssb <- func1(t - tau)
  planet_ssb <- planet_ssb / KM2AU
  u1 <- planet_ssb - earth_ssb
  
  # Adjust for the abberation of light
  abberation <- AberrationOfLight(u1[,1], earth_ssb[,2])
  u2 <- u1[,1] + abberation
  
  # Adjust for Precession
  prec <- PrecessionMatrix(t)
  u3 <- prec %*% u2
  
  # Adjust for Nutation
  nut_matrix <- NutationMatrix(t)
  u4 <- nut_matrix %*% u3
  
  z <- list(c(u4[1,1], u4[2,1], u4[3,1]), geom_dist)
  names(z) <- c("Position Vector", "Geometric Distance")
  
  return (z)
}

# Calculate the apparent place of the Sun
# jd = the julian day number
ApparentPlaceSun <- function(jd)
{
  # Calculate the epoch of observation
  t <- EpochOfObs(jd)
  
  # Extract the barycentric position and velocity of the Earth and convert from
  # KM and KM/sec to AU and AU/day
  earth_ssb <- PositionEarthSSB(t)
  earth_ssb <- earth_ssb / KM2AU
  
  # Extract the barycentric position and velocity of the Sun and convert from
  # KM and KM/sec to AU and AU/day
  sun_ssb <- PositionSunSSB(t)
  sun_ssb <- sun_ssb / KM2AU
  
  # Calculate the geometric distance between the positions of the center of mass
  # of the sun and the Earth
  u <- sun_ssb - earth_ssb
  geom_dist <- VecNorm(u[,1])
  
  # Calculate the light travel time between the sun and Earth and update the
  # sun's position and velocity
  tau <- geom_dist / CAUD
  sun_ssb <- PositionSunSSB(t - tau)
  sun_ssb <- sun_ssb / KM2AU
  u1 <- sun_ssb - earth_ssb
  
  # Adjust for the abberation of light
  abberation <- AberrationOfLight(u1[,1], earth_ssb[,2])
  u2 <- u1[,1] + abberation
  
  # Adjust for Precession
  prec <- PrecessionMatrix(t)
  u3 <- prec %*% u2
  
  # Adjust for Nutation
  nut_matrix <- NutationMatrix(t)
  u4 <- nut_matrix %*% u3
  
  z <- list(c(u4[1,1], u4[2,1], u4[3,1]), geom_dist)
  names(z) <- c("Position Vector", "Geometric Distance")
  
  return (z)
}

# Calculate the apparent place of the Moon
# jd = the julian day number
ApparentPlaceMoon <- function(jd)
{
  # Calculate the epoch of observation
  t <- EpochOfObs(jd)
  
  # Extract the barycentric position and velocity of the Earth and convert from
  # KM and KM/sec to AU and AU/Day
  earth_ssb <- PositionEarthSSB(t)
  earth_ssb <- earth_ssb / KM2AU

  # Extract the geocentric position and velocity of the moon and convert from
  # KM and KM/sec to AU and AU/Day
  moon_geo <- PositionMoonGEO(t)
  moon_geo <- moon_geo / KM2AU

  # Calculate the geometric distance of the moon in AU
  geom_dist <- VecNorm(moon_geo[,1])
  
  # Calculate the light travel time between the planet and Earth and update the
  # planet position and velocity
  tau <- geom_dist / CAUD
  moon_geo <- PositionMoonGEO(t - tau)
  moon_geo <- moon_geo / KM2AU

  # Adjust for the abberation of light
  abberation <- AberrationOfLight(moon_geo[,1], earth_ssb[,2])
  u1 <- moon_geo[,1] + abberation
  
  # Adjust for Precession
  prec <- PrecessionMatrix(t)
  u2 <- prec %*% u1
  
  # Adjust for Nutation
  nut_matrix <- NutationMatrix(t)
  u3 <- nut_matrix %*% u2
  
  sinpi = EARTHRADKM / (geom_dist * KM2AU)
  hor_parallax <- asin(sinpi)
  semi_diameter <- asin(0.272493 * sinpi)
  
  # Calculate the horizontal parallax in radians
  hor_parallax <- asin(EARTHRADKM/(geom_dist*KM2AU))
  
  # Return the moon's position vector, geocentric distance in earth radii,
  # horizontal parallax in radians, and the semi-diameter in radians
  z <- list(c(u3[1,1], u3[2,1], u3[3,1]), geom_dist*KM2AU/EARTHRADKM,
            hor_parallax, semi_diameter)
  names(z) <- c("Position Vector", "Geometric Distance",
                "Horizontal Parallax", "Semi-Diameter")
  
  return (z)
}

# Epoch of observation
# jd = julian day number
EpochOfObs <- function(jd)
{
  t_prime <- (jd - EPOCHJ2000) / DAYSJULCENT
  
  mean_anomaly <- (357.528 + 35999.050 * t_prime) * PI2 / 360
  
  s <- 0.001658 * sin(mean_anomaly + 0.01671 * sin(mean_anomaly))
  
  t <- jd + (s / SECDAY)
  
  return (t)
}

# Body is a position vector of length 3
# Earth is a position vector of length 3
# Sun is a position vector of length 3
RelativisticDeflectionOfLight <- function(body, earth, sun)
{
  body_geo <- body - earth
  body_helio <- body - sun
  earth_helio <- earth - sun
  
  u <- UnitVector(body_geo)
  q <- UnitVector(body_helio)
  e <- UnitVector(earth_helio)
  
  g1 <- MUC / VecNorm(earth_helio)
  g2 <- 1 + DotProduct(q,e)
  
  tmp1 <- DotProduct(u,q) * e
  tmp2 <- DotProduct(e,u) * q
  
  u1 <- VecNorm(u) * (u + g1/g2 * (tmp1 - tmp2))
  
  return (u1)
}

# body is a position vector of length 3
# earth is a velocity vector of length 3
AberrationOfLight <- function(body, earth)
{
  V <- earth / CAUD
  
  return (VecNorm(body) * V)
}
