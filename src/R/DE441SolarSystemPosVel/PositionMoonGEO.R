# Calculate the position of the Moon using the JPL DE 441 Ephemeris data
# based on a julian day number referencing the earth's geocenter

PositionMoonGEO <- function(jd)
{
  # Open connection to the database
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441_de440.duckdb")))
  
  # Select the data for the julian day number of interest
  df_jd <- dbGetQuery(con, "select distinct * from DE441Moon 
                      where Julian_Day_Start <= ? and Julian_Day_End > ? 
                      order by INTERVAL asc", params = list(jd, jd))
  
  # Calculate the length of the subinterval
  length_of_subinterval <- DE441DAYSPERBLOCK / DE441NUMSUBINTMOON
  
  subinterval <- floor(as.integer(jd - df_jd[1,1]) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the
  # subinterval begins with 0, but the subinterval begins with 1 in the 
  # database
  subinterval <- subinterval + 1
  
  # Subset data for the interval of interest
  df_moon <- df_jd[df_jd$INTERVAL == subinterval,]
  df_moon <- subset(df_moon, 
                     select = -c(Julian_Day_Start, Julian_Day_End, INTERVAL))
  
  # Normalize the Julian Day
  valid_start <- df_jd[1,1] + ((subinterval - 1) * length_of_subinterval)
  valid_end <- valid_start + length_of_subinterval
  temp <- jd - valid_start
  x <- (temp / length_of_subinterval * 2.0) - 1.0
  
  # Calculate the Chebyshev polynomials for position and velocity. The velocity
  # is the first derivative of the position polynomial
  chebyshev <- data.frame(matrix(0.0, nrow=DE441NUMCOEFFMOON, ncol=2))
  chebyshev[1,1] <- 1.0
  chebyshev[2,1] <- x
  chebyshev[1,2] <- 0.0
  chebyshev[2,2] <- 1.0
  
  # Calculate the position coefficients
  for (i in seq(from = 3, to = DE441NUMCOEFFMOON, by = 1)){
    chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
  }
  
  # Calculate the velocity coefficients
  for (i in seq(from = 3, to = DE441NUMCOEFFMOON, by = 1)){
    chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
  }
  
  # Calculate the position in kilometers and the velocity in kilometers/sec
  pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
  v <- 0
  for (v in seq(from = 13, to = 1, by = -1)){
    pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * df_moon[1,v])
    pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * df_moon[1,v+DE441NUMCOEFFMOON])
    pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * df_moon[1,v+2*DE441NUMCOEFFMOON])
    
    pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * df_moon[1,v])
    pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * df_moon[1,v+DE441NUMCOEFFMOON])
    pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * df_moon[1,v+2*DE441NUMCOEFFMOON])
  }
  
  # Scale the velocity
  scale_value <- 2 * DE441NUMSUBINTMOON / DE441DAYSPERBLOCK
  pos_vel[1,2] = pos_vel[1,2] * scale_value
  pos_vel[2,2] = pos_vel[2,2] * scale_value
  pos_vel[3,2] = pos_vel[3,2] * scale_value
  
  colnames(pos_vel) <- c('Position Vector', 'Velocity Vector')
  
  # Return the data
  return(pos_vel)
}