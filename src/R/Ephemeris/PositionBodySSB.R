
# Function to retrieve DE441 Header data to calculate the position and velocity
# vectors for solar system bodies
GetDE441HeaderCoefData <- function(body)
{
  # Body information from the de441 header file
  de441_body <- c("Mercury", "Venus", "EMB", "Mars", "Jupiter", "Saturn",
                  "Uranus", "Neptune", "Pluto", "Moon", "Sun", "Nutation",
                  "Libration")
  
  de441_block_offset <- c(3, 171, 231, 309, 342, 366, 387, 405, 423, 441, 753,
                          819, 899)
  
  de441_properties <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3)
  
  de441_coeff <- c(14, 10, 13, 11, 8, 7, 6, 6, 6, 13, 11, 10, 10)
  
  de441_interval <- c(4L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 8L, 2L, 4L, 4L)
  
  dx <- which(de441_body == body)
  
  res <- c(de441_block_offset[dx], de441_properties[dx], de441_coeff[dx],
           de441_interval[dx])
}

# Retrieves the position of a solar system body using the DE441 ephemeris data
# based on the julian day number and the body name (Mercury, Venus, EMB, Mars,
# Jupiter, Saturn, Uranus, Neptune, Pluto, Sun, Moon, Libration)
# num_coef is the number of coefficients for the body
# num_interval is the number of intervals for the body
PositionBodySSB <- function(body, jd)
{
  # Retrieve the number of coefficients and the interval for the body of interest
  bd <- GetDE441HeaderCoefData(body)
  num_interval <- bd[4]
  num_coef <- bd[3]
  
  # Build query string
  tb <- paste0("DE441", body, " ")
  query_str <- paste0("select distinct * from ", tb,
                      "where Julian_Day_Start <= ? ", "and Julian_Day_End > ? ",
                      "order by INTERVAL asc")
  
  # Connect to the database
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441.duckdb")))
  
  df_jd <- dbGetQuery(con, query_str, params = list(jd, jd))
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  # Calculate the subinterval
  length_of_subinterval <- 32 / num_interval
  subinterval <- floor(as.integer(jd - df_jd[1,1]) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the
  # subinterval begins with 0, but the subinterval begins with 1 in the 
  # database
  subinterval <- subinterval + 1
  
  # Subset data for the interval of interest
  df_body <- df_jd[df_jd$INTERVAL == subinterval,]
  df_body <- subset(df_body, select = -c(Julian_Day_Start, Julian_Day_End, INTERVAL))
  
  # Normalize the Julian Day
  valid_start <- df_jd[1,1] + ((subinterval - 1) * length_of_subinterval)
  valid_end <- valid_start + length_of_subinterval
  temp <- jd - valid_start
  x <- (temp / length_of_subinterval * 2.0) - 1.0
  
  # Calculate the Chebyshev polynomials for position and velocity. The velocity
  # is the first derivative of the position polynomial
  chebyshev <- data.frame(matrix(0.0, nrow = num_coef, ncol=2))
  chebyshev[1,1] <- 1.0
  chebyshev[2,1] <- x
  chebyshev[1,2] <- 0.0
  chebyshev[2,2] <- 1.0
  
  # Calculate the position coefficients
  for (i in seq(from = 3, to = num_coef, by = 1)){
    chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
  }
  
  # Calculate the velocity coefficients
  for (i in seq(from = 3, to = num_coef, by = 1)){
    chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
  }
  
  # Calculate the position in kilometers and the velocity in kilometers/sec
  pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
  v <- 0
  for (v in seq(from = num_coef, to = 1, by = -1)){
    pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * df_body[1,v])
    pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * df_body[1,v + num_coef])
    pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * df_body[1,v + 2 * num_coef])
    
    pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * df_body[1,v])
    pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * df_body[1,v + num_coef])
    pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * df_body[1,v + 2 * num_coef])
  }
  
  # Scale the velocity
  scale_value <- 2 * num_interval / 32
  pos_vel[1,2] = pos_vel[1,2] * scale_value
  pos_vel[2,2] = pos_vel[2,2] * scale_value
  pos_vel[3,2] = pos_vel[3,2] * scale_value
  
  colnames(pos_vel) <- c('Position Vector', 'Velocity Vector')
  
  # Return the data
  return(pos_vel)
}

NutationPosition <- function(body, jd)
{
  # Retrieve the number of coefficients and the interval for the body of interest
  bd <- GetDE441HeaderCoefData(body)
  num_interval <- bd[4]
  num_coef <- bd[3]
  
  # Build query string
  tb <- paste0("DE441", body, " ")
  query_str <- paste0("select distinct * from ", tb,
                      "where Julian_Day_Start <= ? ", "and Julian_Day_End > ? ",
                      "order by INTERVAL asc")
  
  # Connect to the database
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441.duckdb")))
  
  df_jd <- dbGetQuery(con, query_str, params = list(jd, jd))
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  # Calculate the subinterval
  length_of_subinterval <- 32 / num_interval
  subinterval <- floor(as.integer(jd - df_jd[1,1]) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the
  # subinterval begins with 0, but the subinterval begins with 1 in the 
  # database
  subinterval <- subinterval + 1
  
  # Subset data for the interval of interest
  df_body <- df_jd[df_jd$INTERVAL == subinterval,]
  df_body <- subset(df_body, select = -c(Julian_Day_Start, Julian_Day_End, INTERVAL))
  
  # Normalize the Julian Day
  valid_start <- df_jd[1,1] + ((subinterval - 1) * length_of_subinterval)
  valid_end <- valid_start + length_of_subinterval
  temp <- jd - valid_start
  x <- (temp / length_of_subinterval * 2.0) - 1.0
  
  # Calculate the Chebyshev polynomials for nutation coefficients
  chebyshev <- data.frame(matrix(0.0, nrow = num_coef, ncol=2))
  chebyshev[1,1] <- 1.0
  chebyshev[2,1] <- x
  chebyshev[1,2] <- 0.0
  chebyshev[2,2] <- 1.0
  
  for (i in seq(from = 3, to = num_coef, by = 1)){
    chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
  }
  
  for (i in seq(from = 3, to = num_coef, by = 1)){
    chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
  }
  
  # Calculate the nutation (longitude, obliquity) in radians
  nut_ang <- data.frame(matrix(0.0, nrow=2, ncol=2))
  v <- 0
  for (v in seq(from = num_coef, to = 1, by = -1)){
    nut_ang[1,1] <- nut_ang[1,1] + (chebyshev[v,1] * df_body[1,v])
    nut_ang[1,2] <- nut_ang[1,2] + (chebyshev[v,1] * df_body[1,v+num_coef])
    
    nut_ang[2,1] <- nut_ang[2,1] + (chebyshev[v,2] * df_body[1,v])
    nut_ang[2,2] <- nut_ang[2,2] + (chebyshev[v,2] * df_body[1,v+num_coef])
  }
  
  # Scale the change in longitude and obliquity
  scale_value <- 2 * num_interval / 32
  nut_ang[2,1] <- nut_ang[2,1] * scale_value
  nut_ang[2,2] <- nut_ang[2,2] * scale_value
  
  colnames(nut_ang) <- c('Nutation in Longitude', 'Nutation in Obliquity')
  
  return(nut_ang)
}

PositionEarthSSB <- function(jd)
{
  emb_pos_vel <- PositionBodySSB("EMB", jd)
  moon_pos_vel <- PositionBodySSB("Moon", jd)
  
  earth_pos_vel <- data.frame(matrix(0.0, nrow=2, ncol=3))
  earth_pos_vel <- emb_pos_vel - (moon_pos_vel / (1 + EMRAT))
  
  colnames(earth_pos_vel) <- c('Position Vector', 'Velocity Vector')
  
  return(earth_pos_vel)
}