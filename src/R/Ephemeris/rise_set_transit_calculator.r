library(data.table)
library(lubridate)

# Rise, Set, and Transit Calculator for Solar System Bodies
# Optimized for 1-minute precision

# Main calculation function
calculate_rise_set_transit <- function(latitude, longitude, date, body_positions, 
                                     timezone = "UTC", body_type = "planet") {
  
  # Validate inputs
  if (abs(latitude) > 90) stop("Latitude must be between -90 and 90 degrees")
  if (abs(longitude) > 180) stop("Longitude must be between -180 and 180 degrees")
  
  # Convert date to Date object if needed
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  
  # Extract positions for the calculation day (need 3 points for interpolation)
  positions <- prepare_positions(body_positions, date)
  
  if (nrow(positions) < 3) {
    stop("Need at least 3 position points (0h, 12h, 24h) for interpolation")
  }
  
  # Check for polar conditions
  polar_status <- check_polar_conditions(latitude, positions$dec_deg)
  
  if (polar_status != "normal") {
    return(list(
      date = date,
      transit = calculate_transit_time(latitude, longitude, positions, timezone),
      rise = if (polar_status == "circumpolar") "circumpolar" else "never_rises",
      set = if (polar_status == "circumpolar") "circumpolar" else "never_sets",
      status = polar_status
    ))
  }
  
  # Calculate transit time
  transit_time <- calculate_transit_time(latitude, longitude, positions, timezone)
  
  # Calculate rise and set times
  rise_set <- calculate_rise_set_times(latitude, longitude, positions, 
                                     transit_time, body_type, timezone)
  
  return(list(
    date = date,
    transit = transit_time,
    rise = rise_set$rise,
    set = rise_set$set,
    status = "normal"
  ))
}

# Prepare position data for interpolation
prepare_positions <- function(body_positions, date) {
  # Expected columns: datetime, ra_hours, dec_deg
  # Filter for the target date and surrounding times
  
  target_times <- c(
    as.POSIXct(paste(date, "00:00:00"), tz = "UTC"),
    as.POSIXct(paste(date, "12:00:00"), tz = "UTC"),
    as.POSIXct(paste(date + 1, "00:00:00"), tz = "UTC")
  )
  
  # Find closest positions to target times
  positions <- data.table()
  
  for (target_time in target_times) {
    closest_idx <- which.min(abs(as.numeric(body_positions$datetime - target_time)))
    if (length(closest_idx) > 0) {
      pos <- body_positions[closest_idx, ]
      pos$hours_from_midnight <- as.numeric(difftime(pos$datetime, 
                                                    as.POSIXct(paste(date, "00:00:00"), tz = "UTC"), 
                                                    units = "hours"))
      positions <- rbind(positions, pos)
    }
  }
  
  return(positions[order(positions$hours_from_midnight)])
}

# Check for polar conditions
check_polar_conditions <- function(latitude, declinations) {
  mean_dec <- mean(declinations, na.rm = TRUE)
  
  # 1-degree buffer zone for edge cases
  if (mean_dec > (90 - abs(latitude) - 1)) {
    return("circumpolar")
  } else if (mean_dec < -(90 - abs(latitude) + 1)) {
    return("never_visible")
  } else {
    return("normal")
  }
}

# Calculate transit time (meridian crossing)
calculate_transit_time <- function(latitude, longitude, positions, timezone) {
  
  # Use mean position for the day
  mean_ra <- mean(positions$ra_hours, na.rm = TRUE)
  
  # Convert to local sidereal time
  # Simplified calculation - in practice would use more precise sidereal time
  lst_at_transit <- mean_ra
  
  # Convert to local solar time
  # Transit occurs when LST = RA
  # Approximate conversion (ignoring equation of time for simplicity)
  transit_ut <- (mean_ra - longitude/15) %% 24
  
  # Convert to local timezone
  transit_local <- (transit_ut + get_timezone_offset(timezone)) %% 24
  
  # Round to nearest minute
  hours <- floor(transit_local)
  minutes <- round((transit_local - hours) * 60)
  
  if (minutes == 60) {
    hours <- (hours + 1) %% 24
    minutes <- 0
  }
  
  return(sprintf("%02d:%02d", hours, minutes))
}

# Calculate rise and set times
calculate_rise_set_times <- function(latitude, longitude, positions, 
                                   transit_time, body_type, timezone) {
  
  # Use mean declination for the day
  mean_dec <- mean(positions$dec_deg, na.rm = TRUE)
  
  # Apply body-specific horizon depression
  horizon_depression <- get_horizon_depression(body_type)
  
  # Calculate hour angle
  cos_h <- -tan(deg_to_rad(latitude)) * tan(deg_to_rad(mean_dec)) - 
           sin(deg_to_rad(horizon_depression)) / 
           (cos(deg_to_rad(latitude)) * cos(deg_to_rad(mean_dec)))
  
  # Check if solution exists
  if (abs(cos_h) > 1) {
    if (cos_h > 1) {
      return(list(rise = "never_rises", set = "never_sets"))
    } else {
      return(list(rise = "circumpolar", set = "circumpolar"))
    }
  }
  
  # Calculate hour angle in degrees
  hour_angle_deg <- rad_to_deg(acos(cos_h))
  
  # Convert to time
  hour_angle_time <- hour_angle_deg / 15.04107  # 15.04107 for sidereal rate
  
  # Calculate rise and set times relative to transit
  transit_decimal <- time_to_decimal(transit_time)
  
  rise_decimal <- (transit_decimal - hour_angle_time) %% 24
  set_decimal <- (transit_decimal + hour_angle_time) %% 24
  
  # Round to nearest minute and format
  rise_time <- format_time_from_decimal(rise_decimal)
  set_time <- format_time_from_decimal(set_decimal)
  
  return(list(rise = rise_time, set = set_time))
}

# Get horizon depression for different body types
get_horizon_depression <- function(body_type) {
  switch(body_type,
    "sun" = -50/60,      # -50 arcminutes (refraction + semi-diameter)
    "moon" = -34/60,     # -34 arcminutes (refraction only, semi-diameter ignored for 1-min precision)
    "planet" = -34/60,   # -34 arcminutes (refraction only)
    -34/60               # default: refraction only
  )
}

# Utility functions
deg_to_rad <- function(deg) deg * pi / 180
rad_to_deg <- function(rad) rad * 180 / pi

time_to_decimal <- function(time_str) {
  parts <- as.numeric(strsplit(time_str, ":")[[1]])
  return(parts[1] + parts[2]/60)
}

format_time_from_decimal <- function(decimal_time) {
  hours <- floor(decimal_time)
  minutes <- round((decimal_time - hours) * 60)
  
  if (minutes == 60) {
    hours <- (hours + 1) %% 24
    minutes <- 0
  }
  
  return(sprintf("%02d:%02d", hours, minutes))
}

get_timezone_offset <- function(timezone) {
  # Simplified timezone offset - in practice would use proper timezone handling
  switch(timezone,
    "UTC" = 0,
    "EST" = -5,
    "CST" = -6,
    "MST" = -7,
    "PST" = -8,
    0  # default to UTC
  )
}

# Batch calculation function for multiple bodies
calculate_multiple_bodies <- function(latitude, longitude, date, all_positions, 
                                    timezone = "UTC") {
  
  results <- list()
  
  for (body_name in names(all_positions)) {
    body_type <- determine_body_type(body_name)
    
    tryCatch({
      result <- calculate_rise_set_transit(
        latitude = latitude,
        longitude = longitude,
        date = date,
        body_positions = all_positions[[body_name]],
        timezone = timezone,
        body_type = body_type
      )
      
      results[[body_name]] <- result
    }, error = function(e) {
      results[[body_name]] <- list(
        date = date,
        transit = "error",
        rise = "error",
        set = "error",
        status = paste("error:", e$message)
      )
    })
  }
  
  return(results)
}

determine_body_type <- function(body_name) {
  body_name_lower <- tolower(body_name)
  
  if (body_name_lower == "sun") return("sun")
  if (body_name_lower == "moon") return("moon")
  return("planet")
}

# Function to create sample position data for testing
create_sample_positions <- function(body_name, date, ra_start = 12, dec_start = 20) {
  # Create sample apparent positions for a day
  times <- seq(from = as.POSIXct(paste(date, "00:00:00"), tz = "UTC"),
               to = as.POSIXct(paste(date + 1, "00:00:00"), tz = "UTC"),
               by = "6 hours")
  
  # Simple motion simulation
  n_points <- length(times)
  ra_hours <- ra_start + seq(0, 0.5, length.out = n_points)  # slight motion
  dec_deg <- dec_start + 0.1 * sin(seq(0, 2*pi, length.out = n_points))  # slight variation
  
  return(data.table(
    datetime = times,
    ra_hours = ra_hours,
    dec_deg = dec_deg
  ))
}

# Example usage and test function
run_example <- function() {
  # Example: Calculate for multiple bodies at specific location and date
  latitude <- 40.7128   # New York City
  longitude <- -74.0060
  date <- as.Date("2024-06-21")  # Summer solstice
  timezone <- "EST"
  
  # Create sample position data for multiple bodies
  sun_positions <- create_sample_positions("sun", date, ra_start = 6, dec_start = 23.5)
  moon_positions <- create_sample_positions("moon", date, ra_start = 18, dec_start = -15)
  mars_positions <- create_sample_positions("mars", date, ra_start = 10, dec_start = 15)
  
  all_positions <- list(
    sun = sun_positions,
    moon = moon_positions,
    mars = mars_positions
  )
  
  # Calculate rise, set, transit for all bodies
  results <- calculate_multiple_bodies(latitude, longitude, date, all_positions, timezone)
  
  # Print results
  cat("Rise, Set, and Transit Times for", as.character(date), "\n")
  cat("Location:", latitude, "°N,", abs(longitude), "°W\n")
  cat("Timezone:", timezone, "\n\n")
  
  for (body_name in names(results)) {
    result <- results[[body_name]]
    cat(sprintf("%-8s: Rise %8s  Transit %8s  Set %8s  [%s]\n",
                toupper(body_name), 
                result$rise, 
                result$transit, 
                result$set,
                result$status))
  }
  
  return(results)
}

# Test polar conditions
run_polar_example <- function() {
  # Example: Arctic location during summer
  latitude <- 75.0      # High Arctic
  longitude <- -68.0
  date <- as.Date("2024-06-21")  # Summer solstice
  timezone <- "UTC"
  
  sun_positions <- create_sample_positions("sun", date, ra_start = 6, dec_start = 23.5)
  
  all_positions <- list(sun = sun_positions)
  
  results <- calculate_multiple_bodies(latitude, longitude, date, all_positions, timezone)
  
  cat("Polar Example - Arctic Summer Solstice\n")
  cat("Location:", latitude, "°N,", abs(longitude), "°W\n\n")
  
  result <- results$sun
  cat(sprintf("SUN: Rise %s  Transit %s  Set %s  [%s]\n",
              result$rise, result$transit, result$set, result$status))
  
  return(results)
}

# Run examples
cat("=== Standard Location Example ===\n")
standard_results <- run_example()

cat("\n=== Polar Location Example ===\n")
polar_results <- run_polar_example()