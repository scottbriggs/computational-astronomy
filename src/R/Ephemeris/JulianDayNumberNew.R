
# New julian day functions based on feedback from Claude

julian_day_number <- function(year, month, day, calendar = "gregorian") {
  # Input validation
  if (year < -20000 || year > 20000) {
    stop("Year must be between -20000 and 20000")
  }
  if (month < 1 || month > 12) {
    stop("Month must be between 1 and 12")
  }
  if (day < 1 || day > 31) {
    stop("Day must be between 1 and 31")
  }
  
  # Convert to astronomical year numbering (no year 0)
  y <- year
  if (y <= 0) {
    y <- y - 1
  }
  
  # Validate day is valid for the month and year
  days_in_month <- get_days_in_month(y, month, calendar)
  if (day > days_in_month) {
    stop(sprintf("Invalid day %d for month %d in year %d (%s calendar)", 
                 day, month, year, calendar))
  }
  
  # Adjust month and year for the algorithm (Jan & Feb are months 13 & 14 of previous year)
  if (month < 3) {
    month <- month + 12
    y <- y - 1
  }
  
  # Calculate Julian Day Number using the appropriate formula
  a <- floor(y / 100)
  
  if (tolower(calendar) == "gregorian") {
    # Gregorian calendar formula
    b <- 2 - a + floor(a / 4)
  } else {
    # Julian calendar formula
    b <- 0
  }
  
  # Main calculation (works for both calendars with the appropriate 'b' value)
  jdn <- floor(365.25 * (y + 4716)) + 
    floor(30.6001 * (month + 1)) + 
    day + b - 1524.5
  
  return(as.integer(jdn))
}

get_days_in_month <- function(year, month, calendar = "gregorian") {
  # Days in each month (non-leap year)
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # Check for February in a leap year
  if (month == 2 && is_leap_year(year, calendar)) {
    return(29)
  }
  
  return(days[month])
}

is_leap_year <- function(year, calendar = "gregorian") {
  # Convert to astronomical year numbering
  y <- year
  if (y <= 0) {
    y <- y - 1
  }
  
  if (tolower(calendar) == "gregorian") {
    # Gregorian calendar leap year rules
    return((y %% 4 == 0 && y %% 100 != 0) || (y %% 400 == 0))
  } else {
    # Julian calendar leap year rules
    return(y %% 4 == 0)
  }
}

gregorian_to_julian_day <- function(year, month, day) {
  return(julian_day_number(year, month, day, "gregorian"))
}

julian_to_julian_day <- function(year, month, day) {
  return(julian_day_number(year, month, day, "julian"))
}

