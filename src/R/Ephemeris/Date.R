
# Date functions

# Julian Day Number
# Use the NASA API to retrieve the julian day number given a calendar date
# The year ranges from -9999-01-01 to 99999-12-31
# The month ranges from 01-12
# The day ranges from 01-31
# Hours range from 1 - 23
# Minutes range from 1 59
# Seconds range from 1 - 59

JulianDayNumber <- function(year, month, day, hour, minute, second) {
  
  # Format datetime for the API
  datetime <- sprintf("%+05d-%02d-%02d_%02d:%02d:%02d", year, month, day, hour, minute, second)
  
  # Build API URL
  url <- paste0("https://ssd-api.jpl.nasa.gov/jd_cal.api?cd=", datetime)
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  # Perform GET request
  response <- GET(url)
  
  # Check if request was successful
  if (status_code(response) != 200) {
    stop("API request failed: ", content(response, "text"))
  }
  
  # Parse API response
  data <- fromJSON(rawToChar(response$content))
  
  return(as.numeric(data$jd))
}

# Calendar Date
# Use the NASA API to retrieve the calendar date for a julian day number
# The julian date number can range from -1931076.5 to 38245308.5
CalendarDate <- function(jd)
{
  # Format API URL
  url <- paste0("https://ssd-api.jpl.nasa.gov/jd_cal.api?jd=", jd, "&format=d.5")
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  # Perform GET request
  response <- GET(url)
  
  # Check for a successful response
  if (status_code(response) != 200) {
    stop("API request failed: ", content(response, "text"))
  }
  
  # Parse JSON response
  data <- fromJSON(rawToChar(response$content))
  
  # Extract components from the response
  year <- data$year
  month <- data$month_name
  day <- as.integer(data$day_and_time)
  doy <- data$doy
  dow_name <- data$dow_name
  
  # Convert fractional days to hours, minutes, and seconds
  frac_day <- as.numeric(data$day_and_time) - as.integer(data$day_and_time)
  
  total_seconds <- frac_day * SECDAY
  
  hours <- floor(total_seconds / 3600)
  minutes <- floor((total_seconds %% 3600) / 60)
  seconds <- round(total_seconds %% 60)
  
  df <- data.frame(year, month, day, hours, minutes, seconds, jd, dow_name,
                   doy)
  
  colnames(df) <- c("Year", "Month", "Day", "Hours", "Minutes", "Seconds",
                    "JDN", "DOW", "DOY")
  
  return(df)
}

# Calculate the date of Easter in the Gregorian and Julian calendars
DateOfEaster <- function(year)
{
  month <- ""
  day <- 0
  
  # Gregorian Calendar
  if (year >= 1583) {
    a <- year %% 19
    b <- year %/% 100
    c <- year %% 100
    d <- b %/% 4
    e <- b %% 4
    f <- (b + 8) %/% 25
    g <- (b - f + 1) %/% 3
    h <- (19 * a + b - d - g + 15) %% 30
    i <- c %/% 4
    k <- c %% 4
    l <- (32 + 2 * e + 2 * i - h - k) %% 7
    m <- (a + 11 * h + 22 * l) %/% 451
    n <- (h + l - 7 * m + 114) %/% 31
    p <- (h + l - 7 * m + 114) %% 31
    
    if (n == 3){
      month <- "March"
    } else if (n == 4){
      month <- "April"
    }
    
    day <- p + 1
    
  } else { # Julian calendar
    a <- year %% 4
    b <- year %% 7
    c <- year %% 19
    d <- (19 * c + 15) %% 30
    e <- (2 * a + 4 * b - d + 34) %% 7
    f <- (d + e + 114) %/% 31
    g <- (d + e + 114) %% 31
    
    if (f == 3){
      month <- "March"
    } else if (f == 4){
      month <- "April"
    }
    
    day <- g + 1
  }
  
  res <- data.frame(year, month, day)
  colnames(res) <- c("Year", "Month", "Day")
  
  return(res)
}
