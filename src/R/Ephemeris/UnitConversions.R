
# Convert degrees, arc minutes, and arc seconds to decimal degrees.
DMSToDeg <- function(degrees, minutes, seconds)
{
  # Validate input types
  if (!is.numeric(degrees) || !is.numeric(minutes) || !is.numeric(seconds)) {
    stop("All inputs must be numeric.")
  }
  
  # Determine the sign based on the first non-zero component
  components <- c(degrees, minutes, seconds)
  first_nonzero <- components[which(components != 0)[1]]
  sign <- ifelse(!is.na(first_nonzero) && first_nonzero < 0, -1, 1)
  
  # Use absolute values to compute magnitude
  decimal <- abs(degrees) + abs(minutes) / 60 + abs(seconds) / 3600
  
  # Apply the sign
  return(sign * decimal)
}

# Format decimal degrees as a string with a parameter to define
# how many digits to the right of the decimal point to display
FormatDegrees <- function(decimal_degrees, digits) {
  if (!is.numeric(decimal_degrees)) {
    stop("decimal_degrees must be numeric.")
  }
  if (!is.numeric(digits) || digits < 0 || digits != floor(digits)) {
    stop("digits must be a non-negative integer.")
  }
  
  # Format using sprintf to ensure fixed number of decimal places
  formatted <- sprintf(paste0("%.", digits, "f"), decimal_degrees)
  return(formatted)
}

# Convert decimal degrees to degrees, arc minutes, and arc seconds.
# Assumes that degrees is already normalized to the range 0 - 360
DegToDMS <- function(decimal_degrees, digits)
{
  if (!is.numeric(decimal_degrees)) {
    stop("Input must be numeric.")
  }
  if (!is.numeric(digits) || digits < 0 || digits != floor(digits)) {
    stop("digits must be a non-negative integer.")
  }
  
  # Preserve sign
  sign <- ifelse(decimal_degrees < 0, -1, 1)
  abs_degrees <- abs(decimal_degrees)
  
  # Extract DMS components
  degrees <- floor(abs_degrees)
  remainder <- (abs_degrees - degrees) * 60
  minutes <- floor(remainder)
  seconds <- round((remainder - minutes) * 60, digits)
  
  # Handle rollovers
  if (seconds >= 60) {
    seconds <- 0
    minutes <- minutes + 1
  }
  if (minutes >= 60) {
    minutes <- 0
    degrees <- degrees + 1
  }
  if (degrees >= 360) {
    degrees <- 0
  }
  
  # Apply sign to degrees only
  degrees <- sign * degrees
  
  return(list(
    degrees = as.integer(degrees),
    minutes = as.integer(minutes),
    seconds = seconds))
}

FormatDMS <- function(dms_list) {
  if (!is.list(dms_list) || length(dms_list) != 3) {
    stop("Input must be a list of three components: degrees, minutes, and seconds.")
  }
  
  degrees <- dms_list[[1]]
  minutes <- dms_list[[2]]
  seconds <- dms_list[[3]]
  
  if (!is.numeric(degrees) || !is.numeric(minutes) || !is.numeric(seconds)) {
    stop("All components of the list must be numeric.")
  }
  
  degrees_str <- sprintf("%d° ", as.integer(degrees))
  minutes_str <- sprintf(" %02d′ ", as.integer(abs(minutes)))
  seconds_str <- paste0(" ", formatC(abs(seconds), format = "f"), "″")
  
  return(paste0(degrees_str, minutes_str, seconds_str))
}

# Convert decimal hours to hours, minutes, and seconds.
HoursToHMS <- function(decimal_hours, digits) {
  if (!is.numeric(decimal_hours) || any(decimal_hours < 0)) {
    stop("Input must be a non-negative numeric value.")
  }
  if (missing(digits) || !is.numeric(digits) || digits < 0 || digits != floor(digits)) {
    stop("digits must be a specified non-negative whole number.")
  }
  
  # Extract parts
  hours <- floor(decimal_hours)
  remainder <- (decimal_hours - hours) * 60
  minutes <- floor(remainder)
  seconds <- round((remainder - minutes) * 60, digits)
  
  # Handle rollovers
  if (seconds >= 60) {
    seconds <- 0
    minutes <- minutes + 1
  }
  if (minutes >= 60) {
    minutes <- 0
    hours <- hours + 1
  }
  
  return(list(
    hours = as.integer(hours),
    minutes = as.integer(minutes),
    seconds = seconds
  ))
}

FormatHMS <- function(hms_list) {
  if (!is.list(hms_list) || length(hms_list) != 3) {
    stop("Input must be a list of three components: hours, minutes, and seconds.")
  }
  
  hours <- hms_list[[1]]
  minutes <- hms_list[[2]]
  seconds <- hms_list[[3]]
  
  if (!is.numeric(hours) || !is.numeric(minutes) || !is.numeric(seconds)) {
    stop("All components of the list must be numeric.")
  }
  
  hours_str <- sprintf("%02d", as.integer(hours))
  seconds_str <- formatC(seconds, format = "f")
  
  return(paste0(hours_str, "h ", minutes_str, "m ", seconds_str, "s"))
}

# Convert hours, minutes, and seconds to decimal hours.
HMSToHour <- function(hours, minutes, seconds)
{
  if (!is.numeric(hours) || !is.numeric(minutes) || !is.numeric(seconds)) {
    stop("All inputs must be numeric.")
  }
  if (any(hours < 0) || any(minutes < 0) || any(seconds < 0)) {
    stop("Hours, minutes, and seconds must be non-negative.")
  }
  
  decimal_hours <- hours + (minutes / 60) + (seconds / 3600)
  return(decimal_hours)
}

# Convert degrees to hours
DegToHour <- function(decDeg)
{
  try(if(decDeg < 0) stop("Degrees must be >= 0"))
  try(if(decDeg > 360) stop("Degrees must be <= 360"))

  decHr <- decDeg / 15

  return (decHr)
}

# Convert hours to degrees
HrToDeg <- function(decHr)
{
  try(if(decHr < 0) stop("Hours must be >= 0"))
  try(if(decHr > 24) stop("Hours must be <= 24"))

  decDeg <- decHr * 15

  return (decDeg)
}

# Convert decimal hours to degrees, arc minutes, and arc seconds.
HrToDMS <- function(decHr)
{
  try(if(decHr < 0) stop("Hours must be >= 0"))
  try(if(decHr > 24) stop("Hours must be <= 24"))

  decDeg <- HrToDeg(decHr)

  return (DegToDMS(decDeg))
}

# Convert decimal degrees to hours, minutes, and seconds.

DegToHMS <- function(decDeg)
{
  try(if(decDeg < 0) stop("Degrees must be >= 0"))
  try(if(decDeg > 360) stop("Degrees must be <= 360"))

  decHr <- DegToHour(decDeg)

  return (HourToHMS(decHr))
}

# Return string for degrees, arcminutes, and arcseconds
DMSString <- function(dms)
{
  str <- sprintf("%d\u00B0 %dm %.2fs",
                 dms[1], dms[2], dms[3])
  
  return (str)
}

# Return string for hours, minutes, and seconds
HMSString <- function(hms)
{
  str <- sprintf("%dh %dm %.2fs", hms[1], hms[2], hms[3])
  
  return(str)
}

# Return string for hours and minutes
HMString <- function(hm)
{
  str <- sprintf("%dh %dm ", hm[1], hm[2])
  
  return(str)
}

