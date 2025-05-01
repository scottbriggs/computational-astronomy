
# Date functions

# Calculates the Julian Day given the month, day, and year. The algorithm works
# for any date in the common era (CE) or before the common era (BCE).
# The Julian Day Number is calculated for a calendar date at 12 noon.
# Decimal day numbers are handled
JulianDayNumber <- function(year, month, day)
{
  # Gregorian calendar adopted October 15, 1582
  IGREG <- (15 + 31 * (10 + 12 * 1582))
  jm <- 0
  
  intDay <- trunc(day)
  fracDay <- day - intDay
  
  jy <- year
  
  if (month > 2)
  { 
    jm <- month + 1
  } else {
    jy <- jy - 1
    jm <- month + 13
  }
  
  julday <- floor(365.25 * jy) + floor(30.6001 * jm) + intDay + 1720995
  
  # Check if date is in the Gregorian calendar. If so, apply corrections
  if (intDay + 31 * (month + 12 * year) >= IGREG)
  {
    ja <- trunc(0.01 * jy)
    julday <- julday + 2 - ja + trunc(0.25 * ja)
  }
  
  # Handle decimal day numbers
  if (fracDay > 0.5){
    fracDay <- fracDay - 0.5
    julday <- julday + fracDay}
  else if (fracDay <= 0.5){
    fracDay <- 0.5 - fracDay
    julday <- julday - fracDay
  }
  
  return (julday)
}

# Calculate the calendar date given the julian day number. The algorithm works
# for both Gregorian and Julian Calendar dates
CalendarDate <- function(jdn)
{
  IGREG <- 2299161
  ja <- 0
  
  if (jdn >= IGREG){
    jalpha <- trunc((jdn-1867216.25) / 36524.25)
    ja <- jdn + 1 + jalpha - trunc(jalpha/4)
  } else if (jdn < 0) {
    ja <- jdn + 36525 * (1 - jdn/36525)
  } else {
    ja <- jdn
  }
  
  jb <- ja + 1524
  jc <- trunc(6680 + (jb - 2439870 - 122.1) / 365.25)
  jd <- trunc(365.25 * jc)
  je <- trunc((jb - jd) / 30.6001)
  day <- jb - jd - trunc(30.6001 * je)
  month <- je - 1
  if (month > 12) {
    month <- month - 12
  }
  year <- jc - 4715
  if (month > 2){
    year <- year  - 1
  }
  if (year <= 0){
    year <- year - 1
  }
  if (jdn < 0) {
    year <- year - 100 * (1 - jdn) / 36525
  }
  
  calendarDate <- c(year, month, day)
  
  return (calendarDate)
}

# Calculate the day of week for the julian day number at 12 noon
DayOfWeek <- function(jdn)
{
  # Assume that the julian day number has been calculated for any time during
  # a day. Convert the julian day number to be at 12 noon
  jdn_int <- trunc(jdn)
  jdn_t <- jdn_int + 0.5
  
  dow_num <- (jdn_t + 1.5) %% 7
  dow <- ""
  
  if (dow_num == 0){
    dow <- "Sun"
  } else if (dow_num == 1) {
    dow <- "Mon"
  } else if (dow_num == 2) {
    dow <- "Tue"
  } else if (dow_num == 3) {
    dow <- "Wed"
  } else if (dow_num == 4) {
    dow <- "Thu"
  } else if (dow_num == 5) {
    dow <- "Fri"
  } else if (dow_num == 6) {
    dow <- "Sat"
  } else if (dow_num == 7) {
    dow <- "Sun"
  } else {
    stop("Invalid value for day of week")
  }
  
  return (dow)
}