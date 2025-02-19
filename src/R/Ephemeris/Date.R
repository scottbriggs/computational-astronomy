
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