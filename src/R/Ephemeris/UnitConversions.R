
# Convert degrees, arc minutes, and arc seconds to decimal degrees.
DMSToDeg <- function(deg, min, sec)
{
  sg <- 0
  if (deg != 0) {
    sg <- sign(deg)
  } else if (deg == 0 && min != 0) {
    sg <- sign(min)
  } else if (deg == 0 && min == 0 && sec != 0) {
    sg <- sign(sec)
  }

  degrees <- abs(deg)
  minutes <- abs(min)
  seconds <- abs(sec)
  dm <- seconds / 60
  tot_min <- dm + minutes
  decDeg <- tot_min / 60
  decDeg <- decDeg + degrees
  decDeg <- sg * decDeg

  return (decDeg)
}

# Convert decimal degrees to degrees, arc minutes, and arc seconds.
DegToDMS <- function(decDeg)
{
  sg <- sign(decDeg)

  Dec <- abs(decDeg)
  Degrees <- IntPart(Dec)
  Minutes <- IntPart(60 * FracPart(Dec))
  Seconds <- 60 * FracPart(60 * FracPart(Dec))

  Seconds <- round(Seconds, 2)

  if (Seconds == 60) {
    Seconds <- 0
    Minutes <- Minutes + 1
  }

  if (Minutes == 60) {
    Minutes <- 0
    Degrees <- Degrees + 1
  }

  if (Degrees != 0) {
    Degrees <- Degrees * sg
  } else if (Minutes != 0) {
      Minutes <- Minutes * sg
    } else if (Seconds != 0) {
      Seconds <- Seconds * sg
    }

  return (c(Degrees, Minutes, Seconds))
}

# Convert decimal hours to hours, minutes, and seconds.
HourToHMS <- function(decHr)
{
  try(if(decHr < 0) stop("Hours must be >= 0"))
  try(if(decHr > 24) stop("Hours must be <= 24"))

  Hours <- IntPart(decHr)
  Minutes <- IntPart(60 * FracPart(decHr))
  Seconds <- 60 * FracPart(60 * FracPart(decHr))

  Seconds <- round(Seconds, 2)

  if (Seconds == 60) {
    Seconds <- 0
    Minutes <- Minutes + 1
  }

  if (Minutes == 60) {
    Minutes <- 0
    Hours <- Hours + 1
  }

  return (c(Hours, Minutes, Seconds))
}

# Convert hours, minutes, and seconds to decimal hours.
HMSToHour <- function(hours, minutes, seconds)
{
  try(if(hours < 0) stop("Hours must be >= 0"))
  try(if(hours > 24) stop("Hours must be <= 24"))

  hm <- seconds / 60
  tot_min <- hm + minutes
  decHr <- tot_min / 60
  decHr <- decHr + hours

  return (decHr)
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
