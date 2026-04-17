
# Date functions

# Julian Day Number
JulianDayNumber <- function(year, month, day)
{
  # Day is a float
  int_day <- as.integer(day)
  frac_day <- day - int_day
  
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441.duckdb")))
  
  query_str <- "select jdn from JulianDayNumber where year = ? 
and month_number = ? and day = ?"
  
  jd <- dbGetQuery(con, query_str, params = list(year, month, int_day))
  
  dbDisconnect(con, shutdown = TRUE)
  
  if (frac_day == 0) {jdn = jd -1 + 0.5}
  
  if (frac_day == 0.5) {jdn = jd}
  
  if (frac_day > 0.5) {jdn <- jd + frac_day - 0.5}
  
  if (frac_day < 0.5 && frac_day > 0) {jdn <- jd - 1 + 0.5 + frac_day}
  
  return (as.numeric(jdn))
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
