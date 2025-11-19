
# Functions to calculate julian day numbers

month_names <- c( "January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", 
                  "December")

weekday_names <- c( "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                    "Friday", "Saturday")

# ---------- Vectorized calendar <-> JDN ----------
gregorian_to_jdn_vec <- function(year, month, day) {
  
  a <- (14L - month) %/% 12L
  y <- year + 4800L - a
  m <- month + 12L * a - 3L
  jdn <- day + (153L * m + 2L) %/% 5L + 365L * y + y %/% 4L - y %/% 100L + 
    y %/% 400L - 32045L
  as.integer(jdn)
  
}

julian_to_jdn_vec <- function(year, month, day) {
  
  a <- (14L - month) %/% 12L
  y <- year + 4800L - a
  m <- month + 12L * a - 3L
  jdn <- day + (153L * m + 2L) %/% 5L + 365L * y + y %/% 4L - 32083L
  as.integer(jdn)
  
}

jdn_to_gregorian_vec <- function(jdn) {
  
  a <- jdn + 32044L
  b <- (4L * a + 3L) %/% 146097L
  c <- a - (146097L * b) %/% 4L
  d <- (4L * c + 3L) %/% 1461L
  e <- c - (1461L * d) %/% 4L
  m <- (5L * e + 2L) %/% 153L
  day   <- e - (153L * m + 2L) %/% 5L + 1L
  month <- m + 3L - 12L * (m %/% 10L)
  year  <- 100L * b + d - 4800L + (m %/% 10L)
  
  list( year  = as.integer(year),
        month = as.integer(month),
        day   = as.integer(day))
  
}

jdn_to_julian_vec <- function(jdn) {
  
  c_ <- jdn + 32082L
  d <- (4L * c_ + 3L) %/% 1461L
  e <- c_ - (1461L * d) %/% 4L
  m <- (5L * e + 2L) %/% 153L
  day   <- e - (153L * m + 2L) %/% 5L + 1L
  month <- m + 3L - 12L * (m %/% 10L)
  year  <- d - 4800L + (m %/% 10L)
  
  list( year  = as.integer(year),
        month = as.integer(month),
        day   = as.integer(day))
  
}



# JDN of 1582-10-15 (first Gregorian day)
gregorian_reform_jdn <- gregorian_to_jdn_vec(1582L, 10L, 15L)[1L]

# Hybrid: JDN -> calendar (vectorized)
jdn_to_calendar_vec <- function(jdn) {
  
  is_greg <- jdn >= gregorian_reform_jdn
  year  <- integer(length(jdn))
  month <- integer(length(jdn))
  day   <- integer(length(jdn))
  
  if (any(is_greg)) {
    
    g <- jdn_to_gregorian_vec(jdn[is_greg])
    year[is_greg]  <- g$year
    month[is_greg] <- g$month
    day[is_greg]   <- g$day
    
  }
  
  
  
  if (any(!is_greg)) {
    
    j <- jdn_to_julian_vec(jdn[!is_greg])
    year[!is_greg]  <- j$year
    month[!is_greg] <- j$month
    day[!is_greg]   <- j$day
    
  }
  
  list(year = year, month = month, day = day)
  
}



# Hybrid: scalar year/month/day -> JDN (used only for bounds)

calendar_to_jdn_scalar <- function(year, month, day) {
  
  if (year < 1582L) {
    
    julian_to_jdn_vec(year, month, day)
    
  } else if (year > 1582L) {
    
    gregorian_to_jdn_vec(year, month, day)
    
  } else { # year == 1582
    
    if (month < 10L) {
      
      julian_to_jdn_vec(year, month, day)
      
    } else if (month > 10L) {
      
      gregorian_to_jdn_vec(year, month, day)
      
    } else { # month == 10
      
      if (day <= 4L) {
        
        julian_to_jdn_vec(year, month, day)
        
      } else if (day >= 15L) {
        
        gregorian_to_jdn_vec(year, month, day)
        
      } else {
        
        stop("Dates 1582-10-05 to 1582-10-14 do not exist in this hybrid calendar.")
        
      }
      
    }
    
  }
  
}



# Start-of-year JDN for a vector of years (for DOY)
year_start_jdn_vec <- function(years) {
  
  res <- integer(length(years))
  before <- years < 1582L
  after  <- years > 1582L
  eq1582 <- years == 1582L
  
  if (any(before)) {
    
    res[before] <- julian_to_jdn_vec(years[before], 1L, 1L)
    
  }
  
  if (any(after)) {
    
    res[after] <- gregorian_to_jdn_vec(years[after], 1L, 1L)
    
  }
  
  if (any(eq1582)) {
    
    # Jan 1, 1582 is still Julian in this hybrid
    
    res[eq1582] <- julian_to_jdn_vec(years[eq1582], 1L, 1L)
    
  }
  
  res
  
}



# ---------- Day-of-week and day-of-year (vectorized) ----------



day_of_week_name_vec <- function(jdn) {
  
  idx <- (jdn + 1L) %% 7L  # 0=Sunday,...,6=Saturday
  weekday_names[idx + 1L]
  
}



day_of_year_vec <- function(jdn, year, start_year, year_start_jdn) {
  
  idx <- (year - start_year) + 1L
  jdn_start <- year_start_jdn[idx]
  as.integer(jdn - jdn_start + 1L)
  
}



# ---------- In-memory generator ----------


# Date ranges correspond to the range of the DE441 ephemeris
generate_jdn_table_in_memory <- function( start_year = -13200L, 
                                          end_year   = 17191L)
{
  
  # JDN bounds
  
  start_jdn <- calendar_to_jdn_scalar(start_year, 1L, 1L)
  end_jdn   <- calendar_to_jdn_scalar(end_year, 12L, 31L)
  
  cat("Start JDN:", start_jdn, " End JDN:", end_jdn, "\n")
  total_days <- end_jdn - start_jdn + 1L
  cat("Total days:", total_days, "\n")
  
  # All JDNs in one vector
  jdn_vec <- seq.int(start_jdn, end_jdn)
  
  # Convert to calendar dates (hybrid)
  cal <- jdn_to_calendar_vec(jdn_vec)
  year  <- cal$year
  month <- cal$month
  day   <- cal$day
  
  # Explicitly enforce requested year bounds (should already match)
  keep <- (year >= start_year) & (year <= end_year)
  jdn_vec <- jdn_vec[keep]
  year    <- year[keep]
  month   <- month[keep]
  day     <- day[keep]
  
  # Precompute start-of-year JDN for DOY
  years_all <- seq.int(start_year, end_year)
  year_start_jdn <- year_start_jdn_vec(years_all)
  
  # Day-of-year & day-of-week
  doy <- day_of_year_vec(jdn_vec, year, start_year, year_start_jdn)
  dow <- day_of_week_name_vec(jdn_vec)
  
  # Build data.table in memory
  DT <- data.table(jdn = jdn_vec, year = year, month_number = month,
                   month_name = month_names[month], day = day, 
                   day_of_year = doy, day_of_week = dow)
  
  DT[]
  
}



# ---------- Example usage ----------

# This will return a ~9M-row data.table in memory:

# jdn_dt <- generate_jdn_table_in_memory()

# str(jdn_dt)