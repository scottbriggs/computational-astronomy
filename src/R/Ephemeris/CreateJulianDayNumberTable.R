
# Call NASA API to get Julian Day Numbers and corresponding dates in the
# Gregorian and Julian calendars for most of the date range of the DE441
# ephemeris

CreateJulianDayNumberTable <- function(){
  
  # Call the NASA API with the earliest julian day number supported
  # by the API
  httr::set_config(config(ssl_verifypeer = 0L))
  res <- GET("https://ssd-api.jpl.nasa.gov/jd_cal.api?jd=-1931076.5")
  data <- fromJSON(rawToChar(res$content))
  
  # extract the day of the month
  dayOfMonth <- 0
  cd <- str_locate_all(data$cd, "-")
  tmp <- sapply(cd, tail, 1)
  dayOfMonth <- as.integer(str_sub(data$cd, tmp[[1]]+1, tmp[[1]]+2))
  
  # Create the date frame to hold the date information
  calendar <- data.frame(JulianDayNumber = c(-1931076.5),
                         Year = c(data$year),
                         Month = c(as.integer(data$month)),
                         MonthName = c(data$month_name),
                         DayOfMonth = c(dayOfMonth),
                         DayOfWeek = c(as.integer(data$dow)),
                         DayOfWeekName = c(data$dow_name),
                         DayOfYear = c(as.integer(data$doy)))
  
  # Add column names
  colnames(calendar) <- c("JulianDayNumber", "Year", "Month", "MonthName",
                          "DayOfMonth", "DayOfWeek", "DayOfWeekName", "DayOfYear")
  
  # Populate the date information for the rest of the julian day numbers
  for (i in seq(from = -1931075.5, to = -1831075.5, by = 1)) {
    res <- GET(paste0("https://ssd-api.jpl.nasa.gov/jd_cal.api?jd=", i))
    data <- fromJSON(rawToChar(res$content))
    
    dayOfMonth <- 0
    cd <- str_locate_all(data$cd, "-")
    tmp <- sapply(cd, tail, 1)
    dayOfMonth <- as.integer(str_sub(data$cd, tmp[[1]]+1, tmp[[1]]+2))
    
    newRow <- data.frame(JulianDayNumber = c(i),
                         Year = c(data$year),
                         Month = c(as.integer(data$month)),
                         MonthName = c(data$month_name),
                         DayOfMonth = c(dayOfMonth),
                         DayOfWeek = c(as.integer(data$dow)),
                         DayOfWeekName = c(data$dow_name),
                         DayOfYear = c(as.integer(data$doy)))
    calendar <- rbind(calendar, newRow)
  }
  
  # Write the date information to a file
  arrow::write_parquet(calendar, here::here("data", "processed", 
                                            "JulianDayNumberData.parquet"))
}
