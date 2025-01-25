# Extract data for the Moon from the DE441 ephemeris and store the data
# in parquet file format. Pass the file name, block size, and file length

ProcessDE441Moon <- function(filename, fileBlocks, fileLength)
{
  log_info('Process data for the Moon')
  log_info('Reading filename {filename}')
  log_info('Number of Blocks = {fileBlocks}')
  log_info('File Length = {fileLength}')
  
  ascii_data <- readLines(here("data", "raw", "de441", filename))
  
  # Create vector to store the ascii data sequentially based on the block size
  # Size of the vector is the block size * 341 lines in each block *
  # 3 data elements in each line
  vect <- rep(0, fileBlocks * 341 * 3)
  
  # i indexes the result vector, which is dependent on the number of blocks
  # in the file
  # j indexes the rows of data in each block
  # k indexes the 3 data elements in each row for each block
  k <- 1
  for (i in seq(from = 2, to = fileLength + 341, by = 341)){
    for (j in seq(from = 0, to = 339, by = 1)){
      
      # First data element in the row
      tmpstr1 <- as.character(substr(ascii_data[j+i], 4, 26))
      tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
      tmpstr1 <- as.numeric(tmpstr1)
      vect[k] <- tmpstr1
      k <- k + 1
      
      # Second data element in the row
      tmpstr1 <- as.character(substr(ascii_data[j+i], 30, 52))
      tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
      tmpstr1 <- as.numeric(tmpstr1)
      vect[k] <- tmpstr1
      k <- k + 1
      
      # Third data element in the row
      tmpstr1 <- as.character(substr(ascii_data[j+i], 56, 78))
      tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
      tmpstr1 <- as.numeric(tmpstr1)
      vect[k] <- tmpstr1
      k <- k + 1
    }
  }
  
  # Column names for Moon, which has 13 coefficients for X, Y, and Z
  # Number of columns = # of coefficients * 3 (x, y, z) + 3 (julian day start,
  # julian day end, and interval)
  # Number of rows = number of blocks * the number of intervals
  numColumnsMoon = DE441NUMCOEFFMOON*3+3
  numRowsMoon = fileBlocks * DE441NUMSUBINTMOON
  moon_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                      "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                      "X10", "X11", "X12", "X13", "Y1", "Y2", "Y3", "Y4",
                      "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                      "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                      "Z9", "Z10", "Z11", "Z12", "Z13")
  moon_data = matrix(0.0,nrow=numRowsMoon, numColumnsMoon)
  colnames(moon_data) <- moon_col_names
  
  # Populate the intervals for the Moon
  for (i in seq(from = 1, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    moon_data[i, "INTERVAL"] <- 1
    moon_data[i+1, "INTERVAL"] <- 2
    moon_data[i+2, "INTERVAL"] <- 3
    moon_data[i+3, "INTERVAL"] <- 4
    moon_data[i+4, "INTERVAL"] <- 5
    moon_data[i+5, "INTERVAL"] <- 6
    moon_data[i+6, "INTERVAL"] <- 7
    moon_data[i+7, "INTERVAL"] <- 8
  }
  
  # Populate the Julian Days for the Moon
  j <- 1
  for (i in seq(from = 1, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    moon_data[i, "Julian_Day_Start"] <- vect[j]
    moon_data[i, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+1, "Julian_Day_Start"] <- vect[j]
    moon_data[i+1, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+2, "Julian_Day_Start"] <- vect[j]
    moon_data[i+2, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+3, "Julian_Day_Start"] <- vect[j]
    moon_data[i+3, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+4, "Julian_Day_Start"] <- vect[j]
    moon_data[i+4, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+5, "Julian_Day_Start"] <- vect[j]
    moon_data[i+5, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+6, "Julian_Day_Start"] <- vect[j]
    moon_data[i+6, "Julian_Day_End"] <- vect[j+1]
    moon_data[i+7, "Julian_Day_Start"] <- vect[j]
    moon_data[i+7, "Julian_Day_End"] <- vect[j+1]
    j <- j + 1020
  }
  
  # Populate the Moon subinterval 1
  k <- 438
  for (i in seq(from = 1, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 2
  k <- 477
  for (i in seq(from = 2, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 3
  k <- 516
  for (i in seq(from = 3, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 4
  k <- 555
  for (i in seq(from = 4, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 5
  k <- 594
  for (i in seq(from = 5, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 6
  k <- 633
  for (i in seq(from = 6, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 7
  k <- 672
  for (i in seq(from = 7, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Moon subinterval 8
  k <- 711
  for (i in seq(from = 8, to = numRowsMoon, by = DE441NUMSUBINTMOON)){
    for (j in seq(from = 3, to = 41, by = 1)){
      moon_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Create file name to save
  fn <- stringr::str_sub(filename, 1, 9)
  fnn <- paste(sep = "", "Moon_", fn, "_441", ".parquet")
  
  log_info('Saving filename {fnn}')
  
  #Save Moon data
  df <- as.data.frame(moon_data)
  arrow::write_parquet(df, here("data", "processed", 
                         "Moon", fnn))
}