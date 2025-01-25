# Extract data for Mercury from the DE441 ephemeris and store the data
# in parquet file format. Pass the file name, block size, and file length

ProcessDE441Mercury <- function(filename, fileBlocks, fileLength)
{
  log_info('Process data for Mercury')
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
  
  # Column names for Mercury, which has 4 intervals and 14 coefficients for X, Y, and Z
  # Number of columns = # of coefficients * 3 (x, y, z) + 3 (julian day start,
  # julian day end, and interval)
  # Number of rows = number of blocks * the number of intervals
  numColumnsMercury = DE441NUMCOEFFMERCURY*3+3
  numRowsMercury = fileBlocks * DE441NUMSUBINTMERCURY
  mercury_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                         "X10", "X11", "X12", "X13", "X14", "Y1", "Y2", "Y3", "Y4",
                         "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                         "Y14", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                         "Z9", "Z10", "Z11", "Z12", "Z13", "Z14")
  mercury_data = matrix(0.0,nrow=numRowsMercury,ncol=numColumnsMercury)
  colnames(mercury_data) <- mercury_col_names
  
  # Populate the intervals for Mercury
  for (i in seq(from = 1, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    mercury_data[i, "INTERVAL"] <- 1
    mercury_data[i+1, "INTERVAL"] <- 2
    mercury_data[i+2, "INTERVAL"] <- 3
    mercury_data[i+3, "INTERVAL"] <- 4
  }
  
  # Populate the Julian Days for Mercury
  j <- 1
  for (i in seq(from = 1, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    mercury_data[i, "Julian_Day_Start"] <- vect[j]
    mercury_data[i, "Julian_Day_End"] <- vect[j+1]
    mercury_data[i+1, "Julian_Day_Start"] <- vect[j]
    mercury_data[i+1, "Julian_Day_End"] <- vect[j+1]
    mercury_data[i+2, "Julian_Day_Start"] <- vect[j]
    mercury_data[i+2, "Julian_Day_End"] <- vect[j+1]
    mercury_data[i+3, "Julian_Day_Start"] <- vect[j]
    mercury_data[i+3, "Julian_Day_End"] <- vect[j+1]
    j <- j + 1020
  }
  
  # Populate Mercury subinterval 1
  k <- 0
  for (i in seq(from = 1, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    for (j in seq(from = 3, to = 44, by = 1)){
      mercury_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate Mercury subinterval 2
  k <- 42
  for (i in seq(from = 2, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    for (j in seq(from = 3, to = 44, by = 1)){
      mercury_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate Mercury subinterval 3
  k <- 84
  for (i in seq(from = 3, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    for (j in seq(from = 3, to = 44, by = 1)){
      mercury_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate Mercury subinterval 4
  k <- 126
  for (i in seq(from = 4, to = numRowsMercury, by = DE441NUMSUBINTMERCURY)){
    for (j in seq(from = 3, to = 44, by = 1)){
      mercury_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Create file name to save
  fn <- stringr::str_sub(filename, 1, 9)
  fnn <- paste(sep = "", "Mercury_", fn, "_441", ".parquet")
  
  logger::log_info('Saving filename {fnn}')
  
  #Save Mercury data
  df <- as.data.frame(mercury_data)
  arrow::write_parquet(df, here::here("data", "processed", 
                         "Mercury", fnn))
}