# Extract data for Nutation from the DE441 ephemeris and store the data
# in parquet file format. Pass the file name, block size, and file length

ProcessDE441Nutation <- function(filename, fileBlocks, fileLength)
{
  log_info('Process data for the Nutation')
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
  
  # Column names for Nutations, which has 10 Coefficients for Longitude and Obliquity
  # Number of columns = # of coefficients * 2 (Longitude, Obliquity) + 3 (julian day start,
  # julian day end, and interval)
  # Number of rows = number of blocks * the number of intervals
  numColumnsNutation = DE441NUMCOEFFNUTATION*2+3
  numRowsNutation = fileBlocks * DE441NUMSUBINTNUTATION
  nutation_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                          "Longitude1", "Longitude2", "Longitude3", "Longitude4",
                          "Longitude5", "Longitude6", "Longitude7", "Longitude8",
                          "Longitude9", "Longitude10", "Obliquity1", "Obliquity2",
                          "Obliquity3", "Obliquity4","Obliquity5", "Obliquity6",
                          "Obliquity7", "Obliquity8", "Obliquity9", "Obliquity10")
  nutation_data = matrix(0.0,nrow=numRowsNutation,ncol=numColumnsNutation)
  colnames(nutation_data) <- nutation_col_names
  
  # Populate the intervals for the Nutations
  for (i in seq(from = 1, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    nutation_data[i, "INTERVAL"] <- 1
    nutation_data[i+1, "INTERVAL"] <- 2
    nutation_data[i+2, "INTERVAL"] <- 3
    nutation_data[i+3, "INTERVAL"] <- 4
  }
  
  # Populate the Julian Days for the Nutations
  j <- 1
  for (i in seq(from = 1, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    nutation_data[i, "Julian_Day_Start"] <- vect[j]
    nutation_data[i, "Julian_Day_End"] <- vect[j+1]
    nutation_data[i+1, "Julian_Day_Start"] <- vect[j]
    nutation_data[i+1, "Julian_Day_End"] <- vect[j+1]
    nutation_data[i+2, "Julian_Day_Start"] <- vect[j]
    nutation_data[i+2, "Julian_Day_End"] <- vect[j+1]
    nutation_data[i+3, "Julian_Day_Start"] <- vect[j]
    nutation_data[i+3, "Julian_Day_End"] <- vect[j+1]
    j <- j + 1020
  }
  
  # Populate the Nutations subinterval 1
  k <- 816
  for (i in seq(from = 1, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    for (j in seq(from = 3, to = 22, by = 1)){
      nutation_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Nutations subinterval 2
  k <- 836
  for (i in seq(from = 2, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    for (j in seq(from = 3, to = 22, by = 1)){
      nutation_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Nutations subinterval 3
  k <- 856
  for (i in seq(from = 3, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    for (j in seq(from = 3, to = 22, by = 1)){
      nutation_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Populate the Nutations subinterval 4
  k <- 876
  for (i in seq(from = 4, to = numRowsNutation, by = DE441NUMSUBINTNUTATION)){
    for (j in seq(from = 3, to = 22, by = 1)){
      nutation_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Create file name to save
  fn <- stringr::str_sub(filename, 1, 9)
  fnn <- paste(sep = "", "Nutation_", fn, "_441", ".parquet")
  
  log_info('Saving filename {fnn}')
  
  #Save Nutation data
  df <- as.data.frame(nutation_data)
  arrow::write_parquet(df, here("data", "processed", 
                         "Nutation", fnn))
}