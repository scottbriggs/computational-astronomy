# Extract data for Uranus from the DE440 ephemeris and store the data
# in parquet file format. Pass the file name, block size, and file length

ProcessDE440Uranus <- function(filename, fileBlocks, fileLength)
{
  log_info('Process data for Uranus')
  log_info('Reading filename {filename}')
  log_info('Number of Blocks = {fileBlocks}')
  log_info('File Length = {fileLength}')
  
  ascii_data <- readLines(here("data", "raw", "de440", filename))
  
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
  
  # Column names for Uranus, which has 6 coefficients for X, Y, and Z
  # Number of columns = # of coefficients * 3 (x, y, z) + 3 (julian day start,
  # julian day end, and interval)
  # Number of rows = number of blocks * the number of intervals
  numColumnsUranus = DE440NUMCOEFFURANUS*3+3
  numRowsUranus = fileBlocks * DE440NUMSUBINTURANUS
  uranus_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                        "X1", "X2", "X3", "X4", "X5", "X6",
                        "Y1", "Y2", "Y3", "Y4", "Y5", "Y6",
                        "Z1", "Z2", "Z3", "Z4", "Z5", "Z6")
  uranus_data = matrix(0.0,nrow=numRowsUranus, numColumnsUranus)
  colnames(uranus_data) <- uranus_col_names
  
  # Populate the intervals for Uranus
  for (i in seq(from = 1, to = numRowsUranus, by = DE440NUMSUBINTURANUS)){
    uranus_data[i, "INTERVAL"] <- 1
  }
  
  # Populate the Julian Days for Uranus
  j <- 1
  for (i in seq(from = 1, to = numRowsUranus, by = DE440NUMSUBINTURANUS)){
    uranus_data[i, "Julian_Day_Start"] <- vect[j]
    uranus_data[i, "Julian_Day_End"] <- vect[j+1]
    j <- j + 1020
  }
  
  # Populate Uranus subinterval 1
  k <- 384
  for (i in seq(from = 1, to = numRowsUranus, by = DE440NUMSUBINTURANUS)){
    for (j in seq(from = 3, to = 20, by = 1)){
      uranus_data[i, j+1] <- vect[j+k]
    }
    k <- k + 1020
  }
  
  # Create file name to save
  fn <- stringr::str_sub(filename, 1, 9)
  fnn <- paste(sep = "", "Uranus_", fn, "_440", ".parquet")
  
  log_info('Saving filename {fnn}')
  
  #Save Uranus data
  df <- as.data.frame(uranus_data)
  arrow::write_parquet(df, here("data", "processed", 
                         "Uranus", fnn))
}