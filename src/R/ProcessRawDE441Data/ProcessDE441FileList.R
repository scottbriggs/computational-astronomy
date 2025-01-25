
# Read all DE441 raw files and write a file that lists the files and file
# sizes for processing

CreateDE441FileList <- function()
{
  # Read DE441 file list
  de441Files <- list.files(path = here("data", "raw", "de441"), pattern='\\.441')
  
  # Drop files that are not DE441 data files
  de441Files <- de441Files[-c(31, 32)]
  
  # How many DE441 files
  numDE441Files <- length(de441Files)
  
  # Read the DE441 files and determine number of lines in each file
  de441FileLength <- rep(0, numDE441Files)
  for (i in 1:numDE441Files) {
    ascii_data <- readLines(de441Files[i], n=-1)
    de441FileLength[i] <- length(ascii_data)
  }
  
  # Equate file size with number of blocks in each file
  de441Blocks <- rep(11415, numDE441Files)
  de441Blocks[9]<- 11416
  de441Blocks[17]<- 11414
  de441Blocks[23]<- 11414
  de441Blocks[29]<- 11414
  
  # Create data frame for the files and file sizes
  tmp <- data.frame(de441Files, de441Blocks, de441FileLength)
  
  # Write the files and file sizes to an excel file
  write_xlsx(tmp, here("data", "raw", "de441", "DE441FileSize.xlsx"), col_names = TRUE)
}




