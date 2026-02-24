
# Read all DE440 raw files and write a file that lists the files and file
# sizes for processing

CreateDE440FileList <- function()
{
  # Read DE440 file list
  de440Files <- list.files(path = here::here("data", "raw", "de440"), pattern='\\.440')
  
  # Drop files that are not DE440 data files
  de440Files <- de440Files[-c(12, 13)]
  
  # How many DE440 files
  numDE440Files <- length(de440Files)
  
  # Read the DE440 files and determine number of lines in each file
  de440FileLength <- rep(0, numDE440Files)
  for (i in 1:numDE440Files) {
    ascii_data <- readLines(here::here("data", "raw", "de440", de440Files[i]), n=-1)
    de440FileLength[i] <- length(ascii_data)
  }
  
  # Equate file size with number of blocks in each file
  de440Blocks <- rep(1142, numDE440Files)
  de440Blocks[3]<- 1143
  de440Blocks[6]<- 1143
  de440Blocks[8]<- 1143
  de440Blocks[11]<- 1143
  
  # Create data frame for the files and file sizes
  tmp <- data.frame(de440Files, de440Blocks, de440FileLength)
  
  # Write data to a parquet file
  parquet_file <- arrow::write_parquet(tmp, here::here("data", "processed", "de440", "de440ASCIIFiles.parquet"))
  
  #Create database table for the DE440 files
  con <- dbConnect(duckdb(), dbdir=here::here("data", "database", "de440.duckdb"))
  
  # Write data for de440 ASCII Files
  table_name <- "DE440Files"
  dbWriteTable(con, table_name, parquet_file)
  
  # Shutdown database
  dbDisconnect(con, shutdown = TRUE)
}  




