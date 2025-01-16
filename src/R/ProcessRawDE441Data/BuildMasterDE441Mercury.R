
# Build master parquet file for Mercury for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Mercury <- function()
{
  # Get list of all parquet files for Mercury
  fp <- list.files(here("data", "processed", "Mercury"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Mercury", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileMercury <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Mercury
  arrow::write_parquet(masterFileMercury, here::here("data", "processed", 
                                    "Mercury", "MercuryDE441.parquet"))
}