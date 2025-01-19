
# Build master parquet file for Moon for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Moon <- function()
{
  # Get list of all parquet files for Moon
  fp <- list.files(here("data", "processed", "Moon"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Moon", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileMoon <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Moon
  arrow::write_parquet(masterFileMoon, here("data", "processed", 
                                    "Moon", "MoonDE441.parquet"))
}