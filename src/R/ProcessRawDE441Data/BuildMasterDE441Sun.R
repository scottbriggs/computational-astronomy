
# Build master parquet file for Sun for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Sun <- function()
{
  # Get list of all parquet files for Sun
  fp <- list.files(here("data", "processed", "Sun"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Sun", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileSun <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Moon
  arrow::write_parquet(masterFileSun, here("data", "processed", 
                                    "Sun", "SunDE441.parquet"))
}