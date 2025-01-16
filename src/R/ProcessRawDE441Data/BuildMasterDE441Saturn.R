
# Build master parquet file for Saturn for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Saturn <- function()
{
  # Get list of all parquet files for Saturn
  fp <- list.files(here("data", "processed", "Saturn"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Saturn", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileSaturn <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Saturn
  arrow::write_parquet(masterFileSaturn, here("data", "processed", 
                                    "Saturn", "SaturnDE441.parquet"))
}