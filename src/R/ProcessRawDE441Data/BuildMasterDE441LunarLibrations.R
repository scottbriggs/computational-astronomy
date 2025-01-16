
# Build master parquet file for Lunar Librations for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441LunarLibrations <- function()
{
  # Get list of all parquet files for Lunar Librations
  fp <- list.files(here("data", "processed", "LunarLibrations"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "LunarLibrations", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileLunarLibrations <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Lunar Librations
  arrow::write_parquet(masterFileNutation, here("data", "processed", 
                                    "LunarLibrations", "LunarLibrationsDE441.parquet"))
}