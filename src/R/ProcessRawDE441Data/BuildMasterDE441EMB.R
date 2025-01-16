
# Build master parquet file for the earth-moon barycenter for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441EMB <- function()
{
  # Get list of all parquet files for EMB
  fp <- list.files(here("data", "processed", "EMB"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "EMB", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileEMB <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for EMB
  arrow::write_parquet(masterFileEMB, here::here("data", "processed", 
                                    "EMB", "EMBDE441.parquet"))
}