
# Build master parquet file for Nutation for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Nutation <- function()
{
  # Get list of all parquet files for Nutation
  fp <- list.files(here("data", "processed", "Nutation"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Nutation", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileNutation <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Nutation
  arrow::write_parquet(masterFileNutation, here("data", "processed", 
                                    "Nutation", "NutationDE441.parquet"))
}