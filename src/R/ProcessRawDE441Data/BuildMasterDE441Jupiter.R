
# Build master parquet file for Jupiter for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Jupiter <- function()
{
  # Get list of all parquet files for Jupiter
  fp <- list.files(here("data", "processed", "Jupiter"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Jupiter", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileJupiter <- dplyr::bind_rows(df_list)
  
  # Save aggregated data Jupiter
  arrow::write_parquet(masterFileJupiter, here("data", "processed", 
                                    "Jupiter", "JupiterDE441.parquet"))
}