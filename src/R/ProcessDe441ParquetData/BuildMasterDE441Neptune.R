
# Build master parquet file for Neptune for all time periods in the
# DE441 ephemeris from the individual parquet files

library(here)
library(arrow)
library(dplyr)

CreateMasterDE441Neptune <- function()
{
  # Get list of all parquet files for Neptune
  fp <- list.files(here("data", "processed", "Neptune"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Neptune", fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFileNeptune <- dplyr::bind_rows(df_list)
  
  # Save aggregated data for Neptune
  arrow::write_parquet(masterFileNeptune, here("data", "processed", 
                                    "Neptune", "NeptuneDE441.parquet"))
}