
# Build master parquet file for Mercury for all time periods in the
# DE441 ephemeris from the individual parquet files

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
    log_info('Reading Mercury parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileMercury <- dplyr::bind_rows(df_list)
  log_info('Merge Mercury parquet files into master')
  
  # Save aggregated data for Mercury
  arrow::write_parquet(masterFileMercury, here("data", "processed", 
                                    "Mercury", "MercuryMasterDE441.parquet"))
  
  logger::log_info('Saving file MercuryMasterDE441.parquet')
}