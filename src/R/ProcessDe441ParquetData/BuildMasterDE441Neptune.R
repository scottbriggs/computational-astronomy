
# Build master parquet file for Neptune for all time periods in the
# DE441 ephemeris from the individual parquet files

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
    log_info('Reading Neptune parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileNeptune <- dplyr::bind_rows(df_list)
  log_info('Merge Neptune parquet files into master')
  
  # Save aggregated data for Neptune
  arrow::write_parquet(masterFileNeptune, here("data", "processed", 
                                    "Neptune", "NeptuneMasterDE441.parquet"))
  
  logger::log_info('Saving file NeptuneMasterDE441.parquet')
}