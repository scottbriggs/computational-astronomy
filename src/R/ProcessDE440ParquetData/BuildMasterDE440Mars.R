
# Build master parquet file for Mars for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Mars <- function()
{
  # Get list of all parquet files for Mars
  fp <- list.files(here("data", "processed", "de440", "Mars"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Mars", fp[[i]]))
    log_info('Reading Mars parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileMars <- dplyr::bind_rows(df_list)
  log_info('Merge Mars parquet files into master')
  
  # Save aggregated data for Mars
  arrow::write_parquet(masterFileMars, here("data", "processed", "de440",
                                    "Mars", "MarsMasterDE440.parquet"))
  
  logger::log_info('Saving file MarsMasterDE440.parquet')
}