
# Build master parquet file for Uranus for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Uranus <- function()
{
  # Get list of all parquet files for Uranus
  fp <- list.files(here("data", "processed", "de440", "Uranus"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Uranus", fp[[i]]))
    log_info('Reading Uranus parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileUranus <- dplyr::bind_rows(df_list)
  log_info('Merge Uranus parquet files into master')
  
  # Save aggregated data for Uranus
  arrow::write_parquet(masterFileUranus, here("data", "processed", "de440",
                                    "Uranus", "UranusMasterDE440.parquet"))
  
  logger::log_info('Saving file UranusMasterDE440.parquet')
}