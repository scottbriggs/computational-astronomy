
# Build master parquet file for EMB for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440EMB <- function()
{
  # Get list of all parquet files for EMB
  fp <- list.files(here("data", "processed", "de440", "EMB"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "EMB", fp[[i]]))
    log_info('Reading EMB parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileEMB <- dplyr::bind_rows(df_list)
  log_info('Merge EMB parquet files into master')
  
  # Save aggregated data for EMB
  arrow::write_parquet(masterFileEMB, here("data", "processed", "de440",
                                    "EMB", "EMBMasterDE440.parquet"))
  
  logger::log_info('Saving file EMBMasterDE440.parquet')
}