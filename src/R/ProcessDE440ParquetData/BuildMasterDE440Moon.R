
# Build master parquet file for Moon for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Moon <- function()
{
  # Get list of all parquet files for Moon
  fp <- list.files(here("data", "processed", "de440", "Moon"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Moon", fp[[i]]))
    log_info('Reading Moon parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileMoon <- dplyr::bind_rows(df_list)
  log_info('Merge Moon parquet files into master')
  
  # Save aggregated data for Moon
  arrow::write_parquet(masterFileMoon, here("data", "processed", "de440",
                                    "Moon", "MoonMasterDE440.parquet"))
  
  logger::log_info('Saving file MoonMasterDE440.parquet')
}