
# Build master parquet file for Sun for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Sun <- function()
{
  # Get list of all parquet files for Sun
  fp <- list.files(here("data", "processed", "de440", "Sun"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Sun", fp[[i]]))
    log_info('Reading Sun parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileSun <- dplyr::bind_rows(df_list)
  log_info('Merge Sun parquet files into master')
  
  # Save aggregated data for Sun
  arrow::write_parquet(masterFileSun, here("data", "processed", "de440",
                                    "Sun", "SunMasterDE440.parquet"))
  
  logger::log_info('Saving file SunMasterDE440.parquet')
}