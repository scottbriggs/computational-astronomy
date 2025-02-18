
# Build master parquet file for Pluto for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Pluto <- function()
{
  # Get list of all parquet files for Pluto
  fp <- list.files(here("data", "processed", "de440", "Pluto"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Pluto", fp[[i]]))
    log_info('Reading Pluto parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFilePluto <- dplyr::bind_rows(df_list)
  log_info('Merge Pluto parquet files into master')
  
  # Save aggregated data for Pluto
  arrow::write_parquet(masterFilePluto, here("data", "processed", "de440",
                                    "Pluto", "PlutoMasterDE440.parquet"))
  
  logger::log_info('Saving file PlutoMasterDE440.parquet')
}