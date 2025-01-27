
# Build master parquet file for Pluto for all time periods in the
# DE441 ephemeris from the individual parquet files

CreateMasterDE441Pluto <- function()
{
  # Get list of all parquet files for Pluto
  fp <- list.files(here("data", "processed", "Pluto"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Pluto", fp[[i]]))
    log_info('Reading Pluto parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFilePluto <- dplyr::bind_rows(df_list)
  log_info('Merge Pluto parquet files into master')
  
  # Save aggregated data for Pluto
  arrow::write_parquet(masterFilePluto, here("data", "processed", 
                                    "Pluto", "PlutoMasterDE441.parquet"))
  
  logger::log_info('Saving file PlutoMasterDE441.parquet')
}