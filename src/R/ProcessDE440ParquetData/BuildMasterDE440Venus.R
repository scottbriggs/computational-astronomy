
# Build master parquet file for Venus for all time periods in the
# DE440 ephemeris from the individual parquet files

CreateMasterDE440Venus <- function()
{
  # Get list of all parquet files for Mercury
  fp <- list.files(here("data", "processed", "de440", "Venus"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de440", "Venus", fp[[i]]))
    log_info('Reading Venus parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileVenus <- dplyr::bind_rows(df_list)
  log_info('Merge Venus parquet files into master')
  
  # Save aggregated data for Venus
  arrow::write_parquet(masterFileVenus, here("data", "processed", "de440",
                                    "Venus", "VenusMasterDE440.parquet"))
  
  logger::log_info('Saving file VenusMasterDE440.parquet')
}