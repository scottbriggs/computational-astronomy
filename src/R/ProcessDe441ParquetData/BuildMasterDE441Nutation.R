
# Build master parquet file for Nutation for all time periods in the
# DE441 ephemeris from the individual parquet files

CreateMasterDE441Nutation <- function()
{
  # Get list of all parquet files for Nutation
  fp <- list.files(here("data", "processed", "Nutation"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Nutation", fp[[i]]))
    log_info('Reading Nutation parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileNutation <- dplyr::bind_rows(df_list)
  log_info('Merge Nutation parquet files into master')
  
  # Save aggregated data for Nutation
  arrow::write_parquet(masterFileNutation, here("data", "processed", 
                                    "Nutation", "NutationMasterDE441.parquet"))
  
  logger::log_info('Saving file NutationMasterDE441.parquet')
}