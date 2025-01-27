
# Build master parquet file for Jupiter for all time periods in the
# DE441 ephemeris from the individual parquet files

CreateMasterDE441Jupiter <- function()
{
  # Get list of all parquet files for Jupiter
  fp <- list.files(here("data", "processed", "Jupiter"))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  df_list <- vector(mode = "list", numFiles)
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "Jupiter", fp[[i]]))
    log_info('Reading Jupiter parquet file {fp[[i]]}')
  }
  
  # Combine data frames into a single data frame
  masterFileJupiter <- dplyr::bind_rows(df_list)
  log_info('Merge Jupiter parquet files into master')
  
  # Save aggregated data for Jupiter
  arrow::write_parquet(masterFileJupiter, here("data", "processed", 
                                    "Jupiter", "JupiterMasterDE441.parquet"))
  
  logger::log_info('Saving file JupiterMasterDE441.parquet')
}