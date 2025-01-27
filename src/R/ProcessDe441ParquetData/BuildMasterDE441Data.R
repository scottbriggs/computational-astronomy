# Build master parquet files for all bodies for all time periods in the
# DE441 ephemeris from the individual parquet files

ProcessDE441ParquetFiles <- function()
{
  # Set up logger
  tmp <- tempfile("ProcessDE441ParquetData", here("log"), fileext = ".txt")
  log_appender(appender_file(tmp))
  
  # Create master parquet file for Mercury
  CreateMasterDE441Mercury()
  CreateMasterDE441Venus()
  CreateMasterDE441EMB()
  CreateMasterDE441Mars()
  CreateMasterDE441Jupiter()
  CreateMasterDE441Saturn()
  CreateMasterDE441Uranus()
  CreateMasterDE441Neptune()
  CreateMasterDE441Pluto()
  CreateMasterDE441Moon()
  CreateMasterDE441Sun()
  CreateMasterDE441Nutation()
}
