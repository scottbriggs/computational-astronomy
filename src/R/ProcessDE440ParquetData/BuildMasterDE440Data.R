# Build master parquet files for all bodies for all time periods in the
# DE440 ephemeris from the individual parquet files

ProcessDE440ParquetFiles <- function()
{
  # Set up logger
  tmp <- tempfile("ProcessDE440ParquetData", here("log"), fileext = ".txt")
  log_appender(appender_file(tmp))
  
  # Create master parquet file for Mercury
  CreateMasterDE440Mercury()
  CreateMasterDE440Venus()
  CreateMasterDE440EMB()
  CreateMasterDE440Mars()
  CreateMasterDE440Jupiter()
  CreateMasterDE440Saturn()
  CreateMasterDE440Uranus()
  CreateMasterDE440Neptune()
  CreateMasterDE440Pluto()
  CreateMasterDE440Moon()
  CreateMasterDE440Sun()
  CreateMasterDE440Nutation()
}
