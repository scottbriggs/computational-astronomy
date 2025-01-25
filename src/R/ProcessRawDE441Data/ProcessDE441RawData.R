
# Process all the DE441 files and write parquet files for each body

ProcessDE441RawFiles <- function()
{
  # Create DE441 file list
  ProcessDE441FileList.R
  
  # Read DE441 file list
  DE441FileList <- read_excel(here("data", "raw", "de441", "DE441FileSize.xlsx"))
  
  # Set up logger
  tmp <- tempfile("ProcessRawDE441Data", here("log"), fileext = ".txt")
  log_appender(appender_file(tmp))
  
  # Process data for Venus
  ProcessDE441Mercury(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                      as.integer(DE441FileList[1,3]))
  
  ProcessDE441Venus(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                    as.integer(DE441FileList[1,3]))
  
  ProcessDE441EMB(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                    as.integer(DE441FileList[1,3]))
  
  ProcessDE441Mars(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                  as.integer(DE441FileList[1,3]))
  
  ProcessDE441Jupiter(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                   as.integer(DE441FileList[1,3]))
  
  ProcessDE441Saturn(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                      as.integer(DE441FileList[1,3]))
  
  ProcessDE441Uranus(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                     as.integer(DE441FileList[1,3]))
  
  ProcessDE441Neptune(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                     as.integer(DE441FileList[1,3]))
  
  ProcessDE441Pluto(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                      as.integer(DE441FileList[1,3]))
  
  ProcessDE441Sun(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                    as.integer(DE441FileList[1,3]))
  
  ProcessDE441Moon(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                  as.integer(DE441FileList[1,3]))
  
  ProcessDE441Nutation(DE441FileList[1,1], as.integer(DE441FileList[1,2]), 
                   as.integer(DE441FileList[1,3]))
}


