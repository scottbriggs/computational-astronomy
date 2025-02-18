
# Process all the DE440 files and write parquet files for each body

ProcessDE440RawFiles <- function()
{
  
  # Read DE440 file list
  DE440FileList <- read_excel(here("data", "raw", "de440", "DE440FileSize.xlsx"))
  
  # Set up logger
  tmp <- tempfile("ProcessRawDE440Data", here("log"), fileext = ".txt")
  log_appender(appender_file(tmp))
  
  numFiles <- nrow(DE440FileList)
  
  for (i in seq(from = 1, to = numFiles, by = 1)){
  
    ProcessDE440Mercury(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                        as.integer(DE440FileList[i,3]))
  
    ProcessDE440Venus(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                      as.integer(DE440FileList[i,3]))
  
    ProcessDE440EMB(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                      as.integer(DE440FileList[i,3]))
  
    ProcessDE440Mars(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                    as.integer(DE440FileList[i,3]))
  
    ProcessDE440Jupiter(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                     as.integer(DE440FileList[i,3]))
    
    ProcessDE440Saturn(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                        as.integer(DE440FileList[i,3]))
    
    ProcessDE440Uranus(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                       as.integer(DE440FileList[i,3]))
    
    ProcessDE440Neptune(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                       as.integer(DE440FileList[i,3]))
    
    ProcessDE440Pluto(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                        as.integer(DE440FileList[i,3]))
    
    ProcessDE440Sun(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                      as.integer(DE440FileList[i,3]))
    
    ProcessDE440Moon(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                    as.integer(DE440FileList[i,3]))
    
    ProcessDE440Nutation(DE440FileList[i,1], as.integer(DE440FileList[i,2]), 
                     as.integer(DE440FileList[i,3]))
  }
}


