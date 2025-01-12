
# Read all DE441 raw files and write a doc that lists the files and file
# sizes for processing

library(readxl)
library(writexl)

# Set working directory and read DE441 file list
de441Files <- list.files(pattern='\\.441')

# Drop files that are not DE441 data files
de441Files <- de441Files[-c(31, 32)]

# How many DE441 files
numDE441Files <- length(de441Files)

# Read the DE441 files and determine number of lines in each file
de441FileLength <- rep(numDE441Files, 0)
for (i in 1:numDE441Files) {
  ascii_data <- readLines(de441Files[i], n=-1)
  de441FileLength[i] <- length(ascii_data)
}

# Create data frame for the files and file sizes
tmp <- data.frame(de441Files, de441FileLength)

# Write the files and file sizes to an excel file
write_xlsx(tmp, '/Users/scottbriggs/Documents/GitHub/computational-astronomy/data/raw/de441/DE441FileSize.xlsx', col_names = TRUE)
