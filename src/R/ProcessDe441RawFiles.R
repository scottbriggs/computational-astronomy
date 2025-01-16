
library(tidyverse)
library(readxl)

# Set working directory and read in DE441 file names and file sizes
DE441Files <- read_excel("~/Documents/GitHub/computational-astronomy/data/raw/de441/DE441FileSize.xlsx")

# Read DE441 file
ascii_data <- readLines(DE441Files[[1]][1])

# Loop through each block and transform the data in each block
k <- 1
numBlocks <- 1
block <- matrix(0.0, nrow = 1020, ncol = DE441Files[[2]][1])
for (j in seq(from = 2, to = DE441Files[[3]][1], by = 341)) {
for (i in seq(from = 0, to = 339, by = 1)) {
  
  # First data element in the row
  tmpstr1 <- as.character(substr(ascii_data[i+j], 4, 26))
  tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
  tmpstr1 <- as.numeric(tmpstr1)
  block[numBlocks][k] <- tmpstr1
  k <- k + 1
  
  # Second data element in the row
  tmpstr1 <- as.character(substr(ascii_data[i+j], 30, 52))
  tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
  tmpstr1 <- as.numeric(tmpstr1)
  block[numBlocks][k] <- tmpstr1
  k <- k + 1
  
  # Third data element in the row
  tmpstr1 <- as.character(substr(ascii_data[i+j], 56, 78))
  tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
  tmpstr1 <- as.numeric(tmpstr1)
  block[numBlocks][k] <- tmpstr1
  k <- k + 1
}
  numBlocks <- numBlocks + 1
  k <- 1
}







