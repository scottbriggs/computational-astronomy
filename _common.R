
# Use here for relative file paths
library(here)
library(DBI)
library(duckdb)

# Set options for creating docs
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

# Set the number of digits to appear in output
options(
  digits=15
  )