
# Use here for relative file paths
library(here)
library(arrow)
library(duckdb)
library(DBI)
library(dplyr)
library(stringi)
library(stringr)
library(data.table)

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