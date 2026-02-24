
# Use here for relative file paths
library(here)
library(logger)
library(arrow)
library(writexl)
library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(httr)
library(jsonlite)
library(duckdb)
library(tidygeocoder)
library(lutz)
library(lubridate)
library(kableExtra)
library(magrittr)
library(data.table)
library(purrr)
library(readr)

# Source all functions
#source(here::here("src", "R", "Ephemeris", "Scripts.R"))
source("src/R/Ephemeris/Scripts.R")

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