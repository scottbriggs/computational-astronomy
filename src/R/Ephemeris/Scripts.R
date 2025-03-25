
# Libraries used by the various functions
library(logger)
library(arrow)
library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(duckdb)
library(tidygeocoder)
library(lutz)
library(lubridate)
library(kableExtra)
library(magrittr)

# Mathematical and ephemeris constants used by the various functions
source(here("src", "R", "Ephemeris", "constants.R"))

# Functions used to process the DE441 ephemeris raw files
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441FileList.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441RawData.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Mercury.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Venus.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441EMB.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Mars.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Jupiter.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Saturn.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Uranus.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Neptune.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Pluto.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Sun.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Moon.R"))
source(here("src", "R", "ProcessRawDE441Data", "ProcessDE441Nutation.R"))

# Functions used to convert the DE441 individual parquet files for each raw file
# into a master parquet file for each solar system body
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Data.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Mercury.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Venus.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441EMB.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Mars.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Jupiter.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Saturn.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Uranus.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Neptune.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Pluto.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Moon.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Sun.R"))
source(here("src", "R", "ProcessDE441ParquetData", "BuildMasterDE441Nutation.R"))

# Function to used the NASA API calculate the julian day number
source(here("src", "R", "Ephemeris", "CreateJulianDayNumberTable.R"))

# Functions to calculate dates
source(here("src", "R", "Ephemeris", "Date.R"))

# Functions to convert between hours and degrees
source(here("src", "R", "Ephemeris", "UnitConversions.R"))

# Function to populate a duckdb database with DE441 and DE440 data
source(here("src", "R", "CreateDatabase", "CreateDE441DE440Database.R"))

# Functions to calculate solar system position and velocity vectors using
# the DE441 data
source(here("src", "R", "DE441SolarSystemPosVel", "PositionMercurySSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionVenusSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionEarthSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionMarsSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionJupiterSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionSaturnSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionUranusSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionNeptuneSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionPlutoSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionMoonGEO.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "PositionSunSSB.R"))
source(here("src", "R", "DE441SolarSystemPosVel", "Nutation.R"))


# Functions used to process the DE440 ephemeris raw files
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440RawData.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Mercury.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Venus.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440EMB.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Mars.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Jupiter.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Saturn.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Uranus.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Neptune.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Pluto.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Sun.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Moon.R"))
source(here("src", "R", "ProcessRawDE440Data", "ProcessDE440Nutation.R"))

# Functions used to convert the DE440 individual parquet files for each raw file
# into a master parquet file for each solar system body
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Data.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Mercury.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Venus.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440EMB.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Mars.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Jupiter.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Saturn.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Uranus.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Neptune.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Pluto.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Moon.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Sun.R"))
source(here("src", "R", "ProcessDE440ParquetData", "BuildMasterDE440Nutation.R"))

# Functions for various math operations
source(here("src", "R", "Ephemeris", "MathConversions.R"))

# Functions to calculate precession
source(here("src", "R", "Ephemeris", "Precession.R"))

# Functions to calculate the apparent place of solar system bodies
source(here("src", "R", "Ephemeris", "ApparentPlace.R"))

# Function to calculate the obliquity of the ecliptic
source(here("src", "R", "Ephemeris", "Obliquity.R"))

# Functions to calculate time-related quantities
source(here("src", "R", "Ephemeris", "Time.R"))

# Functions to calculate coordinate transformations
source(here("src", "R", "Ephemeris", "Coordinate.R"))

