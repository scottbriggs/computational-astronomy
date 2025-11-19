
CreateDE441DE440Database <- function()
{
  #Create database for the DE441 data
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441_de440.duckdb")))
  
  # Add tables for the DE441 solar system bodies
  # Read data for Mercury
  mercury <- arrow::read_parquet(here("data", "processed", "de441", "Mercury", "MercuryMasterDE441.parquet"))
  
  # Write data for Mercury
  table_name <- "DE441Mercury"
  dbWriteTable(con, table_name, mercury)
  
  # Read data for Venus
  venus <- arrow::read_parquet(here("data", "processed","de441", "Venus", "VenusMasterDE441.parquet"))
  
  # Write data for Venus
  table_name <- "DE441Venus"
  dbWriteTable(con, table_name, venus)
  
  # Read data for EMB
  emb <- arrow::read_parquet(here("data", "processed", "de441", "EMB", "EMBMasterDE441.parquet"))
  
  # Write data for EMB
  table_name <- "DE441EMB"
  dbWriteTable(con, table_name, emb)
  
  # Read data for Mars
  mars <- arrow::read_parquet(here("data", "processed", "de441", "Mars", "MarsMasterDE441.parquet"))
  
  # Write data for Mars
  table_name <- "DE441Mars"
  dbWriteTable(con, table_name, mars)
  
  # Read data for Jupiter
  jupiter <- arrow::read_parquet(here("data", "processed", "de441", "Jupiter", "JupiterMasterDE441.parquet"))
  
  # Write data for Jupiter
  table_name <- "DE441Jupiter"
  dbWriteTable(con, table_name, jupiter)
  
  # Read data for Saturn
  saturn <- arrow::read_parquet(here("data", "processed", "de441", "Saturn", "SaturnMasterDE441.parquet"))
  
  # Write data for Saturn
  table_name <- "DE441Saturn"
  dbWriteTable(con, table_name, saturn)
  
  # Read data for Uranus
  uranus <- arrow::read_parquet(here("data", "processed", "de441", "Uranus", "UranusMasterDE441.parquet"))
  
  # Write data for Uranus
  table_name <- "DE441Uranus"
  dbWriteTable(con, table_name, uranus)
  
  # Read data for Neptune
  neptune <- arrow::read_parquet(here("data", "processed", "de441", "Neptune", "NeptuneMasterDE441.parquet"))
  
  # Write data for Neptune
  table_name <- "DE441Neptune"
  dbWriteTable(con, table_name, neptune)
  
  # Read data for Pluto
  pluto <- arrow::read_parquet(here("data", "processed", "de441", "Pluto", "PlutoMasterDE441.parquet"))
  
  # Write data for Pluto
  table_name <- "DE441Pluto"
  dbWriteTable(con, table_name, pluto)
  
  # Read data for the Moon
  moon <- arrow::read_parquet(here("data", "processed", "de441", "Moon", "MoonMasterDE441.parquet"))
  
  # Write data for the Moon
  table_name <- "DE441Moon"
  dbWriteTable(con, table_name, moon)
  
  # Read data for the Sun
  sun <- arrow::read_parquet(here("data", "processed", "de441", "Sun", "SunMasterDE441.parquet"))
  
  # Write data for the Sun
  table_name <- "DE441Sun"
  dbWriteTable(con, table_name, sun)
  
  # Read data for the Nutation
  nutation <- arrow::read_parquet(here("data", "processed", "de441", "Nutation", "NutationMasterDE441.parquet"))
  
  # Write data for the Nutation
  table_name <- "DE441Nutation"
  dbWriteTable(con, table_name, nutation)
  
  # DE440
  
  # Add tables for the DE440 solar system bodies
  # Read data for Mercury
  mercury <- arrow::read_parquet(here("data", "processed", "de440", "Mercury", "MercuryMasterDE440.parquet"))
  
  # Write data for Mercury
  table_name <- "DE440Mercury"
  dbWriteTable(con, table_name, mercury)
  
  # Read data for Venus
  venus <- arrow::read_parquet(here("data", "processed","de440", "Venus", "VenusMasterDE440.parquet"))
  
  # Write data for Venus
  table_name <- "DE440Venus"
  dbWriteTable(con, table_name, venus)
  
  # Read data for EMB
  emb <- arrow::read_parquet(here("data", "processed", "de440", "EMB", "EMBMasterDE440.parquet"))
  
  # Write data for EMB
  table_name <- "DE440EMB"
  dbWriteTable(con, table_name, emb)
  
  # Read data for Mars
  mars <- arrow::read_parquet(here("data", "processed", "de440", "Mars", "MarsMasterDE440.parquet"))
  
  # Write data for Mars
  table_name <- "DE440Mars"
  dbWriteTable(con, table_name, mars)
  
  # Read data for Jupiter
  jupiter <- arrow::read_parquet(here("data", "processed", "de440", "Jupiter", "JupiterMasterDE440.parquet"))
  
  # Write data for Jupiter
  table_name <- "DE440Jupiter"
  dbWriteTable(con, table_name, jupiter)
  
  # Read data for Saturn
  saturn <- arrow::read_parquet(here("data", "processed", "de440", "Saturn", "SaturnMasterDE440.parquet"))
  
  # Write data for Saturn
  table_name <- "DE440Saturn"
  dbWriteTable(con, table_name, saturn)
  
  # Read data for Uranus
  uranus <- arrow::read_parquet(here("data", "processed", "de440", "Uranus", "UranusMasterDE440.parquet"))
  
  # Write data for Uranus
  table_name <- "DE440Uranus"
  dbWriteTable(con, table_name, uranus)
  
  # Read data for Neptune
  neptune <- arrow::read_parquet(here("data", "processed", "de440", "Neptune", "NeptuneMasterDE440.parquet"))
  
  # Write data for Neptune
  table_name <- "DE440Neptune"
  dbWriteTable(con, table_name, neptune)
  
  # Read data for Pluto
  pluto <- arrow::read_parquet(here("data", "processed", "de440", "Pluto", "PlutoMasterDE440.parquet"))
  
  # Write data for Pluto
  table_name <- "DE440Pluto"
  dbWriteTable(con, table_name, pluto)
  
  # Read data for the Moon
  moon <- arrow::read_parquet(here("data", "processed", "de440", "Moon", "MoonMasterDE440.parquet"))
  
  # Write data for the Moon
  table_name <- "DE440Moon"
  dbWriteTable(con, table_name, moon)
  
  # Read data for the Sun
  sun <- arrow::read_parquet(here("data", "processed", "de440", "Sun", "SunMasterDE440.parquet"))
  
  # Write data for the Sun
  table_name <- "DE440Sun"
  dbWriteTable(con, table_name, sun)
  
  # Read data for the Nutation
  nutation <- arrow::read_parquet(here("data", "processed", "de440", "Nutation", "NutationMasterDE440.parquet"))
  
  # Write data for the Nutation
  table_name <- "DE440Nutation"
  dbWriteTable(con, table_name, nutation)
  
  # Read data for the Julian Day Numbers
  jdn <- arrow::read_parquet(here("data", "processed", "JulianDayNumber", "JulianDayNumber.parquet"))
  
  # Write data for the Julian Day Numbers
  table_name <- "JulianDayNumber"
  dbWriteTable(con, table_name, jdn)
  
  # Shutdown database
  dbDisconnect(con, shutdown = TRUE)
}
