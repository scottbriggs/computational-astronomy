
CreateDE441DE440Database <- function()
{
  #Create database for the DE441 data
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441_de440.duckdb")))
  
  # Read data for Mercury
  mercury <- arrow::read_parquet(here("data", "processed", "Mercury", "MercuryMasterDE441.parquet"))
  
  # Write data for Mercury
  table_name <- "DE441Mercury"
  dbWriteTable(con, table_name, mercury)
  
  # Read data for Venus
  venus <- arrow::read_parquet(here("data", "processed", "Venus", "VenusMasterDE441.parquet"))
  
  # Write data for Venus
  table_name <- "DE441Venus"
  dbWriteTable(con, table_name, venus)
  
  # Read data for EMB
  emb <- arrow::read_parquet(here("data", "processed", "EMB", "EMBMasterDE441.parquet"))
  
  # Write data for EMB
  table_name <- "DE441EMB"
  dbWriteTable(con, table_name, emb)
  
  # Read data for Mars
  mars <- arrow::read_parquet(here("data", "processed", "Mars", "MarsMasterDE441.parquet"))
  
  # Write data for Mars
  table_name <- "DE441Mars"
  dbWriteTable(con, table_name, mars)
  
  # Read data for Jupiter
  jupiter <- arrow::read_parquet(here("data", "processed", "Jupiter", "JupiterMasterDE441.parquet"))
  
  # Write data for Jupiter
  table_name <- "DE441Jupiter"
  dbWriteTable(con, table_name, jupiter)
  
  # Read data for Saturn
  saturn <- arrow::read_parquet(here("data", "processed", "Saturn", "SaturnMasterDE441.parquet"))
  
  # Write data for Saturn
  table_name <- "DE441Saturn"
  dbWriteTable(con, table_name, saturn)
  
  # Read data for Uranus
  uranus <- arrow::read_parquet(here("data", "processed", "Uranus", "UranusMasterDE441.parquet"))
  
  # Write data for Uranus
  table_name <- "DE441Uranus"
  dbWriteTable(con, table_name, uranus)
  
  # Read data for Neptune
  neptune <- arrow::read_parquet(here("data", "processed", "Neptune", "NeptuneMasterDE441.parquet"))
  
  # Write data for Neptune
  table_name <- "DE441Neptune"
  dbWriteTable(con, table_name, neptune)
  
  # Read data for Pluto
  pluto <- arrow::read_parquet(here("data", "processed", "Pluto", "PlutoMasterDE441.parquet"))
  
  # Write data for Pluto
  table_name <- "DE441Pluto"
  dbWriteTable(con, table_name, pluto)
  
  # Read data for the Moon
  moon <- arrow::read_parquet(here("data", "processed", "Moon", "MoonMasterDE441.parquet"))
  
  # Write data for the Moon
  table_name <- "DE441Moon"
  dbWriteTable(con, table_name, moon)
  
  # Read data for the Sun
  sun <- arrow::read_parquet(here("data", "processed", "Sun", "SunMasterDE441.parquet"))
  
  # Write data for the Sun
  table_name <- "DE441Sun"
  dbWriteTable(con, table_name, sun)
  
  # Read data for the Nutation
  nutation <- arrow::read_parquet(here("data", "processed", "Nutation", "NutationMasterDE441.parquet"))
  
  # Write data for the Nutation
  table_name <- "DE441Nutation"
  dbWriteTable(con, table_name, nutation)
  
  # Shutdown database
  dbDisconnect(con, shutdown = TRUE)
}
