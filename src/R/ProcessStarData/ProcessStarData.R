
# Code to process star data and load into a new duckdb database

library(duckdb)
library(DBI)

csv1 <- read_csv("data/raw/star_data/athyg_v33.csv", col_types = cols(
  id = col_double(),
  tyc = col_character(),
  gaia = col_double(),
  hyg = col_double(),
  hip = col_double(),
  hd = col_double(),
  hr = col_double(),
  gl = col_character(),
  bayer = col_character(),
  flam = col_double(),
  con = col_character(),
  proper = col_character(),
  ra = col_double(),
  dec = col_double(),
  pos_src = col_character(),
  dist = col_double(),
  x0 = col_double(),
  y0 = col_double(),
  z0 = col_double(),
  dist_src = col_character(),
  mag = col_double(),
  absmag = col_double(),
  ci = col_double(),
  mag_src = col_character(),
  rv = col_double(),
  rv_src = col_character(),
  pm_ra = col_double(),
  pm_dec = col_double(),
  pm_src = col_character(),
  vx = col_double(),
  vy = col_double(),
  vz = col_double(),
  spect = col_character(),
  spect_src = col_character()
) )

write_parquet(csv1, here::here("data", "processed", 
                               "StarData", "StarData.parquet"))

con <- dbConnect(duckdb(dbdir=here("data", "database", "star_data.duckdb")))

star_data <- arrow::read_parquet(here("data", "processed", "StarData", "StarData.parquet"))

table_name <- "StarData"
dbWriteTable(con, table_name, star_data)

dbDisconnect(con, shutdown = TRUE)
