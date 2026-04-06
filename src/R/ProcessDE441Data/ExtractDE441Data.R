
ProcessDE441AsciiFiles <- function()
{
  de441_ascii_files <- c("ascm01000.441", "ascm02000.441", "ascm03000.441", 
                         "ascm04000.441", "ascm05000.441","ascm06000.441",
                         "ascm07000.441", "ascm08000.441", "ascm09000.441",
                         "ascm10000.441", "ascm11000.441", "ascm12000.441",
                         "ascm13000.441", "ascp00000.441", "ascp01000.441",
                         "ascp02000.441", "ascp03000.441", "ascp04000.441",
                         "ascp05000.441", "ascp06000.441", "ascp07000.441",
                         "ascp08000.441", "ascp09000.441", "ascp10000.441",
                         "ascp11000.441", "ascp12000.441", "ascp13000.441",
                         "ascp14000.441", "ascp15000.441", "ascp16000.441")
  
  de441_file_blocks <- c(11415, 11415, 11415, 11415, 11415, 11415, 11415, 11415,
                         11416, 11415, 11415, 11415, 11415, 11415, 11415, 11415,
                         11414, 11415, 11415, 11415, 11415, 11415, 11414, 11415,
                         11415, 11415, 11415, 11415, 11414, 11415)
  
  de441_file_lines <- c(3892515, 3892515, 3892515, 3892515, 3892515, 3892515,
                        3892515, 3892515, 3892856, 3892515, 3892515, 3892515,
                        3892515, 3892515, 3892515, 3892515, 3892174, 3892515,
                        3892515, 3892515, 3892515, 3892515, 3892174, 3892515,
                        3892515, 3892515, 3892515, 3892515, 3892174, 3892515)
  
  
  # Body information from the de441 header file
  de441_body <- c("Mercury", "Venus", "EMB", "Mars", "Jupiter", "Saturn",
                  "Uranus", "Neptune", "Pluto", "Moon", "Sun", "Nutation",
                  "Libration")
  
  de441_block_offset <- c(3, 171, 231, 309, 342, 366, 387, 405, 423, 441, 753,
                           819, 899)
  
  de441_properties <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3)
  
  de441_coeff <- c(14, 10, 13, 11, 8, 7, 6, 6, 6, 13, 11, 10, 10)
  
  de441_interval <- c(4L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 8L, 2L, 4L, 4L)
  
  # Loop through all the ascii files and extract the solar system body data
  # from each file
  num_files <- length(de441_ascii_files)
  
  for (i in 1:num_files) {
    
    ascii_data <- readLines(here::here("data", "raw", "de441", 
                                       de441_ascii_files[i]), n=-1)
    
    # Starting row for each block of data
    block_start <- seq(from = 2, to = de441_file_lines[i] + 341, by = 341)
    
    # Data elements within a row
    row_indices  <- as.vector(outer(0:339, block_start, FUN = `+`))
    
    # Extract all three columns at once from the selected lines
    lines <- ascii_data[row_indices]
    col1 <- as.numeric(chartr("D", "E", substr(lines,  4, 26)))
    col2 <- as.numeric(chartr("D", "E", substr(lines, 30, 52)))
    col3 <- as.numeric(chartr("D", "E", substr(lines, 56, 78)))
    
    # Interleave the three columns to replicate the original sequential order
    vect <- as.vector(rbind(col1, col2, col3))
    
    # Call function to extract data for Mercury
    ExtractSSBodyData(de441_body[1], de441_block_offset[1], de441_properties[1],
                      de441_coeff[1], de441_interval[1], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Venus
    ExtractSSBodyData(de441_body[2], de441_block_offset[2], de441_properties[2],
                      de441_coeff[2], de441_interval[2], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data from the earth-moon barycenter
    ExtractSSBodyData(de441_body[3], de441_block_offset[3], de441_properties[3],
                      de441_coeff[3], de441_interval[3], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Mars
    ExtractSSBodyData(de441_body[4], de441_block_offset[4], de441_properties[4],
                      de441_coeff[4], de441_interval[4], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Jupiter
    ExtractSSBodyData(de441_body[5], de441_block_offset[5], de441_properties[5],
                      de441_coeff[5], de441_interval[5], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Saturn
    ExtractSSBodyData(de441_body[6], de441_block_offset[6], de441_properties[6],
                      de441_coeff[6], de441_interval[6], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Uranus
    ExtractSSBodyData(de441_body[7], de441_block_offset[7], de441_properties[7],
                      de441_coeff[7], de441_interval[7], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Neptune
    ExtractSSBodyData(de441_body[8], de441_block_offset[8], de441_properties[8],
                      de441_coeff[8], de441_interval[8], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Pluto
    ExtractSSBodyData(de441_body[9], de441_block_offset[9], de441_properties[9],
                      de441_coeff[9], de441_interval[9], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for the Moon
    ExtractSSBodyData(de441_body[10], de441_block_offset[10], de441_properties[10],
                      de441_coeff[10], de441_interval[10], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for the Sun
    ExtractSSBodyData(de441_body[11], de441_block_offset[11], de441_properties[11],
                      de441_coeff[11], de441_interval[11], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
    
    # Call function to extract data for Nutation
    ExtractNutationData(de441_body[12], de441_block_offset[12], de441_properties[12],
                        de441_coeff[12], de441_interval[12], de441_ascii_files[i],
                        vect, de441_file_blocks[i])
    
    # Call function to extract data for Libration
    ExtractSSBodyData(de441_body[13], de441_block_offset[13], de441_properties[13],
                      de441_coeff[13], de441_interval[13], de441_ascii_files[i],
                      vect, de441_file_blocks[i])
  }
  
  # Create master files for each body
  num_bodies <- length(de441_body)
  
  for (i in 1:num_bodies) {
    CreateDE441MasterFiles(de441_body[i])
  }
  
}

ExtractSSBodyData <- function(body, block_offset, properties, coef, interval,
                              ascii_file_name, fileptr, file_blocks)
{
  # Set up the output matrix for the body
  numColumnsSSBody <- coef * properties + 3L
  numRowsSSBody    <- file_blocks * interval
  
  # Define column names
  body_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                      paste0("X", seq_len(coef)),
                      paste0("Y", seq_len(coef)),
                      paste0("Z", seq_len(coef)))
  
  # Create matrix
  body_data <- matrix(0.0, nrow = numRowsSSBody, ncol = numColumnsSSBody)
  colnames(body_data) <- body_col_names
  
  # Populate INTERVAL column
  body_data[, "INTERVAL"] <- rep_len(seq_len(interval), numRowsSSBody)
  
  # Populate Julian Day columns
  block_offsets <- seq(from = 0, by = 1020, length.out = file_blocks)
  jd_start <- fileptr[block_offsets + 1L]
  jd_end <- fileptr[block_offsets + 2L]
  
  # Each block produces number of interval rows
  # Replicate each JD value accordingly
  body_data[, "Julian_Day_Start"] <- rep(jd_start, each = interval)
  body_data[, "Julian_Day_End"]   <- rep(jd_end,   each = interval)
  
  # Populate coefficient columns for all subintervals
  # For each subinterval s within a block, offset by s * numCoeff * 3
  # Each subinterval has numCoeff * 3 coefficients
  coeff_per_interval <- coef * properties
  coeff_cols <- 4:numColumnsSSBody
  
  # For each block, compute the base index into vect
  # For each subinterval s within a block, offset by s * numCoeff * 3
  # Result: a matrix where each row is one output row's coefficient indices into vect
  sub_offsets <- (seq_len(interval) - 1L) * coeff_per_interval
  
  # block_base: starting vect index (1-based) for coefficients in each block
  block_base <- block_offsets + block_offset
  
  # Expand: one base per block × numInterval subintervals
  bases <- rep(block_base, each = interval) +
    rep(sub_offsets, times = file_blocks)
  
  # Column offsets within each subinterval (0-based)
  col_offsets <- seq(0L, coeff_per_interval - 1L)
  
  # Build full index matrix: rows = output rows
  idx_matrix <- outer(bases, col_offsets, FUN = `+`)
  
  body_data[, coeff_cols] <- fileptr[idx_matrix]
  
  # Save as parquet
  fn  <- stringr::str_sub(ascii_file_name, 1, 9)
  fnn <- paste0(body, "_", fn, "_de441.parquet")
  
  arrow::write_parquet(as.data.frame(body_data),
                       here::here("data", "processed", "de441", 
                                  body, fnn))
}

ExtractNutationData <- function(body, block_offset, properties, coef, interval,
                              ascii_file_name, fileptr, file_blocks)
{
  # Set up the output matrix for the body
  numColumnsSSBody <- coef * properties + 3L
  numRowsSSBody    <- file_blocks * interval
  
  # Define column names
  body_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                      paste0("Longitude", seq_len(coef)),
                      paste0("Obliquity", seq_len(coef)))
  
  # Create matrix
  body_data <- matrix(0.0, nrow = numRowsSSBody, ncol = numColumnsSSBody)
  colnames(body_data) <- body_col_names
  
  # Populate INTERVAL column
  body_data[, "INTERVAL"] <- rep_len(seq_len(interval), numRowsSSBody)
  
  # Populate Julian Day columns
  block_offsets <- seq(from = 0, by = 1020, length.out = file_blocks)
  jd_start <- fileptr[block_offsets + 1L]
  jd_end <- fileptr[block_offsets + 2L]
  
  # Each block produces number of interval rows
  # Replicate each JD value accordingly
  body_data[, "Julian_Day_Start"] <- rep(jd_start, each = interval)
  body_data[, "Julian_Day_End"]   <- rep(jd_end,   each = interval)
  
  # Populate coefficient columns for all subintervals
  # For each nutation subinterval s within a block, offset by s * numCoeff * 2
  # Each subinterval has numCoeff * 2 coefficients
  coeff_per_interval <- coef * properties
  coeff_cols <- 4:numColumnsSSBody
  
  # For each block, compute the base index into vect
  # For each subinterval s within a block, offset by s * numCoeff * 3
  # Result: a matrix where each row is one output row's coefficient indices into vect
  sub_offsets <- (seq_len(interval) - 1L) * coeff_per_interval
  
  # block_base: starting vect index (1-based) for coefficients in each block
  block_base <- block_offsets + block_offset
  
  # Expand: one base per block × numInterval subintervals
  bases <- rep(block_base, each = interval) +
    rep(sub_offsets, times = file_blocks)
  
  # Column offsets within each subinterval (0-based)
  col_offsets <- seq(0L, coeff_per_interval - 1L)
  
  # Build full index matrix: rows = output rows
  idx_matrix <- outer(bases, col_offsets, FUN = `+`)
  
  body_data[, coeff_cols] <- fileptr[idx_matrix]
  
  # Save as parquet
  fn  <- stringr::str_sub(ascii_file_name, 1, 9)
  fnn <- paste0(body, "_", fn, "_de441.parquet")
  
  arrow::write_parquet(as.data.frame(body_data),
                       here::here("data", "processed", "de441", 
                                  body, fnn))
}

# Create master parquet files for all DE441 bodies which integrates the data
# for all time periods
CreateDE441MasterFiles <- function(body)
{
  # Get list of all parquet files for Mercury
  fp <- list.files(here("data", "processed", "de441", body))
  
  # Create data frames for each parquet file
  numFiles <- length(fp)
  
  df_list <- vector(mode = "list", numFiles)
  
  for (i in 1:numFiles) {
    df_list[[i]] <- arrow::read_parquet(
      here("data", "processed", "de441", body, fp[[i]]))
  }
  
  # Combine data frames into a single data frame
  masterFile <- dplyr::bind_rows(df_list)
  
  # Create master filename
  fn <- paste0(body, "MasterDE441.parquet")
  
  # Save aggregated data for the body
  arrow::write_parquet(masterFile, here("data", "processed", "de441",
                                           body, fn))
}

CreateDE441Database <- function()
{
  #Create database for the DE441 data
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441.duckdb")))
  
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
  
  # Read data for the Libration
  libration <- arrow::read_parquet(here("data", "processed", "de441", "Libration", "LibrationMasterDE441.parquet"))
  
  # Write data for the Libration
  table_name <- "DE441Libration"
  dbWriteTable(con, table_name, libration)
  
  # Shutdown database
  dbDisconnect(con, shutdown = TRUE)
}
