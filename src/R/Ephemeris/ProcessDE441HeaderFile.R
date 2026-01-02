
# Function to parse DE441 header.441 file
ParseDE441Header <- function(header_file) {
  cat("Parsing header file:", header_file, "\n\n")

  lines <- readLines(header_file)
  
  # Read header
  header <- data.table(
    lines[5],
    lines[6],
    lines[7]
  )
  
  setnames(header, c("Ephemeris", "Epoch Start", "Epoch End"))
  
  header# Read Start Epoch, End Epoch, and Record Span
  str_split <- strsplit(lines[11], split = "\\s+")
  extracted_columns <- unlist(str_split)
  start_jd <- as.numeric(extracted_columns[2])
  end_jd <- as.numeric(extracted_columns[3])
  record_span_days <- as.numeric(extracted_columns[4])
  
  epoch <- data.table(
    start_jd,
    end_jd,
    record_span_days
  )

  setnames(epoch, c("Epoch Start Julian Day Number", "Epoch End Julian Day Number",
                    "Record Span Days"))
  
  # Read Constant Names
  const_lines <- lines[16:80]
  const_names <- unlist(str_extract_all(paste(const_lines, collapse = " "), "\\S+"))
  
  # Read Constant Values
  ncon_lines <- lines[85:299]
  val_text <- str_replace_all(ncon_lines, "D", "E")
  constants <- as.numeric(unlist(str_extract_all(paste(val_text, collapse = " "), "\\S+")))
  
  # Read Chebyshev coefficient pointers, number of coefficients, and number of
  # intervals for each body
  cheby <- lines[303]
  chebyshev_coeff_ptr <- as.integer(unlist(str_extract_all(paste(cheby, collapse = " "), "\\S+")))
  
  coeff <- lines[304]
  ncoeff <- as.integer(unlist(str_extract_all(paste(coeff, collapse = " "), "\\S+")))
  
  interval <- lines[305]
  ninterval <- as.integer(unlist(str_extract_all(paste(interval, collapse = " "), "\\S+")))
  
  # Create data table for the constant names and values
  const <- data.table(
    const_names,
    constants
  )
  
  # Column names for the constants
  setnames(const, c("Constant Names", "Values"))
  
  # Bodies found in the header file
  body <- c("Mercury", "Venus", "Earth-Moon Barycenter", "Mars", "Jupiter",
            "Saturn", "Uranus", "Neptune", "Pluto", "Moon", "Sun",
            "Nutation", "Lunar Mantle Libration", "Lunar Mantle Angular Velocity",
            "TT-TDB")
  
  # Create data table for the body coefficients
  dt <- data.table(
    body,
    chebyshev_coeff_ptr,
    ncoeff,
    ninterval
  )
  
  # Column names for the coefficients
  setnames(dt, c("Body", "Chebyshev Coeff Ptr", "Number Of Coeff", "Number Of Intervals"))
  
  header_data <- list()
  header_data$header <- header
  header_data$epoch <- epoch
  header_data$const <- const
  header_data$coeff <- dt
  
  return(header_data)
}

# Function to parse the DE440 Header File
ParseDE440Header <- function(header_file) {
  cat("Parsing header file:", header_file, "\n\n")
  
  lines <- readLines(header_file)
  
  # Read header
  header <- data.table(
    lines[5],
    lines[6],
    lines[7]
  )
  
  setnames(header, c("Ephemeris", "Epoch Start", "Epoch End"))
  
  header# Read Start Epoch, End Epoch, and Record Span
  str_split <- strsplit(lines[11], split = "\\s+")
  extracted_columns <- unlist(str_split)
  start_jd <- as.numeric(extracted_columns[2])
  end_jd <- as.numeric(extracted_columns[3])
  record_span_days <- as.numeric(extracted_columns[4])
  
  epoch <- data.table(
    start_jd,
    end_jd,
    record_span_days
  )
  
  setnames(epoch, c("Epoch Start Julian Day Number", "Epoch End Julian Day Number",
                    "Record Span Days"))
  
  # Read Constant Names
  const_lines <- lines[16:80]
  const_names <- unlist(str_extract_all(paste(const_lines, collapse = " "), "\\S+"))
  
  # Read Constant Values
  ncon_lines <- lines[85:299]
  val_text <- str_replace_all(ncon_lines, "D", "E")
  constants <- as.numeric(unlist(str_extract_all(paste(val_text, collapse = " "), "\\S+")))
  
  # Read Chebyshev coefficient pointers, number of coefficients, and number of
  # intervals for each body
  cheby <- lines[303]
  chebyshev_coeff_ptr <- as.integer(unlist(str_extract_all(paste(cheby, collapse = " "), "\\S+")))
  
  coeff <- lines[304]
  ncoeff <- as.integer(unlist(str_extract_all(paste(coeff, collapse = " "), "\\S+")))
  
  interval <- lines[305]
  ninterval <- as.integer(unlist(str_extract_all(paste(interval, collapse = " "), "\\S+")))
  
  # Create data table for the constant names and values
  const <- data.table(
    const_names,
    constants
  )
  
  # Column names for the constants
  setnames(const, c("Constant Names", "Values"))
  
  # Bodies found in the header file
  body <- c("Mercury", "Venus", "Earth-Moon Barycenter", "Mars", "Jupiter",
            "Saturn", "Uranus", "Neptune", "Pluto", "Moon", "Sun",
            "Nutation", "Lunar Mantle Libration", "Lunar Mantle Angular Velocity",
            "TT-TDB")
  
  # Create data table for the body coefficients
  dt <- data.table(
    body,
    chebyshev_coeff_ptr,
    ncoeff,
    ninterval
  )
  
  # Column names for the coefficients
  setnames(dt, c("Body", "Chebyshev Coeff Ptr", "Number Of Coeff", "Number Of Intervals"))
  
  header_data <- list()
  header_data$header <- header
  header_data$epoch <- epoch
  header_data$const <- const
  header_data$coeff <- dt
  
  return(header_data)
}

CreateDE441HeaderDatabase <- function(header_file) {
  
  # Get header data
  header_data <- ParseDE441Header(header_file)
  
  # Connect to DE441 database
  con <- DBI::dbConnect(duckdb(
    dbdir = here::here("data", "database", "de441.duckdb")
  ))
  
  # Write data for the header rows
  table_name <- "DE441Header"
  DBI::dbWriteTable(con, table_name, header_data$header)
  
  # Write data for the epoch
  table_name <- "DE441Epoch"
  DBI::dbWriteTable(con, table_name, header_data$epoch)
  
  # Write data for the constants
  table_name <- "DE441Constants"
  DBI::dbWriteTable(con, table_name, header_data$const)
  
  # Write data for the coefficients
  table_name <- "DE441Coefficients"
  DBI::dbWriteTable(con, table_name, header_data$coeff)
  
  DBI::dbDisconnect(con, shutdown = TRUE)
}

CreateDE440HeaderDatabase <- function(header_file) {
  
  # Get header data
  header_data <- ParseDE440Header(header_file)
  
  # Connect to DE440 database
  con <- DBI::dbConnect(duckdb(
    dbdir = here::here("data", "database", "de440.duckdb")
  ))
  
  # Write data for the header rows
  table_name <- "DE440Header"
  DBI::dbWriteTable(con, table_name, header_data$header)
  
  # Write data for the epoch
  table_name <- "DE440Epoch"
  DBI::dbWriteTable(con, table_name, header_data$epoch)
  
  # Write data for the constants
  table_name <- "DE440Constants"
  DBI::dbWriteTable(con, table_name, header_data$const)
  
  # Write data for the coefficients
  table_name <- "DE440Coefficients"
  DBI::dbWriteTable(con, table_name, header_data$coeff)
  
  DBI::dbDisconnect(con, shutdown = TRUE)
}


