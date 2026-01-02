# Functions to parse the DE441 ASCII files, extract the body coefficients,
# and create a database of julian day numbers and body coefficients

# Query the database for the coefficient pointer data
GetDE441CoefficientPointers <- function() {
  con <- dbConnect(duckdb(dbdir=here("data", "database", "de441.duckdb")))
  
  body_coeff <- dbGetQuery(con, "select * from DE441Coefficients")
  
  DBI::dbDisconnect(con, shutdown = TRUE)
  
  return (body_coeff)
}

# Extract all records from the ASCII file
ExtractDE441ASCIIRecords <- function(filename) {
  de441_file <- readLines(filename)
  
  record_starts <- which(str_detect(de441_file, "^\\s*\\d+\\s+\\d+"))
  
  records <- list()
  
  for (i in seq_along(record_starts)) {
    start_idx <- record_starts[i]
    
    # Determine end of this record
    if (i < length(record_starts)) {
      end_idx <- record_starts[i + 1] - 1
    } else {
      end_idx <- length(de441_file)
    }
    
    # Parse record header (record number and coefficient count)
    header_line <- str_trim(de441_file[start_idx])
    header_vals <- as.integer(unlist(str_extract_all(header_line, "\\d+")))
    record_num <- header_vals[1]
    n_coeff <- header_vals[2]
    
    # Extract coefficient lines
    coeff_lines <- de441_file[(start_idx + 1):end_idx]
    coeff_lines <- coeff_lines[nchar(str_trim(coeff_lines)) > 0]
    
    # Parse all coefficients
    coeff_text <- paste(coeff_lines, collapse = " ")
    # Handle Fortran D notation
    coeff_text <- str_replace_all(coeff_text, "D", "E")
    
    # Extract all numeric values
    coefficients <- as.numeric(unlist(str_extract_all(coeff_text, 
                                                      "[+-]?\\d+\\.\\d+E?[+-]?\\d*")))
    
    # First two values are the time range for this record
    start_jd <- coefficients[1]
    end_jd <- coefficients[2]
    
    # Remaining values are Chebyshev coefficients
    chebyshev_coeffs <- coefficients[3:length(coefficients)]
    
    records[[i]] <- list(
      record_num = record_num,
      start_jd = start_jd,
      end_jd = end_jd,
      coefficients = chebyshev_coeffs
    )
  }
    return(records)
}

# Extract the chebyshev coefficients from a record in the ASCII file
ExtractDE441BodyCoefficients <- function(record, body_name, body_coeff) {
  coeff_info_df <- as.data.frame(body_coeff)
  coeff_info <- filter(coeff_info_df, Body == body_name)
  offset <- as.numeric(coeff_info[2])
  n_coeffs <- as.numeric(coeff_info[3])
  n_sets <- as.numeric(coeff_info[4])
  
  start_idx <- offset - 2
  coeffs_per_set <- n_coeffs * 3
  total_coeffs <- coeffs_per_set * n_sets
  end_idx <- start_idx + total_coeffs - 1
  
  tmp <- c(record$coefficients)
  coeffs <- tmp[start_idx:end_idx]
  coeff_matrix <- matrix(coeffs, nrow = n_sets, byrow = TRUE)
  
  result <- list(
    body = body_name,
    start_jd = record$start_jd,
    end_jd = record$end_jd,
    n_sets = n_sets,
    n_coefficients = n_coeffs,
    coefficients = coeff_matrix
  )
  
  return(result)
}

# Extract the chebyshev coefficients for all solar system bodies and
# and write each body out as a parquet file for each ascii file
ExtractSolarSystemBodies <- function(filename) {
  
  # Get the coefficient pointers
  coef_ptr <- GetDE441CoefficientPointers()
  
  # Get all the records for the ascii file
  asciiFile <- ExtractDE441ASCIIRecords(filename)
  
  # Get the number of records in the ascii file
  numOfRecords <- length(asciiFile)
  
  # Loop across all records for Mercury and extract the coefficients
  mercuryList <- list()
  
  for (i in 1:numOfRecords) {
    result <- ExtractDE441BodyCoefficients(asciiFile[[i]], "Mercury", coef_ptr)
    mercuryList[[i]] <- result
  }
  
  return (mercuryList)
  
}


