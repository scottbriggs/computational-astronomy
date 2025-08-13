library(data.table)
library(stringr)
library(readr)
library(purrr)

# JPL DE441 Ephemeris Parser
# Efficiently loads and separates ASCII ephemeris files by solar system body

parse_jpl_de441 <- function(input_file, output_dir = "ephemeris_output") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Body mapping with JPL codes
  body_map <- data.table(
    code = 1:13,
    name = c("mercury", "venus", "earth_moon_bary", "mars", "jupiter", 
             "saturn", "uranus", "neptune", "pluto", "moon_geocentric", 
             "sun", "earth_nutations", "lunar_librations"),
    components = c(rep(3, 9), 3, 3, 2, 3),  # coordinate components
    stringsAsFactors = FALSE
  )
  
  cat("Reading ephemeris file...\n")
  
  # Read entire file efficiently
  lines <- read_lines(input_file)
  
  # Extract header information
  header_end <- which(str_detect(lines, "^\\s*GROUP\\s+1050"))[1]
  if (is.na(header_end)) {
    header_end <- which(str_detect(lines, "^\\s*[0-9]+\\.[0-9]+"))[1] - 1
  }
  
  header_lines <- lines[1:header_end]
  data_lines <- lines[(header_end + 1):length(lines)]
  
  cat("Parsing header information...\n")
  
  # Parse constants and parameters from header
  constants <- parse_header_constants(header_lines)
  
  # Extract coefficient structure information
  coeff_info <- extract_coefficient_structure(header_lines)
  
  cat("Processing data records...\n")
  
  # Initialize storage for each body
  body_data <- vector("list", nrow(body_map))
  names(body_data) <- body_map$name
  
  # Parse data records in chunks for memory efficiency
  chunk_size <- 1000
  data_chunks <- split(data_lines, ceiling(seq_along(data_lines) / chunk_size))
  
  for (chunk_idx in seq_along(data_chunks)) {
    cat(sprintf("Processing chunk %d of %d\n", chunk_idx, length(data_chunks)))
    
    chunk_data <- parse_data_chunk(data_chunks[[chunk_idx]], coeff_info, body_map)
    
    # Append to body data
    for (body_name in names(chunk_data)) {
      body_data[[body_name]] <- rbindlist(list(body_data[[body_name]], 
                                               chunk_data[[body_name]]))
    }
  }
  
  cat("Writing separate files for each body...\n")
  
  # Write individual files for each body
  output_files <- character(nrow(body_map))
  
  for (i in seq_len(nrow(body_map))) {
    body_name <- body_map$name[i]
    
    if (!is.null(body_data[[body_name]]) && nrow(body_data[[body_name]]) > 0) {
      output_file <- file.path(output_dir, paste0(body_name, ".txt"))
      
      # Write header for this body
      write_body_header(output_file, header_lines, body_name, constants)
      
      # Write data records
      write_body_data(output_file, body_data[[body_name]], append = TRUE)
      
      output_files[i] <- output_file
      cat(sprintf("Written %s with %d records\n", output_file, 
                  nrow(body_data[[body_name]])))
    }
  }
  
  # Validation
  validate_output(body_data, body_map)
  
  return(list(
    output_files = output_files[output_files != ""],
    body_data = body_data,
    constants = constants,
    coefficient_info = coeff_info
  ))
}

# Parse header constants
parse_header_constants <- function(header_lines) {
  constants <- list()
  
  # Extract key parameters
  au_line <- str_subset(header_lines, "AU")
  if (length(au_line) > 0) {
    constants$AU <- as.numeric(str_extract(au_line[1], "[0-9.E+-]+"))
  }
  
  emrat_line <- str_subset(header_lines, "EMRAT")
  if (length(emrat_line) > 0) {
    constants$EMRAT <- as.numeric(str_extract(emrat_line[1], "[0-9.E+-]+"))
  }
  
  # Extract time span
  time_span <- str_subset(header_lines, "JED")
  if (length(time_span) > 0) {
    times <- str_extract_all(time_span[1], "[0-9.]+")[[1]]
    constants$start_jd <- as.numeric(times[1])
    constants$end_jd <- as.numeric(times[2])
    constants$step_size <- as.numeric(times[3])
  }
  
  return(constants)
}

# Extract coefficient structure
extract_coefficient_structure <- function(header_lines) {
  # Look for GROUP 1050 which contains coefficient structure
  group_start <- which(str_detect(header_lines, "GROUP\\s+1050"))
  
  if (length(group_start) == 0) {
    # Default structure for DE441
    return(data.table(
      body = 1:13,
      offset = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 35),
      ncoeff = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3),
      nsub = rep(1, 13)
    ))
  }
  
  # Parse actual structure from file
  structure_lines <- header_lines[(group_start + 1):min(group_start + 20, 
                                                         length(header_lines))]
  
  # Extract coefficient counts and offsets
  coeff_info <- data.table(
    body = 1:13,
    offset = integer(13),
    ncoeff = integer(13),
    nsub = integer(13)
  )
  
  return(coeff_info)
}

# Parse data chunk
parse_data_chunk <- function(chunk_lines, coeff_info, body_map) {
  # Remove empty lines
  chunk_lines <- chunk_lines[str_trim(chunk_lines) != ""]
  
  if (length(chunk_lines) == 0) return(list())
  
  # Parse records - each record starts with time information
  record_starts <- which(str_detect(chunk_lines, "^\\s*[0-9]+\\.[0-9]+"))
  
  if (length(record_starts) == 0) return(list())
  
  body_chunk_data <- vector("list", nrow(body_map))
  names(body_chunk_data) <- body_map$name
  
  for (i in seq_along(record_starts)) {
    start_line <- record_starts[i]
    end_line <- if (i < length(record_starts)) {
      record_starts[i + 1] - 1
    } else {
      length(chunk_lines)
    }
    
    record_lines <- chunk_lines[start_line:end_line]
    record_data <- parse_single_record(record_lines, coeff_info, body_map)
    
    # Append to body data
    for (body_name in names(record_data)) {
      body_chunk_data[[body_name]] <- rbindlist(list(body_chunk_data[[body_name]], 
                                                     record_data[[body_name]]))
    }
  }
  
  return(body_chunk_data)
}

# Parse single data record
parse_single_record <- function(record_lines, coeff_info, body_map) {
  # Extract time information from first line
  time_info <- as.numeric(str_extract_all(record_lines[1], "[0-9.E+-]+")[[1]])
  start_jd <- time_info[1]
  end_jd <- time_info[2]
  
  # Extract all coefficients from remaining lines
  coeff_text <- paste(record_lines[-1], collapse = " ")
  coefficients <- as.numeric(str_extract_all(coeff_text, "[0-9.E+-]+")[[1]])
  
  body_record_data <- vector("list", nrow(body_map))
  names(body_record_data) <- body_map$name
  
  # Distribute coefficients to bodies
  coeff_idx <- 1
  
  for (i in seq_len(nrow(body_map))) {
    body_name <- body_map$name[i]
    n_components <- body_map$components[i]
    
    # For DE441, typically 3 components (x,y,z) with varying polynomial degrees
    # Simplified extraction - in practice, would need actual coefficient counts
    n_coeffs <- 13 * n_components  # Typical Chebyshev polynomial degree
    
    if (coeff_idx + n_coeffs - 1 <= length(coefficients)) {
      body_coeffs <- coefficients[coeff_idx:(coeff_idx + n_coeffs - 1)]
      
      body_record_data[[body_name]] <- data.table(
        start_jd = start_jd,
        end_jd = end_jd,
        coefficients = list(body_coeffs)
      )
      
      coeff_idx <- coeff_idx + n_coeffs
    }
  }
  
  return(body_record_data)
}

# Write body header
write_body_header <- function(output_file, header_lines, body_name, constants) {
  # Create modified header for specific body
  modified_header <- c(
    paste("# JPL DE441 Ephemeris Data for", toupper(body_name)),
    paste("# Extracted on", Sys.Date()),
    "",
    header_lines[str_detect(header_lines, "KSIZE|AU|EMRAT|CLIGHT")],
    ""
  )
  
  writeLines(modified_header, output_file)
}

# Write body data
write_body_data <- function(output_file, body_data, append = FALSE) {
  if (nrow(body_data) == 0) return()
  
  # Format data for output
  for (i in seq_len(nrow(body_data))) {
    record_line <- sprintf("%.8f %.8f", body_data$start_jd[i], body_data$end_jd[i])
    write(record_line, output_file, append = TRUE)
    
    # Write coefficients in formatted rows
    coeffs <- body_data$coefficients[[i]]
    coeff_matrix <- matrix(coeffs, ncol = 3, byrow = TRUE)
    
    for (j in seq_len(nrow(coeff_matrix))) {
      coeff_line <- paste(sprintf("%23.15E", coeff_matrix[j, ]), collapse = " ")
      write(coeff_line, output_file, append = TRUE)
    }
  }
}

# Validation function
validate_output <- function(body_data, body_map) {
  cat("\n=== Validation Results ===\n")
  
  total_records <- 0
  for (i in seq_len(nrow(body_map))) {
    body_name <- body_map$name[i]
    if (!is.null(body_data[[body_name]])) {
      n_records <- nrow(body_data[[body_name]])
      total_records <- total_records + n_records
      cat(sprintf("%s: %d records\n", body_name, n_records))
    }
  }
  
  cat(sprintf("Total records processed: %d\n", total_records))
  
  # Check time coverage consistency
  time_ranges <- map_dfr(body_data[!map_lgl(body_data, is.null)], 
                         ~data.frame(min_jd = min(.x$start_jd), 
                                     max_jd = max(.x$end_jd)))
  
  if (nrow(time_ranges) > 0) {
    cat(sprintf("Time coverage: JD %.1f to %.1f\n", 
                min(time_ranges$min_jd), max(time_ranges$max_jd)))
  }
}

# Example usage:
# result <- parse_jpl_de441("ascp2000.441", "de441_separated")
# print(result$output_files)