# CEDAR Shiny App Usage Logging
# Track user sessions, feature usage, and performance metrics

# Initialize logging system
init_logging <- function() {
  if (!cedar_logging_enabled) return()
  
  # Create logs directory if it doesn't exist
  if (!dir.exists(cedar_log_dir)) {
    dir.create(cedar_log_dir, recursive = TRUE)
    message("[logging.R] Created log directory: ", cedar_log_dir)
  }
  
  # Clean up old log files
  cleanup_old_logs()
  
  message("[logging.R] Logging initialized. Log file: ", cedar_log_file)
}

# Clean up old log files based on retention policy
cleanup_old_logs <- function() {
  if (!dir.exists(cedar_log_dir)) return()
  
  log_files <- list.files(cedar_log_dir, pattern = "cedar_usage_.*\\.log$", full.names = TRUE)
  cutoff_date <- Sys.Date() - cedar_log_retention_days
  
  for (log_file in log_files) {
    file_date <- file.mtime(log_file)
    if (as.Date(file_date) < cutoff_date) {
      file.remove(log_file)
      message("[logging.R] Removed old log file: ", basename(log_file))
    }
  }
}

# Core logging function
write_log <- function(level, event_type, details = NULL, session_id = NULL, user_agent = NULL) {
  if (!cedar_logging_enabled) return()
  
  # Check log level
  log_levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
  if (log_levels[level] < log_levels[cedar_log_level]) return()
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Create log entry with essential fields only - ensure all are simple strings
  log_entry <- list(
    timestamp = as.character(timestamp),
    level = as.character(level),
    session_id = as.character(session_id %||% "unknown"),
    event_type = as.character(event_type),
    details = details
  )
  
  # Convert to JSON for structured logging with auto_unbox to prevent arrays
  log_line <- jsonlite::toJSON(log_entry, pretty = FALSE, auto_unbox = TRUE)
  message("[logging.R] log_line set to: ", log_line)

  # Write to log file with explicit connection handling
  tryCatch({
    # Use write() with explicit newline for atomic writing
    write(log_line, file = cedar_log_file, append = TRUE)
    message("[logging.R] Successfully wrote log entry.")
  }, error = function(e) {
    message("[logging.R] Error writing to log file: ", e$message)
  })
}

# Session tracking functions (updated for reactive context)
log_session_start_reactive <- function(session) {
  # This function should be called within a reactive context
  tryCatch({
    session_id <- session$token
    user_agent <- session$clientData$user_agent
    
    details <- list(
      url = session$clientData$url_hostname,
      protocol = session$clientData$url_protocol,
      port = session$clientData$url_port,
      pathname = session$clientData$url_pathname
    )
    
    write_log("INFO", "session_start", details, session_id, user_agent)
  }, error = function(e) {
    # Fallback if reactive values not available
    session_id <- session$token
    write_log("INFO", "session_start", list(error = "reactive_data_unavailable"), session_id, NULL)
  })
}

# Simple session end (no reactive values needed)
log_session_end <- function(session) {
  session_id <- session$token
  write_log("INFO", "session_end", NULL, session_id, NULL)
}

# Feature usage tracking
log_tab_change <- function(session, tab_name) {
  session_id <- session$token
  details <- list(tab = tab_name)
  write_log("INFO", "tab_change", details, session_id)
}

log_report_generation <- function(session, report_type, parameters = NULL) {
  session_id <- session$token
  details <- list(
    report_type = report_type,
    parameters = parameters
  )
  write_log("INFO", "report_generated", details, session_id)
}

log_data_filter <- function(session, filter_type, filter_values) {
  session_id <- session$token
  details <- list(
    filter_type = filter_type,
    filter_values = filter_values
  )
  write_log("DEBUG", "data_filter", details, session_id)
}

log_download <- function(session, file_type, filename = NULL) {
  session_id <- session$token
  details <- list(
    file_type = file_type,
    filename = filename
  )
  write_log("INFO", "file_download", details, session_id)
}

# Performance monitoring
log_performance <- function(session, operation, duration_seconds, additional_info = NULL) {
  session_id <- session$token
  details <- list(
    operation = operation,
    duration_seconds = round(duration_seconds, 3),
    additional_info = additional_info
  )
  write_log("INFO", "performance", details, session_id)
}

# Error logging
log_error <- function(session, error_message, context = NULL) {
  session_id <- session$token
  details <- list(
    error = error_message,
    context = context
  )
  write_log("ERROR", "error", details, session_id)
}

# Utility function for timing operations
time_operation <- function(expr, session, operation_name, additional_info = NULL) {
  start_time <- Sys.time()
  
  result <- tryCatch({
    eval(expr)
  }, error = function(e) {
    log_error(session, e$message, operation_name)
    stop(e)
  })
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_performance(session, operation_name, duration, additional_info)
  
  return(result)
}

# Report timing functions for performance tracking and user feedback
report_timing_log_file <- file.path(cedar_data_dir, "report_timing.csv")

# Start a timed report operation and return timing context
start_report_timer <- function(report_type, report_params = NULL) {
  timing_context <- list(
    report_type = report_type,
    report_params = report_params,
    start_time = Sys.time()
  )
  
  message("[logging.R] Started timer for ", report_type)
  return(timing_context)
}

# End timer and log the results
end_report_timer <- function(timing_context) {
  end_time <- Sys.time()
  duration_sec <- as.numeric(difftime(end_time, timing_context$start_time, units = "secs"))
  
  # Create log entry
  timing_row <- data.frame(
    timestamp = format(timing_context$start_time, "%Y-%m-%d %H:%M:%S"),
    report_type = timing_context$report_type,
    duration_sec = duration_sec,
    report_params = if(is.null(timing_context$report_params)) NA else jsonlite::toJSON(timing_context$report_params, auto_unbox = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Write to CSV log file
  if (!file.exists(report_timing_log_file)) {
    write.table(timing_row, report_timing_log_file, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
  } else {
    write.table(timing_row, report_timing_log_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  message(sprintf("[logging.R] %s completed in %.2f seconds (logged to %s)", 
                  timing_context$report_type, duration_sec, report_timing_log_file))
  
  return(duration_sec)
}

# Get average timing for a specific report type
get_average_report_time <- function(report_type) {
  if (!file.exists(report_timing_log_file)) {
    return(NULL)
  }
  
  tryCatch({
    log_data <- read.csv(report_timing_log_file, stringsAsFactors = FALSE)
    type_data <- log_data[log_data$report_type == report_type, ]
    
    if (nrow(type_data) > 0) {
      return(round(mean(type_data$duration_sec, na.rm = TRUE), 2))
    }
    return(NULL)
  }, error = function(e) {
    message("[logging.R] Error reading timing log: ", e$message)
    return(NULL)
  })
}

# Create timing status message for user notifications
create_timing_status_message <- function(report_type, action = "Generating") {
  avg_time <- get_average_report_time(report_type)
  
  if (is.null(avg_time)) {
    return(paste(action, report_type, "report... This may take a few moments."))
  } else {
    return(paste(action, report_type, "report... Average time:", avg_time, "seconds."))
  }
}

# Log analysis functions
read_logs <- function(start_date = NULL, end_date = NULL) {
  message("[logging.R] read_logs called with start_date: ", start_date, ", end_date: ", end_date)
  message("[logging.R] cedar_log_dir: ", cedar_log_dir)
  
  if (!dir.exists(cedar_log_dir)) {
    message("[logging.R] Log directory doesn't exist: ", cedar_log_dir)
    return(data.frame())
  }
  
  log_files <- list.files(cedar_log_dir, pattern = "cedar_usage_.*\\.log$", full.names = TRUE)
  message("[logging.R] Found ", length(log_files), " log files: ", paste(basename(log_files), collapse = ", "))
  
  if (length(log_files) == 0) {
    message("[logging.R] No log files found in ", cedar_log_dir)
    return(data.frame())
  }
  
  all_logs <- data.frame()
  
  for (log_file in log_files) {
    message("[logging.R] Processing log file: ", log_file)
    if (file.exists(log_file)) {
      lines <- readLines(log_file, warn = FALSE)
      message("[logging.R] Read ", length(lines), " lines from ", basename(log_file))
      
      valid_entries <- 0
      for (line in lines) {
        if (nchar(line) > 0) {
          tryCatch({
            log_entry <- jsonlite::fromJSON(line)
            
            # Handle array fields from JSON - extract first element if it's an array
            extract_value <- function(field) {
              if (is.null(field)) return(NA)
              if (is.list(field) || is.vector(field)) {
                if (length(field) > 0) return(as.character(field[1]))
                return(NA)
              }
              return(as.character(field))
            }
            
            # Handle complex details field
            details_value <- log_entry$details
            if (is.list(details_value) && !is.null(names(details_value))) {
              # If details is a named list (object), convert to JSON string
              details_value <- jsonlite::toJSON(details_value, auto_unbox = TRUE)
            } else {
              # If details is simple value or array, extract first element
              details_value <- extract_value(details_value)
            }
            
            # Convert to data frame with consistent structure
            log_df <- data.frame(
              timestamp = extract_value(log_entry$timestamp),
              level = extract_value(log_entry$level),
              session_id = extract_value(log_entry$session_id),
              event_type = extract_value(log_entry$event_type),
              details = as.character(details_value),
              user_agent = extract_value(log_entry$user_agent),
              stringsAsFactors = FALSE
            )
            
            all_logs <- rbind(all_logs, log_df)
            valid_entries <- valid_entries + 1
          }, error = function(e) {
            message("[logging.R] Skipping malformed log entry - Error: ", e$message)
            message("[logging.R] Line content: ", line)
          })
        }
      }
      message("[logging.R] Parsed ", valid_entries, " valid entries from ", basename(log_file))
    } else {
      message("[logging.R] Log file doesn't exist: ", log_file)
    }
  }
  
  message("[logging.R] Total log entries before filtering: ", nrow(all_logs))
  
  if (nrow(all_logs) > 0) {
    all_logs$timestamp <- as.POSIXct(all_logs$timestamp, tz = Sys.timezone())
    
    # Filter by date range if specified
    if (!is.null(start_date)) {
      before_filter <- nrow(all_logs)
      # Convert start_date to beginning of day in local timezone
      start_datetime <- as.POSIXct(paste(start_date, "00:00:00"), tz = Sys.timezone())
      all_logs <- all_logs[all_logs$timestamp >= start_datetime, ]
      message("[logging.R] After start_date filter (", start_datetime, "): ", nrow(all_logs), " (was ", before_filter, ")")
    }
    if (!is.null(end_date)) {
      before_filter <- nrow(all_logs)
      # Convert end_date to end of day in local timezone
      end_datetime <- as.POSIXct(paste(end_date, "23:59:59"), tz = Sys.timezone())
      all_logs <- all_logs[all_logs$timestamp <= end_datetime, ]
      message("[logging.R] After end_date filter (", end_datetime, "): ", nrow(all_logs), " (was ", before_filter, ")")
    }
  }
  
  message("[logging.R] Returning ", nrow(all_logs), " log entries")
  return(all_logs)
}

# Generate usage statistics
get_usage_stats <- function(start_date = NULL, end_date = NULL) {
  logs <- read_logs(start_date, end_date)
  
  if (nrow(logs) == 0) {
    return(list(message = "No log data available"))
  }
  
  stats <- list()
  
  # Session statistics
  session_logs <- logs[logs$event_type %in% c("session_start", "session_end"), ]
  stats$total_sessions <- length(unique(session_logs$session_id))
  stats$total_session_starts <- nrow(session_logs[session_logs$event_type == "session_start", ])
  
  # Feature usage
  tab_changes <- logs[logs$event_type == "tab_change", ]
  if (nrow(tab_changes) > 0) {
    stats$most_popular_tabs <- head(sort(table(tab_changes$details), decreasing = TRUE), 10)
  }
  
  # Report generation
  reports <- logs[logs$event_type == "report_generated", ]
  if (nrow(reports) > 0) {
    stats$reports_generated <- nrow(reports)
    stats$report_types <- table(reports$details)
  }
  
  # Performance metrics
  performance_logs <- logs[logs$event_type == "performance", ]
  if (nrow(performance_logs) > 0) {
    stats$avg_performance <- aggregate(
      as.numeric(performance_logs$details), 
      by = list(performance_logs$details), 
      FUN = mean
    )
    names(stats$avg_performance) <- c("operation", "avg_duration_seconds")
  }
  
  # Error count
  stats$error_count <- nrow(logs[logs$level == "ERROR", ])
  
  # Date range
  stats$date_range <- list(
    start = min(logs$timestamp),
    end = max(logs$timestamp)
  )
  
  return(stats)
}

# Print usage summary
print_usage_summary <- function(start_date = NULL, end_date = NULL) {
  stats <- get_usage_stats(start_date, end_date)
  
  if ("message" %in% names(stats)) {
    cat(stats$message, "\n")
    return()
  }
  
  cat("CEDAR Usage Statistics\n")
  cat("======================\n")
  cat("Date Range:", format(stats$date_range$start, "%Y-%m-%d"), "to", format(stats$date_range$end, "%Y-%m-%d"), "\n")
  cat("Total Sessions:", stats$total_sessions, "\n")
  cat("Session Starts:", stats$total_session_starts, "\n")
  
  if ("reports_generated" %in% names(stats)) {
    cat("Reports Generated:", stats$reports_generated, "\n")
  }
  
  if ("most_popular_tabs" %in% names(stats)) {
    cat("\nMost Popular Tabs:\n")
    for (i in 1:min(5, length(stats$most_popular_tabs))) {
      cat("  ", names(stats$most_popular_tabs)[i], ":", stats$most_popular_tabs[i], "\n")
    }
  }
  
  cat("Errors:", stats$error_count, "\n")
}

message("[logging.R] Logging functions loaded")