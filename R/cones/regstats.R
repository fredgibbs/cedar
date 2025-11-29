# use rollcall to find popular fall courses among sophomores for potential summer offerings
get_high_fall_sophs <- function (students,courses,opt) {
  
  message("getting fall courses with 100+ sophomores for potential summer offerings...")
  myopt <- list()
  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  myopt[["classification"]] <- "Sophomore, 2nd Yr"
  myopt[["term"]] <- "fall"
  rollcall_out <- rollcall(students,myopt)
  
  # 100 is a bit arbitrary; not sure how to calc would what be a better threshold
  rollcall_out <- rollcall_out %>% filter(mean > 100)
  
  # grab just SUBJ_CRSE col
  high_fall_sophs <- tibble(SUBJ_CRSE = unique(rollcall_out$SUBJ_CRSE))
  
  message("all done getting high fall sophs!")
  
  return(as_tibble(high_fall_sophs))
}


# single out bumps to forecast for where_to courses 
get_after_bumps <- function (bumps, students, courses, opt) {
  
  bumps <- bumps$SUBJ_CRSE
  after_bumps <- c()
  
  # reset temp opt params
  myopt <- opt
  
  # loop through bumps to see what courses students take next, and add those to the list
  for (course in bumps) {
    
    # for studio testing...
    #course <- bumps[1]
    #message("now processing: ",course,"...")
    
    myopt[["course"]] <- course
    
    # get top 5 courses where students go after a bump course (than than normal enrollment)
    where_tos <- where_to(students,myopt) %>% arrange (desc(avg_contrib))
    next_courses <- head(where_tos,n=5)
    after_bumps <- c(after_bumps, next_courses$SUBJ_CRSE)
    
  } # end loop through bumps to find next courses
  
  after_bumps <- unique(tibble(SUBJ_CRSE = after_bumps))
  
  message("done assembling, and returning after bumps...")
  
  return(after_bumps)
}



# Helper function to create cache filename following CEDAR patterns
create_regstats_cache_filename <- function(opt) {
  message("[regstats.R] Creating cache filename from opt parameters...")
  
  # Extract key filtering parameters for common dashboard use
  filename_parts <- c("regstats")
  
  # Add college filter (most common)
  if (!is.null(opt[["course_college"]]) && length(opt[["course_college"]]) > 0) {
    college_part <- paste(sort(opt[["course_college"]]), collapse = "-")
    # Clean college names for filesystem safety
    college_part <- gsub("[^A-Za-z0-9-]", "", college_part)
    filename_parts <- c(filename_parts, college_part)
  } else {
    filename_parts <- c(filename_parts, "all-colleges")
  }
  
  # Add term filter (very common)
  if (!is.null(opt[["term"]]) && length(opt[["term"]]) > 0) {
    term_part <- paste(sort(opt[["term"]]), collapse = "-")
    filename_parts <- c(filename_parts, term_part)
  } else {
    filename_parts <- c(filename_parts, "all-terms")
  }
  
  # Add level filter if specified
  if (!is.null(opt[["level"]]) && length(opt[["level"]]) > 0) {
    level_part <- paste(sort(opt[["level"]]), collapse = "-")
    # Clean level names for filesystem safety
    level_part <- gsub("[^A-Za-z0-9-]", "", level_part)
    filename_parts <- c(filename_parts, level_part)
  }
  
  # Add campus if specified (common filter)
  if (!is.null(opt[["course_campus"]]) && length(opt[["course_campus"]]) > 0) {
    campus_part <- paste(sort(opt[["course_campus"]]), collapse = "-")
    campus_part <- gsub("[^A-Za-z0-9-]", "", campus_part)
    filename_parts <- c(filename_parts, campus_part)
  }
  
  # Join with underscores and add extension
  cache_filename <- paste(filename_parts, collapse = "_") 
  cache_filename <- paste0(cache_filename, ".Rds")
  
  message("[regstats.R] Generated cache filename: ", cache_filename)
  return(cache_filename)
}


# Helper function to check if cache file exists and is fresh
load_regstats_cache <- function(opt, max_age_hours = 24) {
  tryCatch({
    cache_dir <- file.path(cedar_data_dir, "regstats")
    cache_filename <- create_regstats_cache_filename(opt)
    cache_path <- file.path(cache_dir, cache_filename)
    
    if (file.exists(cache_path)) {
      cache_age <- difftime(Sys.time(), file.info(cache_path)$mtime, units = "hours")
      
      if (cache_age < max_age_hours) {
        message("[regstats.R] Loading cached regstats: ", cache_filename, 
                " (", round(cache_age, 2), " hours old)")
        
        cached_data <- readRDS(cache_path)
        
        # Add cache metadata
        cached_data[["cache_info"]] <- list(
          loaded_from_cache = TRUE,
          cache_filename = cache_filename,
          cache_age_hours = as.numeric(cache_age),
          generated_at = file.info(cache_path)$mtime
        )
        
        return(cached_data)
      } else {
        message("[regstats.R] Cache expired (", round(cache_age, 2), " hours old): ", cache_filename)
      }
    } else {
      message("[regstats.R] No cache file found: ", cache_filename)
    }
    
    return(NULL)
  }, error = function(e) {
    message("[regstats.R] Error loading regstats cache: ", e$message)
    return(NULL)
  })
}





# Function to assign concern tiers based on standard deviation ranges
# Context-aware concern tier assignment
# - "high" anomalies (bumps, drops): Only flag values ABOVE normal (more than expected)
# - "low" anomalies (dips): Only flag values BELOW normal (less than expected)  
# - Critical: ±1.5 SD (urgent attention needed)
# - Moderate: ±1.0 SD (notable change)
# - Marginal: ±0.5 SD (slight change worth monitoring)
assign_concern_tier <- function(actual_value, mean_value, sd_value, anomaly_direction = "high") {
  # Calculate how many standard deviations away from mean
  deviation <- (actual_value - mean_value) / sd_value
  
  # Handle cases where sd_value is 0, NA, or deviation is NA/Inf (vectorized)
  deviation <- ifelse(is.na(deviation) | is.infinite(deviation) | is.na(sd_value) | sd_value == 0, 
                     0, deviation)
  
  # Context-specific concern tiers based on what actually matters for each anomaly type
  case_when(
    # For HIGH anomalies (bumps, drops): Only care about values above normal
    anomaly_direction == "high" & deviation >= 1.5 ~ "critical_high",      # +1.5 SD and above - urgent attention
    anomaly_direction == "high" & deviation >= 1.0 ~ "moderate_high",      # +1.0 to +1.5 SD - notable increase
    anomaly_direction == "high" & deviation >= 0.5 ~ "marginally_high",    # +0.5 to +1.0 SD - slight increase
    anomaly_direction == "high" & deviation < 0.5 ~ "normal",              # Below +0.5 SD - not concerning for high anomalies
    
    # For LOW anomalies (dips): Only care about values below normal  
    anomaly_direction == "low" & deviation <= -1.5 ~ "critical_low",       # -1.5 SD and below - urgent attention
    anomaly_direction == "low" & deviation <= -1.0 ~ "moderate_low",       # -1.0 to -1.5 SD - notable decrease
    anomaly_direction == "low" & deviation <= -0.5 ~ "marginally_low",     # -0.5 to -1.0 SD - slight decrease  
    anomaly_direction == "low" & deviation > -0.5 ~ "normal",              # Above -0.5 SD - not concerning for low anomalies
    
    TRUE ~ "normal"                                                        # Fallback
  )
}

# Function to create a summary of concerns by tier for dashboard display
create_tiered_summary <- function(flagged_data) {
  summary_data <- tibble()
  
  # Process each anomaly type that has concern_tier column
  anomaly_types <- c("early_drops", "late_drops", "dips", "bumps")
  
  for (type in anomaly_types) {
    if (!is.null(flagged_data[[type]]) && "concern_tier" %in% names(flagged_data[[type]])) {
      type_summary <- flagged_data[[type]] %>%
        count(concern_tier, name = "count") %>%
        mutate(anomaly_type = type) %>%
        select(anomaly_type, concern_tier, count)
      
      summary_data <- bind_rows(summary_data, type_summary)
    }
  }
  
  # Create a pivot table for dashboard display
  if (nrow(summary_data) > 0) {
    tier_summary <- summary_data %>%
      pivot_wider(names_from = concern_tier, values_from = count, values_fill = 0)
    
    # Ensure all expected columns exist with default values
    expected_cols <- c("critical_high", "critical_low", "moderate_high", "moderate_low", 
                      "marginally_high", "marginally_low", "normal")
    for (col in expected_cols) {
      if (!col %in% names(tier_summary)) {
        tier_summary[[col]] <- 0
      }
    }
    
    # Now safely calculate totals
    tier_summary <- tier_summary %>%
      mutate(
        total_flagged = critical_high + critical_low + moderate_high + moderate_low + 
                       marginally_high + marginally_low + normal,
        critical_total = critical_high + critical_low,
        moderate_total = moderate_high + moderate_low,
        marginal_total = marginally_high + marginally_low
      ) %>%
      arrange(desc(critical_total), desc(moderate_total), desc(marginal_total))
    
    return(tier_summary)
  } else {
    return(tibble(anomaly_type = character(), message = "No tiered anomalies found"))
  }
}

# Helper function to format concern tier labels for display
format_concern_tier <- function(tier) {
  case_when(
    tier == "critical_high" ~ "🔴 Critical High",
    tier == "critical_low" ~ "🔴 Critical Low", 
    tier == "moderate_high" ~ "🟡 Moderate High",
    tier == "moderate_low" ~ "🟡 Moderate Low",
    tier == "normal" ~ "🟢 Normal",
    TRUE ~ tier
  )
}


####### main function 
get_reg_stats <- function(students, courses, opt) {
  message("[regstats.R] Welcome to get_reg_stats!")

  # For studio testing
  #opt <- list()
  #opt[["term"]] <- "202510"
  #opt[["course"]] <- "HIST 1160"
  
  
  # grab default thresholds from config.R
  message("[regstats.R] setting default thresholds (from (shiny_)config.R)...")
  message("[regstats.R] cedar_regstats_thresholds exists: ", exists("cedar_regstats_thresholds"))
  if (exists("cedar_regstats_thresholds")) {
    message("[regstats.R] cedar_regstats_thresholds content:")
    cedar_output <- capture.output(print(cedar_regstats_thresholds))
    message("[regstats.R] ", paste(cedar_output, collapse=" | "))
  } else {
    message("[regstats.R] ERROR: cedar_regstats_thresholds not found!")
  }
  default_thresholds <- cedar_regstats_thresholds
  
  # Add safety check for empty thresholds
  if (is.null(default_thresholds) || length(default_thresholds) == 0) {
    message("[regstats.R] WARNING: cedar_regstats_thresholds is NULL or empty! Using fallback values...")
    default_thresholds <- list(
      min_impacted = 20,
      pct_sd = 1,
      min_squeeze = 0.3,
      min_wait = 20,
      section_proximity = 0.3
    )
  }

 # Check for custom thresholds in opt
  using_custom_thresholds <- FALSE

  message("[regstats.R] ===== DEBUGGING OPT STRUCTURE =====")
  message("[regstats.R] opt exists: ", exists("opt"))
  message("[regstats.R] opt is.null: ", is.null(opt))
  message("[regstats.R] opt class: ", class(opt))
  message("[regstats.R] opt length: ", length(opt))
  if (length(opt) > 0) {
    message("[regstats.R] opt names: ", paste(names(opt), collapse=", "))
    message("[regstats.R] 'thresholds' in names(opt): ", "thresholds" %in% names(opt))
    message("[regstats.R] opt[['thresholds']] is.null: ", is.null(opt[["thresholds"]]))
    
    if (!is.null(opt[["thresholds"]])) {
      message("[regstats.R] opt[['thresholds']] class: ", class(opt[["thresholds"]]))
      message("[regstats.R] opt[['thresholds']] length: ", length(opt[["thresholds"]]))
      message("[regstats.R] opt[['thresholds']] names: ", paste(names(opt[["thresholds"]]), collapse=", "))
      
      # Try to print each threshold individually
      for (name in names(opt[["thresholds"]])) {
        message("[regstats.R] threshold ", name, ": ", opt[["thresholds"]][[name]])
      }
    }
  } else {
    message("[regstats.R] opt is empty!")
  }
  message("[regstats.R] ===== END DEBUG =====")
  
  # Try explicit capture of print output
  threshold_output <- capture.output(print(opt[["thresholds"]]))
  message("[regstats.R] Captured print output: ", paste(threshold_output, collapse=" | "))

  if (!is.null(opt[["thresholds"]])) {
    message("[regstats.R] Thresholds detected in opt$thresholds...")
    custom_thresholds <- opt[["thresholds"]]
    
    # Compare custom thresholds with defaults to determine if they differ
    message("[regstats.R] Comparing custom thresholds to defaults...")
    message("Custom thresholds vs defaults:")
    message("Custom thresholds:")
    message("  min_impacted: ", custom_thresholds[["min_impacted"]])
    message("  min_wait: ", custom_thresholds[["min_wait"]])
    message("  pct_sd: ", custom_thresholds[["pct_sd"]])
    message("  min_squeeze: ", custom_thresholds[["min_squeeze"]])
    message("Default thresholds:")
    message("  min_impacted: ", default_thresholds[["min_impacted"]])
    message("  min_wait: ", default_thresholds[["min_wait"]])
    message("  pct_sd: ", default_thresholds[["pct_sd"]])
    message("  min_squeeze: ", default_thresholds[["min_squeeze"]])
    message("  section_proximity: ", default_thresholds[["section_proximity"]])

    # More robust comparison - check each threshold value individually
    thresholds_differ <- FALSE
    common_names <- intersect(names(custom_thresholds), names(default_thresholds))
    
    message("[regstats.R] Comparing individual threshold values:")
    for (name in common_names) {
      custom_val <- custom_thresholds[[name]]
      default_val <- default_thresholds[[name]]
      values_match <- isTRUE(all.equal(custom_val, default_val))
      message("[regstats.R]   ", name, ": custom=", custom_val, " default=", default_val, " match=", values_match)
      if (!values_match) {
        thresholds_differ <- TRUE
      }
    }
    
    if (thresholds_differ) {
      using_custom_thresholds <- TRUE
      message("[regstats.R] Custom thresholds differ from defaults - bypassing cache.")
      thresholds <- custom_thresholds
    } else {
      message("[regstats.R] Custom thresholds match defaults - cache eligible.")
      thresholds <- default_thresholds
    }
  } else {
    message("[regstats.R] Using default thresholds - cache eligible.")
    thresholds <- default_thresholds
  }

  # Only check cache if using standard thresholds following CEDAR patterns
  if (!using_custom_thresholds) {
    message("[regstats.R] Checking for cached regstats (standard thresholds)...")
    cached_results <- load_regstats_cache(opt, max_age_hours = 24)
    if (!is.null(cached_results)) {
      message("[regstats.R] Found valid cached regstats!")
      return(cached_results)
    }
  } else {
    message("[regstats.R] Skipping cache check due to custom thresholds.")
  }

  # process course param
  if (!is.null(opt[["course"]]) && opt[["course"]] != "") {
    message("processing opt$course (set to '", opt[["course"]], "')...")
    course_list <- convert_param_to_list(opt[["course"]])
    
    # Do course filtering early, but not term, so regstats calcs can get mean values across terms
    message("filtering COURSES by course_list...")
    filtered_courses <- courses %>% filter (SUBJ_CRSE %in% course_list)
    
    message("filtering STUDENTS by course_list...")
    filtered_students <- students %>% filter (SUBJ_CRSE %in% course_list)
    message("left with ",nrow(filtered_students)," students.")
  } else {
    filtered_students <- students
  }
  
  # filter by term LATER so calcs below can get mean values across terms
  message("[regstats.R] Filtering students by opt params...")
  myopt <- opt
  myopt[["term"]] <- NULL
  filtered_students <- filter_class_list(students, myopt)


  # get registration and enrollment stats  
  regstats <- calc_cl_enrls(filtered_students)
  
  # find potential registration anomalies
  # use biased SD calc, since we're not really sampling from a population
  message("[regstats.R] Finding courses of interest...")
  flagged <- list()
  std_fields <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","Academic Period Code","term_type","registered")
  std_group_cols <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","term_type")
  #std_arrange_cols <- c("Course Campus Code","Academic Period Code","impacted")
  std_arrange_cols <- c("Course Campus Code", "Course College Code")
  
  
  ##### EARLY DROPS - Fixed with proper population SD
  message("[regstats.R] Finding early drops...")
  drops <- regstats %>% select(all_of(std_fields), drop_early=dr_early, dr_early_mean)
  drops <- drops %>% group_by(across(all_of(std_group_cols)))
  drops <- drops %>% mutate(
    # Population SD using conversion method
    # pop_sd = sd(drop_early) / sqrt(n()/(n()-1)),
    
    # using direct calculation (equivalent result)
    pop_sd = round(sqrt(sum((drop_early - dr_early_mean)^2) / n()), digits = 2),
    
    # Calculate deviation in SD units
    sd_deviation = round((drop_early - dr_early_mean) / pop_sd, digits = 2),
    
    # Absolute impact for filtering and display
    impacted = round(drop_early - dr_early_mean, digits=2),
    
    # Concern tier assignment
    concern_tier = assign_concern_tier(drop_early, dr_early_mean, pop_sd, "high")
  )

  # Apply threshold during filtering (not during calculation)
  drops <- drops %>% filter(
    abs(sd_deviation) >= thresholds[["pct_sd"]] & 
    (abs(impacted) > thresholds[["min_impacted"]] & 
    concern_tier != "normal")
  )

  drops <- drops %>% arrange(across(all_of(std_arrange_cols)))
  flagged[["early_drops"]] <- drops


##### LATE DROPS
message("[regstats.R] Finding late drops...")
late_drops <- regstats %>% select(all_of(std_fields), drop_late=dr_late, dr_late_mean)
late_drops <- late_drops %>% group_by(across(all_of(std_group_cols)))
late_drops <- late_drops %>% mutate(
  # Population SD using direct calculation (matching early drops)
  pop_sd = round(sqrt(sum((drop_late - dr_late_mean)^2) / n()), digits = 2),
  
  # Calculate deviation in SD units
  sd_deviation = round((drop_late - dr_late_mean) / pop_sd, digits = 2),
  
  # Absolute impact for filtering and display
  impacted = round(drop_late - dr_late_mean, digits=2),
  
  # Concern tier assignment for high anomalies
  concern_tier = assign_concern_tier(drop_late, dr_late_mean, pop_sd, "high")
)

# Apply consistent filtering logic (matching early drops)
late_drops <- late_drops %>% filter(
  abs(sd_deviation) >= thresholds[["pct_sd"]] & 
  (abs(impacted) > thresholds[["min_impacted"]] & 
  concern_tier != "normal")
)

flagged[["late_drops"]] <- late_drops %>% arrange(across(all_of(std_arrange_cols)))


##### DIPS 
message("[regstats.R] Finding dips...")
dips <- regstats %>% select(all_of(std_fields), registered, registered_mean)
dips <- dips %>% group_by(across(all_of(std_group_cols)))
dips <- dips %>% mutate(
  # Population SD using direct calculation
  pop_sd = round(sqrt(sum((registered - registered_mean)^2) / n()), digits = 2),
  
  # Calculate deviation in SD units
  sd_deviation = round((registered - registered_mean) / pop_sd, digits = 2),
  
  # For dips, impact is the shortfall (positive value for easier filtering)
  impacted = round(registered_mean - registered, digits=2),
  
  # Concern tier assignment for low anomalies
  concern_tier = assign_concern_tier(registered, registered_mean, pop_sd, "low")
)

# For dips, we care about negative deviations (below normal)
dips <- dips %>% filter(
  sd_deviation <= -thresholds[["pct_sd"]] & 
  (impacted > thresholds[["min_impacted"]] & 
  concern_tier != "normal")
)

flagged[["dips"]] <- dips %>% arrange(across(all_of(std_arrange_cols)))



##### BUMPS 
message("[regstats.R] Finding bumps...")
bumps <- regstats %>% select(all_of(std_fields), registered, registered_mean)
bumps <- bumps %>% group_by(across(all_of(std_group_cols)))
bumps <- bumps %>% mutate(
  # Population SD using direct calculation
  pop_sd = round(sqrt(sum((registered - registered_mean)^2) / n()), digits = 2),
  
  # Calculate deviation in SD units
  sd_deviation = round((registered - registered_mean) / pop_sd, digits = 2),

  # Absolute impact for filtering and display
  impacted = round(registered - registered_mean, digits=2),
  
  # Concern tier assignment for high anomalies
  concern_tier = assign_concern_tier(registered, registered_mean, pop_sd, "high")
)

# For bumps, we care about positive deviations (above normal)
bumps <- bumps %>% filter(
  sd_deviation >= thresholds[["pct_sd"]] & 
  (impacted > thresholds[["min_impacted"]] & 
  concern_tier != "normal")
)

flagged[["bumps"]] <- bumps %>% arrange(across(all_of(std_arrange_cols)))


##### WAITS
  message("[regstats.R] Finding waits...")
  myopt <- opt
  myopt[["uel"]] <- TRUE
  myopt[["group_cols"]] <- c("CAMP","COLLEGE","TERM", "SUBJ_CRSE", "gen_ed_area")
  enrls <- get_enrl(courses, myopt)
  waits <-  enrls %>% filter (waiting > thresholds[["min_wait"]]) %>% arrange (desc(waiting))
  waits <- waits %>% rename (`Academic Period Code` = TERM)
  flagged[["waits"]] <- waits
  
  
  ##### SQUEEZES
  message("[regstats.R] Finding squeezes...")
  squeezes <- merge(enrls,regstats,
                    by.x=c("CAMP","COLLEGE","TERM","SUBJ_CRSE"),
                    by.y=c("Course Campus Code","Course College Code","Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  squeezes <- squeezes %>% mutate(squeeze = round(avail/dr_all_mean,digits=2))
  squeezes <- squeezes %>% 
    filter (enrolled >= thresholds[["min_impacted"]]) %>%  
    filter (squeeze < thresholds[["min_squeeze"]]) %>% 
    arrange(term_type,TERM,squeeze)  
  
  squeezes <- squeezes %>% rename (`Academic Period Code` = TERM)
  
  flagged[["squeezes"]] <- squeezes
  
  
  # filter report data for supplied term
  if (!is.null(opt[["term"]])) {
    message("[regstats.R] Filtering flagged data by term...")
    flagged <- lapply(flagged, function(x) filter_by_term(x, opt[["term"]], "Academic Period Code"))
  }

  ##### COURSES AFTER BUMPS (if not from shiny)
  if (as.logical(Sys.getenv("shiny")) == FALSE) {
    # message("finding courses students take after bumps...")
    # disabling until we have a better way of using these...
    #flagged[["courses_after_bumps"]] <- get_after_bumps(flagged[["bumps"]], students, courses, opt)
  }

  # gather SUBJ_CRSE col into separate list
  message("[regstats.R] Gathering flagged courses...")
  flagged_courses <- c()
  for (flag in flagged) {
    if (!is.null(flag$SUBJ_CRSE)) {
      flagged_courses <- c(flagged_courses, as.character(flag$SUBJ_CRSE))  
    }
  }

  message("[regstats.R] Filtered flagged_courses has ", length(flagged_courses), " courses.")

  flagged[["all_flagged_courses"]] <- sort(unique(flagged_courses))
  
  # save thresholds for adding to report
  flagged[["thresholds"]] <- thresholds 
  
  # Create tiered summary for dashboard
  flagged[["tiered_summary"]] <- create_tiered_summary(flagged)
  
  # keep separate from flagged courses since we don't need to forecast for this all the time
  if (as.logical(Sys.getenv("shiny")) == FALSE) {
    flagged[["high_fall_sophs"]] <- get_high_fall_sophs(students, courses, myopt)
  }
  
  if (!using_custom_thresholds) {

# Save flagged data to cache following CEDAR patterns
  tryCatch({
    # Create cache directory if it doesn't exist
    cache_dir <- file.path(cedar_data_dir, "regstats")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Generate cache filename based on opt parameters
    cache_filename <- create_regstats_cache_filename(opt)
    cache_path <- file.path(cache_dir, cache_filename)
    
    # Add metadata to flagged object
    flagged[["cache_info"]] <- list(
      cached = FALSE,
      cache_filename = cache_filename,
      generated_at = Sys.time(),
      opt_params = opt,
      cedar_version = if (exists("cedar_version")) cedar_version else "unknown"
    )
    
    message("[regstats.R] Saving flagged data to: ", cache_filename)
    saveRDS(flagged, cache_path)
    
    # Optional: Clean up old cache files (keep last 20 for common queries)
    existing_files <- list.files(cache_dir, pattern = "^regstats.*\\.Rds$", full.names = TRUE)
    if (length(existing_files) > 20) {
      file_info <- file.info(existing_files)
      old_files <- existing_files[order(file_info$mtime)[1:(length(existing_files) - 20)]]
      unlink(old_files)
      message("[regstats.R] Cleaned up ", length(old_files), " old cache files")
    }
    
  }, error = function(e) {
    message("[regstats.R] Warning: Failed to save regstats cache: ", e$message)
  })

 } else {
    message("[regstats.R] Not caching results due to custom thresholds.")
    # Still add metadata for transparency
    flagged[["cache_info"]] <- list(
      cached = FALSE,
      cache_filename = NULL,
      generated_at = Sys.time(),
      opt_params = opt,
      using_standard_thresholds = FALSE,
      custom_thresholds = thresholds,
      reason_no_cache = "Custom thresholds used"
    )
  }  

  message("[regstats.R] Returning flagged courses...")
  return(flagged)
}



create_regstat_report <- function(students,courses,opt) {
  message("[regstats.R] Welcome to create_regstat_report!")
  
  # get flagged courses
  flagged <- get_reg_stats(students,courses,opt)
  
  # if arrange param set, use it
  if (!is.null(opt[["arrange"]])) {
    arrange_col <- opt[["arrange"]]
    flagged <- flagged %>%  arrange(get({{arrange_col}}) )
  }
  
  # payload
  d_params <- list("opt" = opt,
                    "tables" = list(
                      "flagged" = flagged
                    )
                  )

  # message("[regstats.R] Setting filename based on opt params...")
  # good_opts <- c("course_campus","course_college","course","term","pt","im")
  # output_filename <- "regstats"
  
  # # loop through opt params to build output filename
  # for (i in 1:length(opt)) {
  #   message(names(opt[i]))
  #   if (names(opt[i]) %in% good_opts) {
  #     output_filename <- paste0(output_filename,"-",opt[i])
  #   }
  # }
  # message("output_filename is now: ",output_filename)
  # d_params$output_filename <- output_filename

  d_params$output_filename <- "output"
  d_params$rmd_file <- paste0(cedar_base_dir,"Rmd/regstats-report.Rmd")
  d_params$output_dir_base <- paste0(cedar_output_dir,"regstats-reports/")

  message("[regstats.R] Calling create_report to render regstats report...")
  create_report(opt,d_params)

  message("[regstats.R] All done creating regstats report!")
}
