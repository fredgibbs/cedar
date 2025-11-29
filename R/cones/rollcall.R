# rollcall.R provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify at least a course (or course_list) to see change over time, and/or a term


#' Summarize Classifications
#'
#' This function summarizes student classifications and majors based on specified grouping columns.
#' It calculates distinct counts, means, and percentages of enrollments.
#'
#' @param filtered_students A data frame of filtered student data.
#' @param opt A list of options, including `group_cols` for grouping columns.
#' @return A data frame summarizing student classifications and their percentages of course enrollments.
#' @examples
#' opt <- list(group_cols = c("Course Campus Code", "Major"))
#' summarize_classifications(filtered_students, opt)
summarize_classifications <- function(filtered_students, opt) {
  message("[rollcall.R] Welcome to summarize_classifications! (which can be majors or classifications)")

  # for testing
  # opt <- list()
  # opt[["course"]] <- "ACCT 2110"
  # opt[["course_campus"]] <- "ABQ"
  # opt[["group_cols"]] <- c("Academic Period Code","Course Campus Code","Course College Code","SUBJ_CRSE", "Major", "term_type")
  # filtered_students <- students %>%  filter_class_list(opt)

  group_cols <- opt[["group_cols"]]

  # Set default group_cols if not provided
  if (is.null(group_cols)) {
    message("[rollcall.R] No group_cols specified. Using defaults.")
    group_cols <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                    "Major", "Student Classification", "SUBJ_CRSE", "Short Course Title", "level")
  } else {
    message("[rollcall.R] Using provided group_cols.")
    group_cols <- convert_param_to_list(group_cols)
    group_cols <- as.character(group_cols)
  }

  message("[rollcall.R] group_cols: ", paste(group_cols, collapse = ", "))

  # Group and count distinct students
  summary <- filtered_students %>%
    group_by_at(group_cols) %>%
    distinct(`Student ID`, .keep_all = TRUE) %>%
    summarize(.groups = "keep", count = n())

  # Regroup without "Academic Period Code" to calculate means
  group_cols <- group_cols[-which(group_cols %in% c("Academic Period Code"))]

  # Calculate mean of counts of students by group_cols across all terms
  summary <- summary %>%
    group_by_at(group_cols) %>%
    mutate(mean = round(mean(count), digits = 1))

  # Count course enrollments and percentages
  reg_summary <- calc_cl_enrls(filtered_students)

  # Ungroup for merging and select relevant columns
  crse_enrollment <- reg_summary %>%
    ungroup() %>%
    select(c(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, `Academic Period Code`, registered, registered_mean))

  # Merge summary of majors and classifications with course enrollment data
  merge_sum_enrl <- merge(summary, crse_enrollment, by = c("Course Campus Code", "Course College Code", 
                                                           "Academic Period Code", "SUBJ_CRSE"))
  # Regroup and calculate percentages
  merge_sum_enrl <- merge_sum_enrl %>%
    group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE) %>%
    mutate(term_pct = round(count / registered * 100, digits = 1)) %>%
    mutate(term_type_pct = round(mean / registered_mean  * 100, digits = 1)) %>% 
    arrange(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, desc(term_pct))

  
  message("[rollcall.R] Summary of classifications/majors:")
  message(head (merge_sum_enrl))

  message("[rollcall.R] Returning summarized classifications/majors with enrollment data...")
  return(merge_sum_enrl)
}


#' Create Consistent Color Palette for Rollcall Plots
#'
#' Generates a consistent color mapping for categories across multiple plots to ensure
#' the same majors/classifications have the same colors in fall, spring, and summer plots.
#' @param rollcall_data A dataframe containing rollcall data across all term types.
#' @param fill_column The column name to use for color mapping (e.g., "Student Classification" or "Major")
#' @param top_n Number of top categories to include in color palette (default: 10)
#' @return A named vector of colors where names are category values
#' @examples
#' color_palette <- create_rollcall_color_palette(rollcall_data, "Major", top_n = 8)
create_rollcall_color_palette <- function(rollcall_data, fill_column, top_n = 10) {
  message("[rollcall.R] Creating consistent color palette for ", fill_column)
  
  # Get all unique values across all term types, ordered by overall frequency
  all_categories <- rollcall_data %>%
    group_by(!!sym(fill_column)) %>%
    summarise(total_pct = sum(term_type_pct, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_pct)) %>%
    slice_head(n = top_n) %>%
    pull(!!sym(fill_column))
  
  # Create color palette using CEDAR-friendly colors
  # Use a colorblind-friendly palette with enough distinct colors
  cedar_colors <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5"
  )
  
  # Create named vector mapping categories to colors
  color_mapping <- setNames(cedar_colors[1:length(all_categories)], all_categories)
  
  message("[rollcall.R] Created color palette for ", length(all_categories), " categories")
  return(color_mapping)
}

# add function to show circle plot to show output from rollcall
# could be a separate file, but small enough to include here for now
#' Plot Rollcall Summary with Consistent Colors
#'
#' This function creates a circular plot to visualize the distribution of student classifications and majors.
#' @param rollcall_data A dataframe containing rollcall data.
#' @param fill_column The column name to use for fill aesthetic (e.g., "Student Classification" or "Major")
#' @param color_palette Optional named vector of colors for consistent coloring across plots
#' @param filter_column Optional list with column name and values to filter data (e.g., list(column = "Course Campus Code", values = c("ABQ", "TAOS")))
#' @return A ggplot object representing the circular plot.
#' @examples
#' # For consistent colors across multiple plots:
#' color_palette <- create_rollcall_color_palette(rollcall_data, "Major")
#' fall_plot <- plot_rollcall_summary(fall_data, "Major", color_palette)
#' spring_plot <- plot_rollcall_summary(spring_data, "Major", color_palette)
#' # With campus filter:
#' campus_filter <- list(column = "Course Campus Code", values = c("ABQ"))
#' filtered_plot <- plot_rollcall_summary(rollcall_data, "Major", color_palette, campus_filter)
plot_rollcall_summary <- function(rollcall_data, fill_column = "Student Classification", color_palette = NULL, filter_column = NULL) {
  message("[rollcall.R] Creating circular plot for rollcall summary using fill column: ", fill_column)
  
  # Apply filter if provided
  if (!is.null(filter_column) && !is.null(filter_column$column) && !is.null(filter_column$values)) {
    filter_col <- filter_column$column
    filter_vals <- filter_column$values
    
    if (filter_col %in% colnames(rollcall_data) && length(filter_vals) > 0) {
      original_rows <- nrow(rollcall_data)
      rollcall_data <- rollcall_data %>% filter(!!sym(filter_col) %in% filter_vals)
      message("[rollcall.R] Applied filter on '", filter_col, "' for values: ", paste(filter_vals, collapse = ", "))
      message("[rollcall.R] Filtered data from ", original_rows, " to ", nrow(rollcall_data), " rows")
    } else {
      message("[rollcall.R] Warning: Filter column '", filter_col, "' not found or no filter values provided")
    }
  }
  
  # for testing
  # rollcall_data <- merge_sum_enrl
  # fill_column <- "Major"
  # color_palette <- NULL
  
  # Debug: Check data structure
  message("[rollcall.R] Input data has ", nrow(rollcall_data), " rows and ", ncol(rollcall_data), " columns")
  message("[rollcall.R] Available columns: ", paste(colnames(rollcall_data), collapse = ", "))
  
  # Check if the specified column exists in the data
  if (!fill_column %in% colnames(rollcall_data)) {
    message("[rollcall.R] Warning: Column '", fill_column, "' not found in data. Available columns: ", paste(colnames(rollcall_data), collapse = ", "))
    return(NULL)
  } else {
    message("[rollcall.R] Found specified fill column: ", fill_column)
  }
  
  # Check if pct column exists and has valid data
  if (!"term_type_pct" %in% colnames(rollcall_data)) {
    message("[rollcall.R] Warning: 'term_type_pct' column not found in data")
    return(NULL)
  }
  
  message("[rollcall.R] pct column summary: min=", min(rollcall_data$term_type_pct, na.rm = TRUE), 
          ", max=", max(rollcall_data$term_type_pct, na.rm = TRUE), 
          ", NAs=", sum(is.na(rollcall_data$term_type_pct)))
  
  # remove extra cols for plat data
  message("[rollcall.R] Preparing data for plotting...")
  rollcall_data_for_plot <- rollcall_data %>% ungroup() %>%
    select(all_of(c(fill_column, "Course Campus Code", "Course College Code", "SUBJ_CRSE", "term_type", "count", "term_type_pct", "mean"))) %>%
    distinct() %>%
    arrange(`Course Campus Code`, `Course College Code`,SUBJ_CRSE, term_type, desc(term_type_pct))
  
  # testing summmarizing across period codes for just term_type
  
  rollcall_data_by_term_type <- rollcall_data %>% 
    group_by(across(all_of(c(fill_column, "Course Campus Code", "Course College Code", "SUBJ_CRSE", "term_type")))) %>% 
    summarize(term_type_count = sum(count)) %>% 
    arrange(`Course Campus Code`, `Course College Code`,SUBJ_CRSE, term_type, desc(term_type_count))
  
  rollcall_data_for_plot <- rollcall_data_by_term_type
  
  # Create separate plots for each term_type
  term_types <- unique(rollcall_data_for_plot$term_type)
  message("[rollcall.R] Found ", length(term_types), " term types: ", paste(term_types, collapse = ", "))
  
  plots_by_term <- list()
  
  for (tt in term_types) {
    
    #tt <- "spring"
    message("[rollcall.R] Creating plot for ", tt)
    
    # Filter data for this term_type and get top 7 (without ties)
    term_data <- rollcall_data_for_plot %>% ungroup() %>% 
      filter(term_type == tt) %>%
      slice_max(order_by = term_type_count, n = 5, with_ties = FALSE)
    
    if (nrow(term_data) == 0) {
      message("[rollcall.R] No data for ", tt)
      next
    }
    
    # Calculate percentages from the counts and create custom text
    term_data <- term_data %>%
      mutate(
        percentage = round(term_type_count / sum(term_type_count) * 100, 1),
        custom_text = paste0(percentage, "%\n(", term_type_count, " students)")
      )
    
    # Apply consistent colors if palette provided (CEDAR pattern)
    plot_colors <- NULL
    if (!is.null(color_palette)) {
      category_values <- term_data[[fill_column]]
      plot_colors <- color_palette[category_values]
      plot_colors <- plot_colors[!is.na(plot_colors)]  # Remove any missing mappings
      message("[rollcall.R] Applied custom colors for ", length(plot_colors), " categories in ", tt)
    }
    
    term_plot_interactive <- plot_ly(
      data = term_data,
      labels = ~get(fill_column),
      values = ~term_type_count,  # Use raw counts for proportional sizing
      type = "pie",
      hole = 0.4,  # Creates the donut hole
      text = ~custom_text,
      textinfo = "label+text",  # Show label and our custom percentage + count text
      hovertemplate = paste0("<b>%{label}</b><br>",
                            "Count: %{value}<br>",
                            "Percentage: %{customdata}%<br>",
                            "<extra></extra>"),
      customdata = ~percentage,  # Pass percentage to hover
      marker = list(
        line = list(color = "white", width = 2),
        colors = plot_colors  # Apply consistent colors
      )
    ) %>%
      layout(
        title = paste(tt, "Terms -", fill_column),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.1, y = 0.5)
      )
    
    # Store plot with term_type as key (lowercase for consistency)
    plots_by_term[[tolower(tt)]] <- term_plot_interactive
  }
  
  message("[rollcall.R] Created ", length(plots_by_term), " separate plots")
  
  # Return plots organized by term_type
  message("[rollcall.R] Returning ring chart(s).")
  
  # set return list
  return_list <- list(
    fall = plots_by_term[["fall"]],
    spring = plots_by_term[["spring"]],
    summer = plots_by_term[["summer"]],
    by_term = plots_by_term  # All plots in a list
  )

# return_list[["fall"]]
# return_list[["spring"]]

  return(return_list)
}


#' Rollcall: Main Function
#'
#' This is the main function for processing student data. It filters students, removes pre-201980 data,
#' and aggregates student classifications and majors.
#'
#' @param students A dataframe of aggregated MyReports Class List Reports
#' @param opt A list of options for filtering and grouping, including `group_cols`, `term`, and `course`.
#' @return A summarized dataframe of student classifications and majors.
#' @examples
#' opt <- list(group_cols = c("Major", "Student Classification"), term = 202110)
#' rollcall(students, opt)
rollcall <- function(students, opt) {
  message("[rollcall.R] Welcome to Rollcall!")

  # for studio testing...
  # opt <- list()
  # opt$course <- "BIOL 2305"
  # opt$term <- NULL
  # opt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", 
  #                          "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  

  # Set default group_cols if not provided
  if (is.null(opt[["group_cols"]])) {
    message("No group_cols specified. Using defaults.")
    opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                             "Student Classification", "Major", "SUBJ_CRSE", "Short Course Title", "level")
  }

  # Set default registration_status_code if not provided
  if (is.null(opt[["registration_status_code"]])) {
    message("No registration_status_code specified. Using defaults.")
    opt[["registration_status_code"]] <- c("RE", "RS")
    
  }
  
  message("opt[['group_cols']]: ", paste(opt[["group_cols"]], collapse = ", "))
  message("opt[['registration_status_code']]: ",paste(opt[["registration_status_code"]], collapse = ", "))
  
  # Set the "use exclude list" flag
  message("Setting --uel flag to TRUE...")
  opt$uel <- TRUE

  # Filter students based on options
  filtered_students <- filter_class_list(students, opt)

  # Remove students from terms before 201980
  message("[rollcall.R] Removing students pre-201980...")
  students$`Academic Period Code` <- as.integer(students$`Academic Period Code`)
  filtered_students <- filtered_students %>% filter(`Academic Period Code` >= 201980)

  # Summarize classifications
  message("[rollcall.R] Aggregating student majors/classifications...")
  summary <- summarize_classifications(filtered_students, opt)

  message("[rollcall.R] Returning summary from rollcall...")
  return(summary)
}


#' Plot Classification Time Series
#'
#' Creates line plots showing the percentage of students in each classification across terms over time.
#' @param rollcall_data A dataframe from summarize_classifications containing rollcall data with term info.
#' @param value_column The column to use for y-axis values (default: "term_pct")
#' @param top_n Number of top classifications/majors to display (default: 8)
#' @return A plotly object showing time series lines.
#' @examples
#' plot_time_series(rollcall_data, fill_column = "Student Classification", value_column = "term_type_pct", top_n = 6)
plot_time_series <- function(rollcall_data, fill_column = "Student Classification", value_column = "term_type_pct", top_n = 5) {
  message("[rollcall.R] Welcome to plot_time_series! Creating time series plot.")
  

  #top_n <- 5
  #fill_column = "Major"
  
  # Debug: Check data structure
  message("[rollcall.R] Creating time series plot for ", fill_column, " using value column: ", value_column)
  message("[rollcall.R] Input data has ", nrow(rollcall_data), " rows and ", ncol(rollcall_data), " columns")
  message("[rollcall.R] Available columns: ", paste(colnames(rollcall_data), collapse = ", "))
  
  # Define columns for plotting and grouping
  plot_cols <- c("Academic Period Code", "SUBJ_CRSE", fill_column, value_column, "count")
  grouping_cols <- c("SUBJ_CRSE", "Academic Period Code", "Course Campus Code", "Course College Code", fill_column)
  
  message("[rollcall.R] Using grouping_cols: ", paste(grouping_cols, collapse = ", "))
  message("[rollcall.R] Using plot_cols: ", paste(plot_cols, collapse = ", "))
  
  # Check required columns
  required_cols <- c(fill_column, "Academic Period Code", value_column, "count")
  missing_cols <- required_cols[!required_cols %in% colnames(rollcall_data)]
  
  if (length(missing_cols) > 0) {
    message("[rollcall.R] Warning: Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  # Step 1: Identify top N majors/classifications by summing student counts across all terms
  message("[rollcall.R] Identifying top ", top_n, " ", fill_column, " by total student count...")
  top_categories <- rollcall_data %>%
    group_by(across(all_of(fill_column))) %>%
    summarize(total_students = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(total_students)) %>%
    slice_head(n = top_n) %>%
    pull(!!sym(fill_column))
  
  message("[rollcall.R] Top ", fill_column, " categories: ", paste(top_categories, collapse = ", "))
  
  # Step 2: Filter data to only include top categories, aggregate by grouping columns
  message("[rollcall.R] Filtering data to top categories and aggregating by grouping columns...")
  time_series_data <- rollcall_data %>% 
    filter(!!sym(fill_column) %in% top_categories) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarize(
      term_type_pct = mean(!!sym(value_column), na.rm = TRUE),
      count = sum(count, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(`Academic Period Code`, !!sym(fill_column))
  
  if (nrow(time_series_data) == 0) {
    message("[rollcall.R] No data available for time series plot")
    return(NULL)
  }
  
  message("[rollcall.R] Prepared time series data with ", nrow(time_series_data), " rows for ", length(top_categories), " categories")  
  message("[rollcall.R] Sample data:")
  print(head(time_series_data))
  

  # Create the line plot
  message("[rollcall.R] Creating time series plot...")
  
  time_plot <- plot_ly(
    data = time_series_data,
    x = ~`Academic Period Code`,
    y = ~term_type_pct,
    color = ~get(fill_column),
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                          "Term: %{x}<br>",
                          "Percentage: %{y:.1f}%<br>",
                          "Count: %{customdata}<br>",
                          "<extra></extra>"),
    customdata = ~count,
    line = list(width = 3),
    marker = list(size = 6)
  ) %>%
    layout(
      title = "Trends Over Time",
      xaxis = list(title = "Academic Term", tickangle = -45),
      yaxis = list(title = "Percentage of Course Enrollment"),
      hovermode = "closest",
      legend = list(orientation = "v", x = 1.02, y = 0.5)
    )
  
  
  #time_plot
  
  message("[rollcall.R] Returning time series plot...")
  return(time_plot)
}


#' Plot Rollcall Summary with Consistent Colors Across Terms
#'
#' Wrapper function that creates rollcall plots with consistent color mapping
#' across all term types (fall, spring, summer).
#' @param rollcall_data A dataframe containing rollcall data for all terms.
#' @param fill_column The column name to use for fill aesthetic.
#' @param top_n Number of top categories to include (default: 7)
#' @param filter_column Optional list with column name and values to filter data (e.g., list(column = "Course Campus Code", values = c("ABQ", "TAOS")))
#' @return A list of plots with consistent colors across term types.
#' @examples
#' consistent_plots <- plot_rollcall_with_consistent_colors(rollcall_data, "Major", top_n = 8)
#' # With campus filter:
#' campus_filter <- list(column = "Course Campus Code", values = c("ABQ"))
#' filtered_plots <- plot_rollcall_with_consistent_colors(rollcall_data, "Major", 7, campus_filter)
plot_rollcall_with_consistent_colors <- function(rollcall_data, fill_column = "Student Classification", top_n = 7, filter_column = NULL) {
  message("[rollcall.R] Creating rollcall plots with consistent colors for ", fill_column)
  
  # Create consistent color palette across all terms (before filtering)
  color_palette <- create_rollcall_color_palette(rollcall_data, fill_column, top_n)
  
  # Generate plots with consistent colors and optional filtering
  plots <- plot_rollcall_summary(rollcall_data, fill_column, color_palette, filter_column)
  
  message("[rollcall.R] Generated rollcall plots with consistent ", fill_column, " colors")
  return(plots)
}



