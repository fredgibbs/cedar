# This file contains a set of three functions that "looks out" to see where students are before, after, and during a course.
# All functions are term agnostic, so output data is averages from class list data across terms.
# REQUIRED: opt$course

# TODO: use filter class list to process opt params and allow for more specific inquiries, like term types or a specific year


# WHERE_TO: Calculate destination flows with all necessary metrics
# WHERE_TO: Calculate destination flows with all necessary metrics
where_to <- function (students, opt) {
  
  # opt <- list()
  # opt[["course"]] <- "HIST 1105"

  target_course <- opt[["course"]]
  incl_summer <- opt[["summer"]] %||% FALSE
  
  message("[lookout.R] === WHERE_TO Analysis Starting ===")
  message("[lookout.R] Finding where students go AFTER ", target_course, "...")
  message("[lookout.R] Include summer terms: ", incl_summer)
  
  # Get students in target course with term type
  student_list <- students %>% 
    filter(SUBJ_CRSE == target_course) %>% 
    select(`Course Campus Code`, `Course College Code`, `Academic Period Code`, `Student ID`, term_type) %>% 
    distinct() %>%
    rename(target_term = `Academic Period Code`, source_term_type = term_type)
  
  message("[lookout.R] Across all campuses and registration types (including drops), found ", nrow(student_list), " students in ", target_course)
  
  # Show detailed term-by-term enrollment
  source_by_term <- student_list %>%
    group_by(target_term, source_term_type) %>%
    summarize(enrl = n(), .groups = "drop") %>%
    arrange(target_term)
  
  message("[lookout.R] Term-by-term enrollment in ", target_course, ":")
  for(i in 1:min(10, nrow(source_by_term))) {
    message("[lookout.R]   ", source_by_term$target_term[i], " (", 
            toupper(source_by_term$source_term_type[i]), "): ",
            source_by_term$enrl[i], " students")
  }
  
  # Show term type summary
  term_dist <- student_list %>% count(source_term_type, sort = TRUE)
  message("[lookout.R] Source term type distribution:")
  for(i in 1:nrow(term_dist)) {
    avg_enrl <- source_by_term %>%
      filter(source_term_type == term_dist$source_term_type[i]) %>%
      summarize(avg = mean(enrl)) %>%
      pull(avg)
    message("[lookout.R]   ", toupper(term_dist$source_term_type[i]), ": ", 
            term_dist$n[i], " total enrollments (~",
            round(avg_enrl, 1), " avg/term)")
  }
  
  # Map to next terms
  message("[lookout.R] Adding next term column...")
  student_list <- student_list %>% add_next_term_col("target_term", summer = incl_summer)
  
  mapped <- student_list %>% filter(!is.na(next_term))
  message("[lookout.R] Successfully mapped ", nrow(mapped), " students to next terms.")
  
  # Get courses in next terms
  message("[lookout.R] Finding courses in next terms...")
  disp_merge <- merge(student_list, students, 
                     by.y = c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), 
                     by.x = c("Course Campus Code", "Course College Code", "Student ID", "next_term")) %>%
    distinct() %>%
    rename(dest_term_type = term_type)
  
  message("[lookout.R] Found ", nrow(disp_merge), " enrollments in next terms.")
  message("[lookout.R] (including ", nrow(disp_merge %>% filter(SUBJ_CRSE == target_course)), " repeat enrollments in ", target_course, ")")
  message("[lookout.R] Number of destination courses: ", length(unique(disp_merge$SUBJ_CRSE)))
  
  # Show flow patterns
  if(nrow(disp_merge) > 0) {
    flow_patterns <- disp_merge %>% 
      select(source_term_type, dest_term_type) %>%
      distinct() %>%
      count(source_term_type, dest_term_type, sort = TRUE)
    message("[lookout.R] Flow patterns by term type:")
    for(i in 1:nrow(flow_patterns)) {
      message("[lookout.R]   ", toupper(flow_patterns$source_term_type[i]), " → ", 
              toupper(flow_patterns$dest_term_type[i]))
    }
  }
  
  # Calculate by term - no classification grouping
  message("[lookout.R] Calculating term-specific flows...")
  by_term <- disp_merge %>% 
    group_by(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, 
             source_term_type, dest_term_type, next_term) %>%  
    summarize(enrolled = n(), .groups = "drop")
  
  message("[lookout.R] Created ", nrow(by_term), " term-specific records")
  
  # Show top term-specific flows
  if(nrow(by_term) > 0) {
    top_terms <- by_term %>%
      group_by(next_term, SUBJ_CRSE, source_term_type, dest_term_type) %>%
      summarize(total = sum(enrolled), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10)
    
    message("[lookout.R] Top term-specific destination flows (source → target):")
    for(i in 1:nrow(top_terms)) {
      message("[lookout.R]   ", top_terms$next_term[i], ": ",
              top_terms$SUBJ_CRSE[i], " (", 
              toupper(top_terms$source_term_type[i]), " → ",
              toupper(top_terms$dest_term_type[i]), "): ",
              top_terms$total[i], " students")
    }
  }
  
  # Average across terms - no classification grouping
  message("[lookout.R] Averaging across terms...")
  dispersal_avgs <- by_term %>%
    group_by(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, 
             source_term_type, dest_term_type) %>%
    summarize(
      total_students = sum(enrolled),
      num_terms = n_distinct(next_term),
      min_contrib = min(enrolled),
      max_contrib = max(enrolled),
      .groups = "drop"
    ) %>%
    mutate(avg_contrib = total_students / num_terms) %>%
    arrange(desc(avg_contrib))
  
  dispersal_avgs$from_crse <- target_course
  
  message("[lookout.R] Final: ", nrow(dispersal_avgs), " destination flows.")
  message("[lookout.R] distinct courses: ", length(unique(dispersal_avgs$SUBJ_CRSE)))
  message("[lookout.R] across campuses: ", length(unique(dispersal_avgs$`Course Campus Code`)))
  message("[lookout.R] distinct courses at other campuses: ", 
          length(unique(dispersal_avgs$SUBJ_CRSE[dispersal_avgs$`Course Campus Code` != "ABQ"])))
  
  # Show top destinations with full context
  if(nrow(dispersal_avgs) > 0) {
    top_dest <- dispersal_avgs %>% head(10)
    message("[lookout.R] *** Top destination courses ***")
    for(i in 1:nrow(top_dest)) {
      message("[lookout.R]   ", top_dest$SUBJ_CRSE[i], " (", 
              toupper(top_dest$source_term_type[i]), " → ",
              toupper(top_dest$dest_term_type[i]), "):")
      message("[lookout.R]     Average: ", round(top_dest$avg_contrib[i], 1), 
              " students/term")
      message("[lookout.R]     Range: ", top_dest$min_contrib[i], " to ", 
              top_dest$max_contrib[i], " students")
      message("[lookout.R]     Across: ", top_dest$num_terms[i], " terms")
      message("[lookout.R]     Total: ", top_dest$total_students[i], 
              " students (all terms combined)")
    }
    
    # Flow summary by direction
    flow_summary <- dispersal_avgs %>%
      group_by(source_term_type, dest_term_type) %>%
      summarize(courses = n(),
                avg_total = sum(avg_contrib),
                students_total = sum(total_students),
                .groups = "drop")
    
    message("[lookout.R] *** Flow direction summary ***")
    for(i in 1:nrow(flow_summary)) {
      message("[lookout.R]   ", toupper(flow_summary$source_term_type[i]), " → ", 
              toupper(flow_summary$dest_term_type[i]), ":")
      message("[lookout.R]     ", flow_summary$courses[i], " destination courses")
      message("[lookout.R]     ", round(flow_summary$avg_total[i], 1), 
              " combined avg students/term")
      message("[lookout.R]     ", flow_summary$students_total[i], 
              " total students across all terms")
    }
  }
  
  message("[lookout.R] === WHERE_TO Complete ===")
  return(dispersal_avgs)
}


# WHERE_FROM: Calculate feeder flows with all necessary metrics
where_from <- function (students, opt) {
  
  # opt <- list()
  # opt[["course"]] <- "HIST 1105"
  
  target_course <- opt[["course"]]
  incl_summer <- opt[["summer"]] %||% FALSE
  
  message("[lookout.R] === WHERE_FROM Analysis Starting ===")
  message("[lookout.R] Finding where students come FROM before ", target_course, "...")
  message("[lookout.R] Include summer terms: ", incl_summer)
  
  # Get students in target course with term type
  target_student_list <- students %>% 
    filter(SUBJ_CRSE == target_course) %>% 
    select(`Course Campus Code`, `Course College Code`, `Academic Period Code`, `Student ID`, term_type) %>% 
    distinct() %>%
    rename(target_term = `Academic Period Code`, target_term_type = term_type)
  
  message("[lookout.R] Across all campuses and registration types (including drops), found ", nrow(target_student_list), " students in ", target_course)
  
  # Show detailed term-by-term enrollment
  target_by_term <- target_student_list %>%
    group_by(target_term, target_term_type) %>%
    summarize(enrl = n(), .groups = "drop") %>%
    arrange(target_term)
  
  message("[lookout.R] Term-by-term enrollment in ", target_course, ":")
  for(i in 1:min(10, nrow(target_by_term))) {
    message("[lookout.R]   ", target_by_term$target_term[i], " (", 
            toupper(target_by_term$target_term_type[i]), "): ",
            target_by_term$enrl[i], " students")
  }
  
  # Show term type summary
  term_dist <- target_student_list %>% count(target_term_type, sort = TRUE)
  
  message("[lookout.R] Target term type distribution:")
  for(i in 1:nrow(term_dist)) {
    avg_enrl <- target_by_term %>%
      filter(target_term_type == term_dist$target_term_type[i]) %>%
      summarize(avg = mean(enrl)) %>%
      pull(avg)
    message("[lookout.R]   ", toupper(term_dist$target_term_type[i]), ": ", 
            term_dist$n[i], " total enrollments (~",
            round(avg_enrl, 1), " avg/term)")
  }
  
  # Map to previous terms
  message("[lookout.R] Adding previous term column...")
  target_student_list <- target_student_list %>% add_prev_term_col("target_term", summer = incl_summer)
  
  mapped <- target_student_list %>% filter(!is.na(prev_term))
  message("[lookout.R] Successfully mapped ", nrow(mapped), " students to previous terms.")
  
  # Get courses from previous terms
  message("[lookout.R] Finding courses from previous terms...")
  conduit_students <- merge(target_student_list, students, 
                            by.y = c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), 
                            by.x = c("Course Campus Code", "Course College Code", "Student ID", "prev_term")) %>%
    distinct()
  
  message("[lookout.R] Found ", nrow(conduit_students), " enrollments in previous terms")
  message("[lookout.R] (including ", nrow(conduit_students %>% filter(SUBJ_CRSE == target_course)), " repeat enrollments in ", target_course, ")")
  message("[lookout.R] Number of courses contributing: ", length(unique(conduit_students$SUBJ_CRSE)))
  

  # Show flow patterns
  if(nrow(conduit_students) > 0) {
    flow_patterns <- conduit_students %>% 
      select(term_type, target_term_type) %>%
      distinct() %>%
      count(term_type, target_term_type, sort = TRUE)
    message("[lookout.R] Flow patterns by term type:")
    for(i in 1:nrow(flow_patterns)) {
      message("[lookout.R]   ", toupper(flow_patterns$term_type[i]), " → ", 
              toupper(flow_patterns$target_term_type[i]))
    }
  }
  
  # Calculate by term first
  message("[lookout.R] Calculating term-specific contributions...")
  by_term <- conduit_students %>% 
    group_by(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, 
             term_type, target_term_type, `Student Classification`, target_term) %>%  
    summarize(enrolled = n(), .groups = "drop")
  
  message("[lookout.R] Created ", nrow(by_term), " term-specific records")
  
  # Show top term-specific contributions
  if(nrow(by_term) > 0) {
    top_terms <- by_term %>%
      group_by(target_term, SUBJ_CRSE, term_type, target_term_type) %>%
      summarize(total = sum(enrolled), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10)

    message("[lookout.R] Top term-specific feeder flows (source → target):")
    for(i in 1:nrow(top_terms)) {
      message("[lookout.R]   ", top_terms$target_term[i], ": ",
              top_terms$SUBJ_CRSE[i], " (", 
              toupper(top_terms$term_type[i]), " → ",
              toupper(top_terms$target_term_type[i]), "): ",
              top_terms$total[i], " students")
    }
  }
  
# Average across terms - no classification grouping
  message("[lookout.R] Averaging across terms...")
  feeder_avgs <- by_term %>%
    group_by(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, 
             term_type, target_term_type) %>%
    summarize(
      total_students = sum(enrolled),
      num_terms = n_distinct(target_term),
      min_contrib = min(enrolled),
      max_contrib = max(enrolled),
      .groups = "drop"
    ) %>%
    mutate(avg_contrib = total_students / num_terms) %>%
    rename(source_term_type = term_type) %>%
    arrange(desc(avg_contrib))
  
  feeder_avgs$to_crse <- target_course
  
  message("[lookout.R] Final: ", nrow(feeder_avgs), " feeder flows.")
  message("[lookout.R] distinct courses: ", length(unique(feeder_avgs$SUBJ_CRSE)))
  message("[lookout.R] across campuses: ", length(unique(feeder_avgs$`Course Campus Code`)))
  message("[lookout.R] distinct courses at other campuses: ", 
          length(unique(feeder_avgs$SUBJ_CRSE[feeder_avgs$`Course Campus Code` != "ABQ"])))

  # Show top feeders with full context
  if(nrow(feeder_avgs) > 0) {
    top_feeders <- feeder_avgs %>% head(10)
    message("[lookout.R] *** Top feeder courses ***")
    for(i in 1:nrow(top_feeders)) {
      message("[lookout.R]   ", top_feeders$SUBJ_CRSE[i], " (", 
              toupper(top_feeders$source_term_type[i]), " → ",
              toupper(top_feeders$target_term_type[i]), "):")
      message("[lookout.R]     Average: ", round(top_feeders$avg_contrib[i], 1), 
              " students/term")
      message("[lookout.R]     Range: ", top_feeders$min_contrib[i], " to ", 
              top_feeders$max_contrib[i], " students")
      message("[lookout.R]     Across: ", top_feeders$num_terms[i], " terms")
      message("[lookout.R]     Total: ", top_feeders$total_students[i], 
              " students (all terms combined)")
    }
    
    # Flow summary by direction
    flow_summary <- feeder_avgs %>%
      group_by(source_term_type, target_term_type) %>%
      summarize(courses = n(),
                avg_total = sum(avg_contrib),
                students_total = sum(total_students),
                .groups = "drop")
    
    message("[lookout.R] *** Flow direction summary ***")
    for(i in 1:nrow(flow_summary)) {
      message("[lookout.R]   ", toupper(flow_summary$source_term_type[i]), " → ", 
              toupper(flow_summary$target_term_type[i]), ":")
      message("[lookout.R]     ", flow_summary$courses[i], " feeder courses")
      message("[lookout.R]     ", round(flow_summary$avg_total[i], 1), 
              " combined avg students/term")
      message("[lookout.R]     ", flow_summary$students_total[i], 
              " total students across all terms")
    }
  }
  
  message("[lookout.R] === WHERE_FROM Complete ===")
  return(feeder_avgs)
}


# Simplified plotting function - just visualizes pre-calculated data
# Simplified plotting function - just visualizes pre-calculated data
plot_course_sankey_by_term_with_flow_counts <- function(to_courses, from_courses, opt) {
  
  message("[lookout.R] === SANKEY VISUALIZATION ===")
  message("[lookout.R] Creating term-specific sankey for course: ", opt[["course"]])
  

  source_course <- opt[["course"]]
  min_contrib <- opt[["min_contrib"]] %||% 2
  max_courses <- opt[["max_courses"]] %||% 8
  
  message("[lookout.R] Using visualization parameters:")
  message("[lookout.R]   min_contrib: ", min_contrib)
  message("[lookout.R]   max_courses: ", max_courses)
  
  # Get unique TARGET term types from the data
  term_types_to <- unique(to_courses$source_term_type)
  term_types_from <- unique(from_courses$target_term_type)
  target_term_types <- unique(c(term_types_to, term_types_from))
  target_term_types <- target_term_types[!is.na(target_term_types)]
  
  message("[lookout.R] Creating plots for term types: ", paste(target_term_types, collapse = ", "))
  
  sankey_plots <- list()
  
  for (current_target_type in target_term_types) {
    message("[lookout.R] --- Processing ", toupper(current_target_type), " term ---")
    
    # Filter TO courses for this term type - NO additional averaging needed
    to_term <- to_courses %>% 
      filter(source_term_type == !!current_target_type) %>%
      filter(avg_contrib >= min_contrib) %>%
      arrange(desc(avg_contrib)) %>%
      head(max_courses)
    
    message("[lookout.R] Outgoing flows (", nrow(to_term), " courses):")
    if(nrow(to_term) > 0) {
      for(i in 1:min(5, nrow(to_term))) {
        message("[lookout.R]   ", to_term$SUBJ_CRSE[i], ": ", 
                round(to_term$avg_contrib[i], 1), " students")
      }
    }
    
    # Filter FROM courses - aggregate duplicates BEFORE limiting to max_courses
    from_term_all <- from_courses %>% 
      filter(target_term_type == !!current_target_type) %>%
      filter(avg_contrib >= min_contrib)
    
    # Aggregate by course (sum contributions from different source term types)
    from_term <- from_term_all %>%
      group_by(SUBJ_CRSE) %>%
      summarize(
        avg_contrib = sum(avg_contrib),
        source_term_type = paste(unique(source_term_type), collapse = ","),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_contrib)) %>%
      head(max_courses)
    
    message("[lookout.R] Incoming flows (", nrow(from_term), " courses):")
    if(nrow(from_term) > 0) {
      for(i in 1:min(5, nrow(from_term))) {
        message("[lookout.R]   ", from_term$SUBJ_CRSE[i], " (", 
                from_term$source_term_type[i], "): ", 
                round(from_term$avg_contrib[i], 1), " students")
      }
    }
    
    # Skip if no data
    if (nrow(to_term) == 0 && nrow(from_term) == 0) {
      message("[lookout.R] No sufficient flows for ", current_target_type, " - skipping")
      next
    }
    
    # Create links - simple transformation of pre-calculated data
    to_links <- to_term %>%
      mutate(source = source_course, target = SUBJ_CRSE, value = avg_contrib) %>%
      select(source, target, value) %>%
      filter(target != source_course)
    
    from_links <- from_term %>%
      mutate(source = SUBJ_CRSE, target = source_course, value = avg_contrib) %>%
      select(source, target, value) %>%
      filter(source != source_course)
    
    # Handle courses that appear in both TO and FROM flows
    # For these courses, we need to create separate "left" and "right" nodes
    courses_in_both <- intersect(to_term$SUBJ_CRSE, from_term$SUBJ_CRSE)
    
    if(length(courses_in_both) > 0) {
      message("[lookout.R] Found ", length(courses_in_both), " courses appearing in both directions: ", 
              paste(courses_in_both, collapse = ", "))
      
      # Modify the links to use separate left/right versions for bidirectional courses
      to_links <- to_links %>%
        mutate(target = ifelse(target %in% courses_in_both, 
                              paste0(target, "_RIGHT"), 
                              target))
      
      from_links <- from_links %>%
        mutate(source = ifelse(source %in% courses_in_both, 
                              paste0(source, "_LEFT"), 
                              source))
    }
    
    all_links <- rbind(to_links, from_links)
    
    if (nrow(all_links) == 0) {
      message("[lookout.R] No valid links for ", current_target_type, " - skipping")
      next
    }
    
    message("[lookout.R] Created ", nrow(all_links), " visualization links")
    
    # Calculate totals
    total_to_flow <- if(nrow(to_term) > 0) sum(to_term$avg_contrib) else 0
    total_from_flow <- if(nrow(from_term) > 0) sum(from_term$avg_contrib) else 0
    
    message("[lookout.R] Flow totals: OUT=", round(total_to_flow, 1), 
            ", IN=", round(total_from_flow, 1))
    
    # Create nodes with safe handling for duplicate lookups and left/right versions
    all_nodes <- unique(c(all_links$source, all_links$target))
    
    display_labels <- vapply(all_nodes, function(node) {
      # Handle _LEFT and _RIGHT suffixed nodes
      base_course <- gsub("_(LEFT|RIGHT)$", "", node)
      
      if (node == source_course) {
        paste0(node, "<br>(", current_target_type, ")<br>",
               "OUT: ", round(total_to_flow, 1), " | IN: ", round(total_from_flow, 1))
      } else if (grepl("_LEFT$", node)) {
        # This is a course appearing on the left (source) side
        matches_contrib <- from_term$avg_contrib[from_term$SUBJ_CRSE == base_course]
        matches_term <- from_term$source_term_type[from_term$SUBJ_CRSE == base_course]
        contrib <- if(length(matches_contrib) > 0) matches_contrib[1] else 0
        src_term <- if(length(matches_term) > 0) matches_term[1] else "?"
        paste0(base_course, "<br>(", src_term, ": ~", round(contrib, 1), ")")
      } else if (grepl("_RIGHT$", node)) {
        # This is a course appearing on the right (target) side
        matches <- to_term$avg_contrib[to_term$SUBJ_CRSE == base_course]
        contrib <- if(length(matches) > 0) matches[1] else 0
        paste0(base_course, "<br>(~", round(contrib, 1), ")")
      } else if (base_course %in% to_term$SUBJ_CRSE) {
        matches <- to_term$avg_contrib[to_term$SUBJ_CRSE == base_course]
        contrib <- if(length(matches) > 0) matches[1] else 0
        paste0(base_course, "<br>(~", round(contrib, 1), ")")
      } else if (base_course %in% from_term$SUBJ_CRSE) {
        matches_contrib <- from_term$avg_contrib[from_term$SUBJ_CRSE == base_course]
        matches_term <- from_term$source_term_type[from_term$SUBJ_CRSE == base_course]
        contrib <- if(length(matches_contrib) > 0) matches_contrib[1] else 0
        src_term <- if(length(matches_term) > 0) matches_term[1] else "?"
        paste0(base_course, "<br>(", src_term, ": ~", round(contrib, 1), ")")
      } else {
        node
      }
    }, FUN.VALUE = character(1))
    
    node_colors <- vapply(all_nodes, function(node) {
      if (node == source_course) {
        "#1f77b4"  # Main course - blue
      } else if (grepl("_LEFT$", node) || node %in% from_links$source) {
        "#ff7f0e"  # Source/incoming courses - orange
      } else {
        "#2ca02c"  # Target/outgoing courses - green
      }
    }, FUN.VALUE = character(1))
    
    nodes_df <- data.frame(
      name = all_nodes,
      display_label = display_labels,
      color = node_colors,
      stringsAsFactors = FALSE
    )
    
    message("[lookout.R] Created ", nrow(nodes_df), " nodes")
    
    # Create plotly sankey
    all_links$source_id <- match(all_links$source, nodes_df$name) - 1
    all_links$target_id <- match(all_links$target, nodes_df$name) - 1
    
    sankey_plot <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes_df$display_label,
        color = nodes_df$color,
        pad = 15,
        thickness = 30
      ),
      link = list(
        source = all_links$source_id,
        target = all_links$target_id,
        value = all_links$value,
        color = ifelse(all_links$target == source_course, 
                       "rgba(255, 127, 14, 0.4)",
                       "rgba(44, 160, 44, 0.4)")
      )
    ) %>%
      layout(
        title = paste(toupper(current_target_type), "Flow:", source_course),
        font = list(size = 11),
        margin = list(t = 60)
      )
    
    sankey_plots[[current_target_type]] <- sankey_plot
    message("[lookout.R] Created ", current_target_type, " sankey plot")
  }
  
  message("[lookout.R] === SANKEY COMPLETE: Returning ", length(sankey_plots), " plots ===")
  return(sankey_plots)
}


# Sankey with flow student counts
# opt <- list(
#   course = "HIST 1105",
#   min_contrib = 2,
#   max_courses = 6,
#   summer = FALSE
# )

# Get flow data - calculations done in where_to/where_from
  # to_courses <- where_to(students, opt)
  # from_courses <- where_from(students, opt)
  
# Term-specific plots with flow counts
# term_flow_plots <- plot_course_sankey_by_term_with_flow_counts(to_courses, from_courses, opt)

# Access individual plots
# fall_plot <- term_flow_plots[["fall"]]
# fall_plot
# spring_plot <- term_flow_plots[["spring"]]
# spring_plot
# summer_plot <- term_plots[["summer"]]  # if summer = TRUE





# WHERE_AT, for a given course, finds OTHER courses taken by students at same time
where_at <- function (students,opt) {
  message("\n Figuring out where ELSE students are besides target course...")
  
  target_course <- opt[["course"]]
  
  # get unique ids from students in target_course
  student_list <- students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Course Campus Code`, `Course College Code`, `Academic Period Code`,`Student ID`) %>% distinct()
  
  # rename `Academic Period Code` to target_term
  student_list <- student_list %>%  rename (target_term = `Academic Period Code`)
  
  # merge all student data with IDs from target course to get all student data across courses
  # this provides all student data for all students enrolled in target_course for each term
  student_courses <- merge(student_list, students, by.y=c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), by.x=c("Course Campus Code", "Course College Code", "Student ID", "target_term"))
  student_courses <- student_courses %>% distinct()
  
  # group by term, course, and classification and sum to get total enrollment
  student_courses_summary <- student_courses %>% 
    group_by(`Course Campus Code`,`Course College Code`, target_term,SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n(), .groups="keep")
  
  # drop term, but keep term_type to compute mean number of students in each course by term type
  student_courses_summary <- student_courses_summary %>% 
    group_by(`Course Campus Code`,`Course College Code`, SUBJ_CRSE,`Student Classification`,term_type) %>% 
    mutate(enrl_from_target = mean(enrolled))
  
  # create summary table of courses and mean enrl
  courses_avgs <- student_courses_summary %>% 
    select(`Course Campus Code`,`Course College Code`, SUBJ_CRSE,enrl_from_target) %>%  
    distinct()
  
  # filter out target course
  courses_avgs <- courses_avgs %>% filter (SUBJ_CRSE != target_course)
  
  # create col to indicate target course
  courses_avgs$in_crse <- target_course
  
  #TODO: include target semester variance?
  # get enrollment for current term (via opt), avg for target course, and display delta
  
  message("returning where_AT results...")
  return(courses_avgs)
}

# function to create ploline chart of whereat results to show trends over time
plot_whereat_trends <- function(whereat_data,opt) { 
  message("[Lookout.R] Welcome to plot_where_at_trends...")
  
  # Check if data is available
  if (is.null(whereat_data) || nrow(whereat_data) == 0) {
    message("No data available for plotting.")
    return(NULL)
  }
  
  # Create a line plot using ggplot2
  p <- ggplot(whereat_data, aes(x = target_term, y = enrl_from_target, color = SUBJ_CRSE)) +
    geom_line() +
    labs(title = paste("Enrollment Trends for", opt[["course"]]),
         x = "Term",
         y = "Average Enrollment",
         color = "Course") +
    theme_minimal()
  
  return(p)
}


# main function for external calls
# primary work is to manage a loop to call functions above for each course specified in opt params
lookout <- function (students,opt) {
  
  # for studio testing
  # opt <- list()
  # opt$course <-"HIST 412"
  # opt$status <- "A"
  
  # check for required course param
  if (is.null(opt[["course"]])){
    stop("Required params: -c (course)
         For example: -c 'ENGL 1120'", call.=FALSE)
  }
  
  # convert opt$course into a list
  course_list <- convert_param_to_list(opt[["course"]])
  
  # create DFs storing rows for courses
  where_to_summary <- data.frame()
  where_from_summary <- data.frame()
  where_at_summary <- data.frame()
  
  # create list for final return data
  lookout_summary <- list()
  
  # loop through courses, subsetted by year
  message("about to loop through courses...")
  for (course in course_list) {
    opt$course <- course
    message("processing: ",course)
    
    ###### WHERE_TO
    to_courses <- where_to(students,opt)
    #add new DF to summary table
    where_to_summary <- rbind(where_to_summary,to_courses)
    
    
    ###### WHERE_FROM
    from_courses <- where_from(students,opt)
    #add new DF to summary table
    where_from_summary <- rbind(where_from_summary,from_courses)
    
    
    ###### WHERE_AT
    at_courses <- where_at(students,opt)
    #add new DF to summary table
    where_at_summary <- rbind(where_at_summary,at_courses)
    
  } # end loop through course list
  
  lookout_summary[["where_to"]] <- where_to_summary
  lookout_summary[["where_from"]] <- where_from_summary
  lookout_summary[["where_at"]] <- where_at_summary
  
  return (lookout_summary)
}  
