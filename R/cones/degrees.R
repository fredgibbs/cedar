# degrees_data comes from the Graduates and Pending Graduates Report

count_degrees <- function(degrees_data) {
  
  # don't filter by college here to get majors/minors from other colleges who have an A&S program as a second majors, certificate, etc.
  degrees_data <- degrees_data %>% select(`Academic Period Code`,`Actual College`,`Translated College`,ID, Department,Program,`Program Code`,`Award Category`,Degree, Major, `Major Code`,`Second Major`, `First Minor`,`Second Minor`)
  degrees_data <- unique(degrees_data) # many degree duplicated because of student attribute field from original data
  degrees_data <- degrees_data %>% rename("term_code" = `Academic Period Code`)

  # use pre-defined major_to_program_map to filter for just A&S degrees
  # TODO: make useful for all colleges
  programs <- names(major_to_program_map)
  
  # get students who are graduating with a first or second major
  degrees_filtered <- degrees_data %>% filter (Major %in% programs | `Second Major` %in% programs) 
  
  #TODO: what to do with minors /  certificates / etc?
  
  # summarize, but not using major code (to avoid variations like PSY and PSYC) 
  # the 'Major' field is more reliable/standard because of mappings.
  degree_summary <- degrees_filtered %>%  group_by(term_code,Major,Degree) %>% summarize (majors=n(), .groups = 'drop')
  
  return (degree_summary)
}

# get_degrees_for_dept_report prepares the d_params object with all the data needed for the degrees section of the report
# It calls count_degrees to get the degree summary and then prepares plots and tables for the report.
# It filters the degree summary by the d_params object to include only relevant programs and terms
# It also creates a faceted line chart of degrees awarded over time, and a stacked bar chart of total degrees awarded across all programs.
# The function returns the updated d_params object with the plots and tables added.
# It also prepares the output filename based on the department code and program focus.
get_degrees_for_dept_report <- function(degrees_data, d_params) {
  message("[degrees.R] Welcome to get_degrees_for_dept_report!")

  # Get degrees data from global data_objects 
  degrees <- degrees_data

  degree_summary <- count_degrees(degrees)
  
  # filter term according to input params
  message("[degrees.R] Filtering degree summary by term...")
  degree_summary <- degree_summary %>% filter (as.integer(`term_code`) >= d_params$term_start & as.integer(`term_code`) <= d_params$term_end)

  # group by term_code instead of acad_year
  message("[degrees.R] Grouping degree summary by term_code...")
  degree_summary <- degree_summary %>%  group_by(term_code,Major,Degree) %>% summarize (majors=sum(majors), .groups = 'drop')
  
  # filter by d_params prog_names
  message("[degrees.R] Filtering degree summary by program names...")
  degree_summary_filtered <- degree_summary %>% filter (Major %in% d_params$prog_names)
  
  # create LINE CHART, faceted by MAJOR of degrees awarded over time
  # facets only appear when a program has multiple programs
  message("[degrees.R] Creating faceted line chart of degrees awarded...")
  if (nrow(degree_summary_filtered) > 0 ) {
    degree_summary_faceted_by_major_plot <- ggplot(degree_summary_filtered, aes(x=`term_code`, y=majors, col=Degree)) + 
      #ggtitle(plot_title) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "")) +
      geom_line(aes(group=Degree)) + 
      geom_point(aes(group=Degree),alpha=.8) +
      facet_wrap(~Major,ncol=3) +
      scale_color_brewer(palette=d_params$palette) +
      xlab("Term Code") + ylab("Degrees Awarded")
    # Convert to plotly
    degree_summary_faceted_by_major_plot <- ggplotly(degree_summary_faceted_by_major_plot)
  } else { degree_summary_faceted_by_major_plot <- NULL }

  d_params$plots[["degree_summary_faceted_by_major_plot"]] <- degree_summary_faceted_by_major_plot
  

  # summarize just for degree type (ignore program)
  message("[degrees.R] Summarizing for degree type...")
  degree_summary_filtered_program <- degree_summary_filtered %>% group_by(term_code,Degree) %>% 
    summarize (majors_total = sum(majors))
  
  d_params$tables[["degree_summary_filtered_program"]] <- degree_summary_filtered_program
  
  
  # prep plot title
  progs <- paste(d_params$prog_codes, collapse=", ")
  plot_title <- paste(d_params$dept_name,": ", progs)

  message("[degrees.R] Creating stacked bar chart of degrees awarded...")
  if (nrow(degree_summary_filtered_program) > 0 ) {
    degree_summary_filtered_program_stacked_plot <- degree_summary_filtered_program %>% 
      mutate(Degree = fct_reorder(Degree, majors_total)) %>%
      ggplot(aes(x=`term_code`, y=majors_total, fill=Degree)) + 
      ggtitle(plot_title) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "")) +
      geom_bar(position="stack",stat="identity") + 
      scale_fill_brewer(palette=d_params$palette, limits= unique(degree_summary_filtered_program$Degree)) +
      #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      xlab("Term Code") + ylab("Degrees Awarded")
  # convert to plotly
  degree_summary_filtered_program_stacked_plot <- ggplotly(degree_summary_filtered_program_stacked_plot)
  } else { degree_summary_filtered_program_stacked_plot <- NULL }
  
  d_params$plots[["degree_summary_filtered_program_stacked_plot"]] <- degree_summary_filtered_program_stacked_plot

  message("[degrees.R] returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
