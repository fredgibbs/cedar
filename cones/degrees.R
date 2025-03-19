count_degrees <- function() {
  
  # degrees.Rda comes from parse-degrees.R, which processes Graduates and Pending Graduates Report.
  degrees <- load_degrees()
  
  # don't filter by college here to get majors/minors from other colleges who have an A&S program as a second majors, certificate, etc.
  # degrees <- grads %>% filter (`College`=="College of Arts and Sciences" | `College`=="Graduate Programs") 
  degrees <- degrees %>% select(`Academic Period Code`,`Actual College`,`Translated College`,ID, Department,Program,`Program Code`,`Award Category`,Degree, Major, `Major Code`,`Second Major`, `First Minor`,`Second Minor`)
  degrees <- unique(degrees) # many degree duplicated because of student attribute field from original data
  degrees <- degrees %>% rename("term_code" = `Academic Period Code`)
  
  # use pre-defined major_to_program_map to filter for just A&S degrees
  programs <- names(major_to_program_map)
  
  # get students who are graduating with a first or second major
  degrees_filtered <- degrees %>% filter (Major %in% programs | `Second Major` %in% programs) 
  
  #TODO: what to do with minors /  certificates / etc?
  
  # summarize, but not using major code to avoid variations like PSY and PSYC; The 'Major' field is more reliable/standard because of mappings.
  degree_summary <- degrees_filtered %>%  group_by(term_code,Major,Degree) %>% summarize (majors=n())
  
  message("degree_summary:")
  degree_summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return (degree_summary)
}


get_degrees_for_dept_report <- function(d_params) {
  
  degree_summary <- count_degrees()
  
  # filter term according to input params
  degree_summary <- degree_summary %>% filter (as.integer(`term_code`) >= d_params$term_start & as.integer(`term_code`) <= d_params$term_end) 
  
  # add acad_year col
  degree_summary <- add_acad_year(degree_summary,"term_code")
  degree_summary <- degree_summary %>%  group_by(acad_year,Major,Degree) %>% summarize (majors=sum(majors))
  
  # filter by d_params prog_names
  degree_summary_filtered <- degree_summary %>% filter (Major %in% d_params$prog_names)
  
  # create LINE CHART, faceted by MAJOR of degrees awarded over time
  # facets only appear when a program has multiple programs
  if (nrow(degree_summary_filtered) > 0 ) {
    degree_summary_faceted_by_major_plot <- ggplot(degree_summary_filtered, aes(x=`acad_year`, y=majors, col=Degree)) + 
      #ggtitle(plot_title) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "")) +
      geom_line(aes(group=Degree)) + 
      geom_point(aes(group=Degree),alpha=.8) +
      facet_wrap(~Major,ncol=3) +
      scale_color_brewer(palette=d_params$palette) +
      xlab("Academic Year") + ylab("Degrees Awarded") 
  } else {degree_summary_filtered_program_plot <- "No data to plot..."}
  
  #degree_summary_faceted_by_major_plot
  degree_summary_faceted_by_major_plot <- ggplotly(degree_summary_faceted_by_major_plot)
  d_params$plots[["degree_summary_faceted_by_major_plot"]] <- degree_summary_faceted_by_major_plot
  
  # summarize just for degree type (ignore program)
  degree_summary_filtered_program <- degree_summary_filtered %>% group_by(acad_year,Degree) %>% 
    summarize (majors_total = sum(majors))
  
  d_params$tables[["degree_summary_filtered_program"]] <- degree_summary_filtered_program
  
  
  # prep plot title
  progs <- paste(d_params$prog_codes, collapse=", ")
  plot_title <- paste(d_params$dept_name,": ", progs)
  
  
  # create BAR CHART of total degrees awarded across ALL PROGRAMS
  if (nrow(degree_summary_filtered_program) > 0 ) {
    degree_summary_filtered_program_stacked_plot <- degree_summary_filtered_program %>% 
      mutate(Degree = fct_reorder(Degree, majors_total)) %>%
      ggplot(aes(x=`acad_year`, y=majors_total, fill=Degree)) + 
      ggtitle(plot_title) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "")) +
      geom_bar(position="stack",stat="identity") + 
      scale_fill_brewer(palette=d_params$palette, limits= unique(degree_summary_filtered_program$Degree)) +
      
      #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      xlab("Academic Year") + ylab("Degrees Awarded") 
  } else {degree_summary_filtered_program_plot <- "No data to plot..."}
  
  #degree_summary_filtered_program_stacked_plot
  degree_summary_filtered_program_stacked_plot <- ggplotly(degree_summary_filtered_program_stacked_plot)
  d_params$plots[["degree_summary_filtered_program_stacked_plot"]] <- degree_summary_filtered_program_stacked_plot
  
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
