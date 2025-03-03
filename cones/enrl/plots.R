

# takes summary datra from enrl (already filtered, aggregated, etc)


plot_enrls <- function(summary, opt) {
  
  message("welcome to plot_enrls!")  

  courses <- load_courses()
  myopt <- list()
  myopt$aggregate <- "method"
  myopt$x <- "compress"
  myopt$uel <- TRUE 
  myopt$dept <- "HIST"
  myopt$level <- "lower"
  
  message("getting enrollment data via get_enrl...")
  enrls <- get_enrl(courses,myopt)  # filter, aggregate, etc
  
    
  #TODO: filter out AOP sections so it doesn't bring down averages?
  
  enrls_plot <- enrls %>% 
    ggplot(aes(x=TERM, y=enrolled)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    facet_wrap(~INST_METHOD) +
    ylab("Course") + xlab("Total Enrollment (since 2019)") 
  
  enrls_plot
  
  
  
  
  enrls_plot <- enrls %>% 
    ggplot(aes(x=TERM, y=enrolled, color=INST_METHOD)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    facet_wrap(~SUBJ_CRSE) +
    ylab("Course") + xlab("Total Enrollment (since 2019)") 
  
  enrls_plot
  
  
  
  
  # for inspection, rank by avg size across terms or total enrolled
  highest_total_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(enrolled)) %>% slice_head(n=20)    
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size)) %>% slice_head(n=20)    
  
  highest_total_enrl_plot <- highest_total_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, enrolled)) %>%
    ggplot(aes(y=CRSE_TITLE, x=enrolled)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Total Enrollment (since 2019)") 
  
  highest_total_enrl_plot
  
  
  highest_mean_enrl_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(y=CRSE_TITLE, x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Mean Enrollment (since 2019)") 
  
  
  
  
  # histogram of avg class sizes
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size))  
  
  highest_mean_histo_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_histogram(aes(fill=level),bins = 30) +
    scale_fill_brewer(palette=d_params$palette) +
    ylab("Number of courses") + xlab("# of students") 
  
  highest_mean_histo_plot <- ggplotly(highest_mean_histo_plot) %>% 
    layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
           xaxis = list(standoff = -1))
  
  
  # load d_params w/ enrollment plots
  d_params$plots[["highest_total_enrl_plot"]] <- highest_total_enrl_plot
  d_params$plots[["highest_mean_enrl_plot"]] <- highest_mean_enrl_plot
  d_params$plots[["highest_mean_histo_plot"]] <- highest_mean_histo_plot
  
  return(d_params)
}



