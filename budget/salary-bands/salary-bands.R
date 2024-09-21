# this function provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify a course (or course_list)

salary-bands <- function (courses,opt) {
  
  # load class list data
  courses <- load_courses(opt)
  
  #### for studio testing
  opt <- list()
  opt$aggregate <- FALSE
  opt$crosslist <- "compress"
  opt$college <- "AS"
  opt$term <- "202380,202410"
  opt$uel <- TRUE
  opt$job_cat <- "TPT,Term Teacher"
  #opt$enrl_min <- 25
  #opt$enrl_max <- 49
  
  filtered_courses <- get_enrl(courses,opt) 
  #filtered_courses <- filter_DESRs(courses,opt) # this does xl compressing
  nrow(filtered_courses)
  
  enrl_histo_plot <- filtered_courses %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, total_enrl)) %>%
    ggplot(aes(x=total_enrl)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_histogram(aes(fill=level), boundary = 1,binwidth = 5) +
    scale_fill_brewer(palette="Spectral") +
    ylab("Number of courses") + xlab("# of students") 
  
  enrl_histo_plot
  
  # enrl_histo_plot <- ggplotly(highest_mean_histo_plot) %>% 
  #   layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
  #          xaxis = list(standoff = -1))
  # 
  
  
  calc_cost <- function(breaks,salaries) {
    i <- 1
    courses_in_bands <- c()
    for(value in breaks) {
      opt$enrl_min <- breaks[i]
      opt$enrl_max <- breaks[i+1]-1
      filtered_courses <- filter_DESRs(courses,opt) # this does xl compressing
      courses_in_bands <- append(courses_in_bands,nrow(filtered_courses))
      i <- i + 1
    }
    
    costs <- salaries * courses_in_bands
    return(costs)
  }
  
  
  old_breaks <- c(0,75,100,1000)
  old_salaries <- c(4957.07,6160.24,7363.41,0)
  
  old_costs <- calc_cost(old_breaks, old_salaries)
  
  
  
  # from judy
  new_breaks <- c(0,25,50,75,100,1000)
  new_salaries <- c(4957,5558,6159,6760,7363,0) # 150K more
  
  
  new_breaks <- c(0,40,60,80,100,1000)
  new_salaries <- c(5000,5600,6100,6500,6800,0) # 63K more
  
  
  new_breaks <- c(0,40,60,80,100,1000)
  new_salaries <- c(5000,5500,5900,6200,6500,0) # 48.6K more
  
  new_breaks <- c(0,40,60,80,100,1000)
  new_salaries <- c(5000,5500,5900,6300,6700,0) # 51.6K more
  
  new_breaks <- c(0,40,60,80,100,150,1000)
  new_salaries <- c(5000,5500,5900,6300,6700,7000,0) # 52.2K more
  
  new_breaks <- c(0,45,60,80,110,150,1000)
  new_salaries <- c(5000,5500,5900,6300,6700,7000,0) # 33.5K more
  
  new_breaks <- c(0,45,60,85,120,1000)
  new_salaries <- c(5000,5500,5900,6300,6700,0) # 31.3K more
  
  new_breaks <- c(0,45,60,85,120,1000)
  new_salaries <- c(5000,5500,6000,6500,7000,0) # K more
  
  
  new_breaks <- c(0,45,60,85,120,160,1000)
  new_salaries <- c(5000,5600,6200,6700,7100,7500,0) #  45.9K more
  
  new_breaks <- c(0,45,60,80,110,150,1000)
  new_salaries <- c(5000,5600,6300,6700,7100,7300,0) #  45.9K more
  
  
  new_breaks <- c(0,40,60,85,120,160,1000)
  new_salaries <- c(5000,5600,6200,6700,7200,7600,0) # 67.6K more
  
  
  
  new_breaks <- c(0,40,60,80,125,165,1000)
  new_salaries <- c(5000,5500,6000,6500,7000,7500,0) # 54K more
  
  
  new_breaks <- c(0,40,60,80,125,165,1000)
  new_salaries <- c(5000,5600,6100,6500,6900,7200,0) #  61.5K more
  
  new_breaks <- c(0,45,60,80,110,150,1000)
  new_salaries <- c(5000,5600,6100,6600,7000,7300,0) #  45.8K more
  
  new_breaks <- c(0,45,60,80,110,150,1000)
  new_salaries <- c(5000,5600,6200,6700,7100,7400,0) #  45.8K more
  
  new_breaks <- c(0,50,75,100,1000)
  new_salaries <- c(5000,5800,6600,7400,0) # 40K more
  
  new_breaks <- c(0,50,75,100,1000)
  new_salaries <- c(5000,5800,6600,7300,0) # 34K more
  
  
  costs <- calc_cost(new_breaks, new_salaries)
  
  message("diff: ",sum(costs)-sum(old_costs))
  
  
  
  
  ############# OLD FIRST TRY
  
  # ENROLLMENT BONUS SCENARIOS
  # try to generate tables of what we are spending now at various levels
  final  <- final %>% add_column(proposed_sal = 0)
  
  final[is.na(final)] <- 0
  
  courses <- final
  
  create_comp_chart <- function (courses,breaks,base_amt,bonus,min_enrl) {
    
    # create col for proposed salary for each enrollment tier
    courses$proposed_sal[courses$total_enrl > breaks[1] & courses$total_enrl <= breaks[2] ] <- rates[1]
    courses$proposed_sal[courses$total_enrl > breaks[2] & courses$total_enrl <= breaks[3] ] <- rates[2]
    courses$proposed_sal[courses$total_enrl > breaks[3] & courses$total_enrl <= breaks[4] ] <- rates[3]
    courses$proposed_sal[courses$total_enrl > breaks[4] & courses$total_enrl <= breaks[5] ] <- rates[4]
    courses$proposed_sal[courses$total_enrl > breaks[5] ] <- rates[5]
    
    
    a_tier1 <- sum(courses$act_sal[final$total_enrl > breaks[1] & courses$total_enrl <= breaks[2] ])
    a_tier2 <- sum(courses$act_sal[final$total_enrl > breaks[2] & courses$total_enrl <= breaks[3] ])
    a_tier3 <- sum(courses$act_sal[final$total_enrl > breaks[3] & courses$total_enrl <= breaks[4] ]) 
    a_tier4 <- sum(courses$act_sal[final$total_enrl > breaks[4] & courses$total_enrl <= breaks[5] ]) 
    a_tier5 <- sum(courses$act_sal[final$total_enrl > breaks[5] ])
    a_tiers <- c(a_tier1,a_tier2,a_tier3,a_tier4,a_tier5)
    a_r_t_tiers <- a_tiers*1.08
    
    p_tier1 <- sum(courses$proposed_sal[courses$total_enrl > breaks[1] & courses$total_enrl <= breaks[2] ])
    p_tier2 <- sum(courses$proposed_sal[courses$total_enrl > breaks[2] & courses$total_enrl <= breaks[3] ])
    p_tier3 <- sum(courses$proposed_sal[courses$total_enrl > breaks[3] & courses$total_enrl <= breaks[4] ]) 
    p_tier4 <- sum(courses$proposed_sal[courses$total_enrl > breaks[4] & courses$total_enrl <= breaks[5] ]) 
    p_tier5 <- sum(courses$proposed_sal[courses$total_enrl > breaks[5] ])
    p_tiers <- c(p_tier1,p_tier2,p_tier3,p_tier4,p_tier5)
    
    # create comparison DF
    comp <- data.frame(breaks, current_sal=a_tiers, cur_sal_raise = a_r_t_tiers, proposed_sal = p_tiers)
    comp
    
    #get totals for each column
    comp <- comp %>%
      bind_rows(summarize(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"total")))
    comp[[6,1]] <- "total"
    
    comp$diff <- comp$proposed_sal - comp$cur_sal_raise 
    
    print(comp)
    
    print(paste0("act_sal for enrollment under ",min_enrl," is: ",sum(final$act_sal[final$total_enrl <= min_enrl]*1.08,na.rm=TRUE)))
    
    #print(paste("current total:",sum(courses$act_sal,na.rm=TRUE)*1.08,"proposed total:",sum(courses$proposed_sal,na.rm=TRUE)))  
  }
  
  
  #breaks <- c(0,50,755,100,125)
  #breaks <- c(0,75,75,100,125)
  breaks <- c(0,75,100,100,100)
  rates <- c(4960,5600,5600,5600,6700)
  #base_amt <- 4960
  #bonus <- 600
  min_enrl <- 25
  
  create_comp_chart(final,breaks,base_amt,bonus,min_enrl)
  
  #TODO: need a way of understanding non-standard payment amounts
  # it looks like we are 100k off from trying to replicate current spending just on base amounts by course and enorllment
  
  
  
  #final[final$proposed_sal < final$act_sal,]
  #final[final$total_enrl > 18 & final$total_enrl < 22,]
  #sum(final$act_sal[final$total_enrl < 22],na.rm=TRUE)
  
  
  
  
  
}