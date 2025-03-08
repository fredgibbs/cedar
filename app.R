#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(bslib)
library(tidyverse)

source("includes/shiny_config.R")
source("includes/load_funcs.R")
load_funcs(cedar_base_dir)

# use "secret" environment variables from posit cloud connect 
desrs <- Sys.getenv("desrs") 
class_lists <- Sys.getenv("class_lists") 
academic_studies <- Sys.getenv("academic_studies") 
degrees <- Sys.getenv("degrees") 
forecasts <- Sys.getenv("forecasts") 

message("loading data...")
courses <- readRDS(url(desrs))
students <- readRDS(url(class_lists))
academic_studies <- readRDS(url(academic_studies))
degrees <- readRDS(url(degrees))
forecasts <- readRDS(url(forecasts))

# message("loaded students with ",nrow(students)," rows.")

# filter courses
# opt <- list()
# opt[["shiny"]] <- TRUE
# opt[["uel"]] <- TRUE
# opt[["level"]] <- "lower" # for faster testing
# opt[["dept"]] <- "HIST"


# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "CEDAR",
  
  nav_panel(
    title = "Enrollment", 
    
    fluidRow(
      column(2,
             selectizeInput(
               inputId = "enrl_campus",
               label = "Select Campus", 
               multiple = TRUE,
               choices = sort(unique(courses$CAMP))),
      ),
      column(2,
             selectizeInput(
               inputId = "enrl_college",
               label = "Select College", 
               multiple = TRUE,
               choices = sort(unique(courses$COLLEGE))),
      ),
      column(2,
             selectizeInput(
               inputId = "enrl_dept",
               label = "Select Department", 
               multiple = TRUE,
               choices = sort(unique(courses$DEPT))),
      ),
      column(2,
             selectizeInput(
               inputId = "enrl_course",
               label = "Select Course", 
               multiple = TRUE,
               choices = NULL),
      ),
      column(2,
             selectizeInput(
               inputId = "enrl_term_type",
               label = "Select Term Type", 
               multiple = TRUE,
               choices = sort(unique(courses$term_type))),
      ),
      
      column(2,
             selectizeInput(
               inputId = "enrl_agg_by",
               label = "Select Columns", 
               multiple = TRUE,
               choices = c("SUBJ_CRSE","DEPT","TERM","PT","INST_METHOD")),
      )
    ), # end fluidRow 
    fluidRow(
      column(2,
             selectInput(
               inputId = "enrl_term",
               label = "Term", 
               multiple = TRUE,
               choices = sort(unique(courses$TERM))),
      ),
      column(2,
             selectInput(
               inputId = "enrl_pt",
               label = "Part of Term", 
               multiple = TRUE,
               choices = sort(unique(courses$PT))),
      ),
      column(2,
             selectInput(
               inputId = "enrl_im",
               label = "Method",
               multiple = TRUE,
               choices = sort(unique(courses$INST_METHOD))),
      ),
      column(2,
             selectInput(
               inputId = "enrl_level",
               label = "Level", 
               multiple = TRUE,
               choices = sort(unique(courses$INST_METHOD))),
      ),
      column(2,
             actionButton("enrl_button",label = "Refresh table"),
      )
    ), # end fluidRow
    
    
    card( 
      card_header("Enrollment Summary"),
      DT::DTOutput("enrl_summary")
    )
    
  ), # end nav_panel for enrollment
  
  nav_panel(
    title = "Course Reports", 
    
    fluidRow(
      column(8,
             selectizeInput(
               inputId = "cr_course",
               label = "Select Course", 
               multiple = FALSE,
               choices = NULL),
      ),
      column(4,
             actionButton("cr_button",label = "Create Report"),
      )
    ), # end fluidRow
    
    fluidRow(
      column(12,
             #DTOutput("cd_enrls")
             uiOutput("cd_enrls")
      )
    ) # end fluidRow
    
    # card(
    #   min-width = "300px",
    #   card_header("Type Summary"),
    #   
    # ),
    # 
    # card( 
    #   card_header("Courses in Common"),
    #   
    # )
  ), # end nav panel course_report
  
  
  
  nav_panel(
    title = "Seatfinder", 
    
    fluidRow(
      column(2,
             selectizeInput(
               inputId = "sf_campus",
               label = "Select Campus", 
               multiple = TRUE,
               choices = sort(unique(courses$CAMP))),
      ),
      column(2,
             selectizeInput(
               inputId = "sf_college",
               label = "Select College", 
               multiple = TRUE,
               choices = sort(unique(courses$COLLEGE))),
      ),
      column(2,
             selectizeInput(
               inputId = "sf_dept",
               label = "Select Department", 
               multiple = TRUE,
               choices = sort(unique(courses$DEPT))),
      ),
      column(2,
             selectInput(
               inputId = "sf_pt",
               label = "Part of Term", 
               choices = sort(unique(courses$PT))),
      ),
      column(2,
             selectInput(
               inputId = "sf_im",
               label = "Method",
               multiple = TRUE,
               choices = sort(unique(courses$INST_METHOD))),
      ),
      column(2,
             selectInput(
               inputId = "sf_level",
               label = "Level", 
               choices = sort(unique(courses$level))),
      ),
      column(2,
             actionButton("sf_button",label = "Refresh table"),
      )
    ), # end fluidRow
    
    
    tabsetPanel(
      tabPanel("Summary", DT::DTOutput("type_summary")),
      tabPanel("Common", DT::DTOutput("courses_common"))
    )
    
  ), # end nav panel seatfinder
  
  
  nav_panel(
    title = "Regstats", 
    
    fluidRow(
      column(3,
             selectInput(
               inputId = "rs_term",
               label = "Term", 
               multiple = TRUE,
               choices = sort(unique(courses$TERM))),
      ),
      column(3,
             selectInput(
               inputId = "rs_pt",
               label = "Part of Term", 
               choices = sort(unique(courses$PT))),
      ),
      column(3,
             selectInput(
               inputId = "rs_im",
               label = "Method",
               multiple = TRUE,
               choices = sort(unique(courses$INST_METHOD))),
      ),
      column(3,
             selectInput(
               inputId = "rs_level",
               label = "Level", 
               choices = sort(unique(courses$level))),
      ),
      column(3,
             actionButton("rs_button",label = "Refresh table"),
      )
    ), # end fluidRow
    
    fluidRow(
      column(2,
             numericInput(
               inputId = "sf_sd_buffer",   # % of standard deviation before flagging as concern
               label = "DF buffer",
               min = 0,
               max = 2,
               value = cedar_regstats_thresholds[["sd_buffer"]])
      ),
      column(2,
             numericInput(
               inputId = "sf_min_count",   # # min number of students in a course before we flag as a concern
               label = "Min Students",
               min = 0,
               max = 50,
               value = cedar_regstats_thresholds[["min_count"]])
      ),
      column(2,
             numericInput(
               inputId = "sf_min_impacted",
               label = "Min Impacted",   # min difference b/w enrollment and mean (= number of students affected)
               value = cedar_regstats_thresholds[["min_impacted"]])
      ),
      column(2,
             numericInput(
               inputId = "sf_min_pct_sd",
               label = "Pct SD",   # percent of students outside the mean compared to standard deviation
               value = cedar_regstats_thresholds[["min_pct_sd"]])
      ),
      
      column(2,
             numericInput(
               inputId = "sf_min_squeeze",   # squeeze is ratio of avail seats to  mean attrition
               label = "Method",
               value = cedar_regstats_thresholds[["min_squeeze"]])
      ),
      column(2,
             numericInput(
               inputId = "sf_min_wait",   # min number of students on waitlist before being flagged
               label = "Min Waiting", 
               value = cedar_regstats_thresholds[["min_wait"]])
      ),
      column(2,
             actionButton("sf_button",label = "Refresh table"),
      )
    ), # end fluidRow
    
    
    fluidRow(
      column(12,
             #DTOutput("cd_enrls")
             uiOutput("rs_enrls")
      )
    ) # end fluidRow
  
  ) # end nav panel regstats
) # end ui


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'enrl_course', choices = sort(unique(courses$SUBJ_CRSE)), server = TRUE)
  updateSelectizeInput(session, 'cr_course', choices = sort(unique(courses$SUBJ_CRSE)), server = TRUE)
  
  
  ##### ENROLLMENT #####
  observeEvent(input$enrl_button,{
    
    output$enrl_summary = DT::renderDataTable({
      #req(input$course_selection)
      opt <- list()
      opt$status <- "A"
      opt$uel <- TRUE
      opt[["group_cols"]] <- input$enrl_agg_by
      opt[["campus"]] <- input$enrl_campus
      opt[["college"]] <- input$enrl_college
      opt[["dept"]] <- input$enrl_dept
      opt[["pt"]] <- input$enrl_pt
      opt[["method"]] <- input$enrl_method
      opt[["term"]] <- input$enrl_term
      opt[["level"]] <- input$enrl_level
      opt[["course"]] <- input$enrl_course
      
      print(opt)
      
      data <- get_enrl(courses,opt,input$enrl_agg_by)  
    })
    
  },ignoreInit = TRUE)
  
  
  #Change country input optoins if continents change
  observeEvent(input$enrl_dept, {
    
    #Filter countries based on current continent selection
    deptsToShow = courses %>% 
      filter(DEPT %in% input$enrl_dept) %>% ungroup() %>% select(SUBJ_CRSE) %>% arrange(SUBJ_CRSE)
    
    #Update the actual input
    updateSelectInput(session, "enrl_course", choices = deptsToShow)
    
  })
  
  
  ##### COURSE REPORT #####
  observeEvent(input$cr_button,{
    opt <- list()
    opt[["shiny"]] <- TRUE
    opt[["course"]] <- input$cr_course
    
    course_data <- create_course_report(students,courses,opt)
    
    html_file <- "Rmd/output.html"
    
    #output$cd_enrls <- renderDataTable({
    output$rs_enrls <- renderUI({
      tags$iframe(src = base64enc::dataURI(file=html_file, mime="text/html; charset=UTF-8"),style="height:100vh; width:100%")
    })
    print("done rendering ui.")
  },ignoreInit = TRUE)
  
  
  ##### SEATFINDER #####
  observeEvent(input$sf_button,{
    #RV$data<-myCustomFunction(RV$data)
    
    # get seatfinder data
    opt <- list()
    opt[["campus"]] <- input$sf_campus
    opt[["college"]] <- input$sf_college
    opt[["term"]] <- input$sf_term
    opt[["pt"]] <- input$sf_pt
    opt[["im"]] <- input$sf_im
    opt[["level"]] <- input$sf_level
    courses_list <- seatfinder(students,courses,opt)
    
    output$type_summary = DT::renderDataTable({
      data <- courses_list[["type_summary"]]
    })
    
    output$courses_common = DT::renderDataTable({
      data <- courses_list[["courses_common"]]
    })
    
  },ignoreInit = TRUE)
  
  
  ##### REGSTATS #####
  observeEvent(input$rs_button,{
    #RV$data<-myCustomFunction(RV$data)
    
    # get regstats data
    opt <- list()
    opt[["shiny"]] <- TRUE
    opt[["term"]] <- "202510"
    opt[["pt"]] <- input$rs_pt
    opt[["im"]] <- input$rs_im
    opt[["level"]] <- input$rs_level
    opt[["thresholds"]][["buffer"]] <- input$sf_buffer
    opt[["thresholds"]][["min_count"]] <- input$sf_min_count
    opt[["thresholds"]][["min_impacted"]] <- input$sf_min_impacted
    opt[["thresholds"]][["min_squeeze"]] <- input$sf_squeeze
    opt[["thresholds"]][["min_pct_sd"]] <- input$sf_min_pct_sd
    opt[["thresholds"]][["min_wait"]] <-  input$sf_min_wait
    
        
    flagged <- create_regstat_report(students,courses,opt)
    
    #html_file <- "/Users/fwgibbs/Dropbox/cedar/output/regstats-reports/html/regstats-202510-1.html"
    html_file <- "Rmd/output.html"
    #print(html_file)
    
    #output$cd_enrls <- renderDataTable({
    output$rs_enrls <- renderUI({
      tags$iframe(src = base64enc::dataURI(file=html_file, mime="text/html; charset=UTF-8"),style="height:100vh; width:100%")
    })
    print("done rendering ui.")
    
  },ignoreInit = TRUE)
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
