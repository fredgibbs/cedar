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
library(feather)

source("includes/shiny_config.R")
source("includes/load_funcs.R")
load_funcs(cedar_base_dir)

# external source loading
# source("https://raw.githubusercontent.com/fredgibbs/cedar/refs/heads/main/cones/enrl/enrl.R")
#load(url("https://www.dropbox.com/scl/fi/8jxi6y5goz44ub5vsbms9/degrees.Rda?rlkey=we9lgiqkw0sq6vjtds6tpr08v&st=ch13dtit&dl=1"))
#load(url("https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/EUsiRM6_Xr9Ng-ygeuWDvNwB75F2tZuHSa_nHSBrepsLnw?download=1"))

# classlists
# readRDS(url("https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/ERz_u5OXEjNFvbNGTbG7T1sBRxKp3QGehRVm_7HmhxZmPA?download=1"))


#load(url("https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/EcFs0ZAuy35IgjAQ593NAIwBcnocDZ1u6oLALNgVZRs30g?download=1"))
desrs <- Sys.getenv("desrs") 
load(url(desrs))
courses <- completed_and_ongoing_courses

# load students
classlists <- Sys.getenv("classlists") 
readRDS(url(classlists))


# filter courses
opt <- list()
opt[["uel"]] <- TRUE
opt[["level"]] <- "lower" # for faster testing
#courses <- load_courses(opt=NULL) 
courses <- courses %>%  filter_DESRs(opt) 



# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "CEDAR",
  
  nav_panel(title = "Enrollment", 
            
            #p("Enrollment controls and data"),
            
            fluidRow(
              column(2,
                     selectizeInput(inputId = "enrl_course",
                                 label = "Select Course", 
                                 multiple = TRUE,
                                 choices = sort(unique(courses$SUBJ_CRSE))),
              ),
              column(2,
                     selectizeInput(inputId = "enrl_dept",
                                    label = "Select Department", 
                                    multiple = TRUE,
                                    choices = sort(unique(courses$DEPT))),
              ),
              column(2,
                     selectizeInput(inputId = "enrl_term_type",
                                    label = "Select Term Type", 
                                    multiple = TRUE,
                                    choices = sort(unique(courses$term_type))),
              ),
              
              column(2,
                     selectizeInput(inputId = "enrl_agg_by",
                                    label = "Select Columns", 
                                    multiple = TRUE,
                                    choices = c("SUBJ_CRSE","DEPT","TERM","PT","INST_METHOD")),
              )
            ), # end fluidRow 1
            fluidRow(
              column(2,
                     selectInput(inputId = "enrl_term",
                                 label = "Term", 
                                 multiple = TRUE,
                                 choices = sort(unique(courses$TERM))),
              ),
              column(2,
                     selectInput(inputId = "enrl_pt",
                                 label = "Part of Term", 
                                 multiple = TRUE,
                                 choices = sort(unique(courses$PT))),
              ),
              column(2,
                     selectInput(inputId = "enrl_im",
                                 label = "Method",
                                 multiple = TRUE,
                                 choices = sort(unique(courses$INST_METHOD))),
              ),
              column(2,
                     selectInput(inputId = "enrl_level",
                                 label = "Level", 
                                 multiple = TRUE,
                                 choices = courses$level),
              ),
              column(2,
                     actionButton("enrl_button",label = "Refresh table"),
              )
            ), # end fluidRow
            
            
            card( 
              card_header("Enrollment Summary"),
              DT::DTOutput("enrl_summary")
            )
            
  ), # end nav_panel
  nav_panel(title = "Course Reports", 
            p("Course Reports"),
            
            fluidRow(
              column(8,
                     selectizeInput(inputId = "cr_course_selection",
                                    label = "Select Course", 
                                    multiple = TRUE,
                                    choices = sort(unique(courses$SUBJ_CRSE))),
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
            )

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
  
  
  
  nav_panel(title = "Seatfinder", 
            p("Seatfinder controls and data"),
            
            fluidRow(
              column(3,
                     selectInput(inputId = "pt",
                                 label = "Part of Term", 
                                 choices = sort(unique(courses$PT))),
              ),
              column(3,
                     selectInput(inputId = "im",
                                 label = "Method",
                                 multiple = TRUE,
                                 choices = sort(unique(courses$INST_METHOD))),
              ),
              column(3,
                     selectInput(inputId = "level",
                                 label = "Level", 
                                 choices = sort(unique(courses$level))),
              ),
              column(3,
                     actionButton("seatfinder_button",label = "Refresh table"),
              )
            ), # end fluidRow
            
            tabsetPanel(
              tabPanel("Summary", DT::DTOutput("type_summary")),
              tabPanel("Common", DT::DTOutput("courses_common"))
            )

  ), # end nav panel seatfinder
  
  
  nav_panel(title = "Regstats", 
            p("Regstats controls and data"),
            
            fluidRow(
              column(3,
                     selectInput(inputId = "rs_term",
                                 label = "Term", 
                                 multiple = TRUE,
                                 choices = sort(unique(courses$TERM))),
              ),
              column(3,
                     selectInput(inputId = "rs_pt",
                                 label = "Part of Term", 
                                 choices = sort(unique(courses$PT))),
              ),
              column(3,
                     selectInput(inputId = "rs_im",
                                 label = "Method",
                                 multiple = TRUE,
                                 choices = sort(unique(courses$INST_METHOD))),
              ),
              column(3,
                     selectInput(inputId = "rs_level",
                                 label = "Level", 
                                 choices = sort(unique(courses$level))),
              ),
              column(3,
                     actionButton("rs_button",label = "Refresh table"),
              )
            ), # end fluidRow
            
            fluidRow(
              column(12,
                     #DTOutput("cd_enrls")
                     uiOutput("rs_enrls")
              )
            )
            
            
  ) # end nav panel regstats
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  observeEvent(input$enrl_button,{
    
    output$enrl_summary = DT::renderDataTable({
      #req(input$course_selection)
      opt <- list()
      opt$status <- "A"
      opt$uel <- TRUE
      opt$aggregate <- input$enrl_agg_by
      opt[["dept"]] <- input$enrl_dept
      opt[["pt"]] <- input$enrl_pt
      opt[["method"]] <- input$enrl_method
      opt[["term"]] <- input$enrl_term
      print(str(opt[["term"]]))
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
  
  
  
  observeEvent(input$cr_button,{
    opt <- list()
    opt[["output"]] <- "html"
    opt[["course"]] <- input$cr_course_selection
    
    # create report and get filename
    # html_file <- create_course_report(students,courses,opt)
    #course_data <- get_course_data(students,courses,opt)
    html_file <- "/Users/fwgibbs/Dropbox/cedar/output/course-reports/html/ENGL_1120.html"
    #print(html_file)
    
    #output$cd_enrls <- renderDataTable({
    output$cd_enrls <- renderUI({
      #course_data[["enrls"]]
      #tags$h1("header")
      tags$iframe(src = base64enc::dataURI(file=html_file, mime="text/html; charset=UTF-8"),style="height:100vh; width:100%")
      
      #tags$iframe(src=paste0("file://",html_file))
      #print(mine)
      #mine
      })
    print("done rendering")
    
  },ignoreInit = TRUE)
  
  

  
  observeEvent(input$seatfinder_button,{
    #RV$data<-myCustomFunction(RV$data)
    
    # get seatfinder data
    opt <- list()
    opt$term <- "202410,202510"
    opt[["pt"]] <- input$pt
    opt[["im"]] <- input$im
    opt[["level"]] <- input$level
    courses_list <- seatfinder(students,courses,opt)
    
    output$type_summary = DT::renderDataTable({
      data <- courses_list[["type_summary"]]
    })
    
    output$courses_common = DT::renderDataTable({
      data <- courses_list[["courses_common"]]
    })
    
  },ignoreInit = TRUE)
  
  
  
  observeEvent(input$rs_button,{
    #RV$data<-myCustomFunction(RV$data)
    
    # get regstats data
    opt <- list()
    opt$term <- "202480,202510"
    opt[["pt"]] <- input$rs_pt
    opt[["im"]] <- input$rs_im
    opt[["level"]] <- input$rs_level
    flagged <- create_regstat_report(students,courses,opt)
    
    html_file <- "/Users/fwgibbs/Dropbox/cedar/output/regstats-reports/html/regstats-202510.html"
    #print(html_file)
    
    #output$cd_enrls <- renderDataTable({
    output$rs_enrls <- renderUI({
      tags$iframe(src = base64enc::dataURI(file=html_file, mime="text/html; charset=UTF-8"),style="height:100vh; width:100%")
    })
    print("done rendering")
    
  },ignoreInit = TRUE)
  
  
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
