source("renv/activate.R")

message("loading libraries...")
pacman::p_load(tidyverse, dplyr, fs, optparse, plotly)
conflicted::conflicts_prefer(dplyr::filter())


message("loading external functions...")
#source("includes/config.R")
source("includes/load_funcs.R")
load_funcs("./")
.GlobalEnv$courses <- load_courses()
.GlobalEnv$students <- load_students()
