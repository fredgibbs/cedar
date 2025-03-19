source("renv/activate.R")

# if using Rstudio interactively, load req libraries, config files, functions, and data
if (interactive()) {
  message("loading libraries...")
  pacman::p_load(tidyverse, dplyr, fs, optparse, plotly)
  
  message("loading external functions...")
  source("includes/config.R")
  source("includes/load_funcs.R")
  load_funcs("./")
  
  resolve_conflicts() # defined in misc_funcs, loaded by load_funcs
  
  load_global_data()
  
}

