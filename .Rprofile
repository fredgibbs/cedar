message("Welcome to .Rprofile!")

# Check if we're in a Shiny context - if so, skip all setup (global.R will handle everything)
is_shiny_context <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) || 
                   exists("shiny", where = globalenv(), mode = "any") ||
                   any(grepl("shiny", search()))

if (is_shiny_context) {
  message("Shiny context detected - skipping all .Rprofile setup (global.R will handle everything)")
} else {
  # Handle renv activation for non-Shiny contexts
  if (file.exists("renv/activate.R")) {
    message("Found renv/activate.R. Activating renv...")
    source("renv/activate.R")
    message("Activated renv.")
  }

  # Load libraries, config files, functions, and data for interactive use
  message("checking if running interactively...")
  
  if (interactive()) {
    message(".Rprofile is running interactively in RStudio.")
    message("Loading libraries...")
    pacman::p_load(tidyverse, dplyr, fs, qs, optparse, plotly)
    message("Loaded libraries.")

    message(".Rprofile is loading external functions...")
    
    source("config/config.R")
    message("Loaded config.R.")
    
    source("R/includes/load_funcs.R")
    message("Loaded load_funcs.R. About to run it...")
    load_funcs("./")
    message("Ran load_funcs().")

    resolve_conflicts() # defined in misc_funcs, loaded by load_funcs
    message("Ran resolve_conflicts().")

    load_global_data(opt=NULL)
    message("Ran load_global_data().")
  } else {
    message(".Rprofile is running outside of RStudio.")
  }
}

message(".Rprofile has finished loading.")
