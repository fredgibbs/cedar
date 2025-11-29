# generic Rmd report creator
create_report <- function(opt, d_params) {
  message("[reporting.R] Welcome to create_report!")

  output <- opt$output
  if (!is.null(output)) {
    message("[reporting.R] Setting output format to: ", output)
    prefix <- ifelse (output=="aspx","aspx/","html/")
    suffix <- ifelse (output=="aspx","-report.aspx",".html")
  } else {
    message("[reporting.R] opt$output is NULL; defaulting to HTML output...")
    prefix <- "html/"
    suffix <- ".html"
  }

  message("[reporting.R] d_params$output_filename set to: ", d_params$output_filename)
  message("[reporting.R] d_params$output_dir_base set to: ", d_params$output_dir_base)


  # CAREFUL: the paths for the output file are relative to the Rmd file, not the current working directory
  fixed_filename <- gsub(" ", "_", d_params$output_filename)
  output_filename <- paste0(d_params$output_dir_base,prefix,fixed_filename,suffix)

  message("[reporting.R] Working dir: ",getwd())
  message("[reporting.R] Output file set to: ", output_filename)

  if (is_docker()) {
    app_root <- normalizePath(file.path(getwd()))
    message("[reporting.R] is_docker: ", is_docker())
    message("[reporting.R] app_root: ", app_root)
    message("[reporting.R] getwd(): ", getwd())

    www_dir <- file.path(app_root, "www")
    data_dir <- file.path(app_root, "data")
    message("[reporting.R] ensuring directories exist... www=", www_dir, ", data=", data_dir)
    if (!dir.exists(www_dir)) { dir.create(www_dir, recursive = TRUE); message("[reporting.R] created ", www_dir) } else { message("[reporting.R] exists ", www_dir) }
    if (!dir.exists(data_dir)) { dir.create(data_dir, recursive = TRUE); message("[reporting.R] created ", data_dir) } else { message("[reporting.R] exists ", data_dir) }

    # create output file path based on www_dir
    # save to www for rendering in Shiny app
    output_filepath <- file.path(www_dir, paste0(d_params$output_filename, ".html"))
    message("[reporting.R] target output: ", output_filepath)

    message("[reporting.R] rendering Rmd...")
    rmd_output <- rmarkdown::render(d_params$rmd_file,
                      output_file = output_filepath,
                      params = d_params)
    
    # copy file from www dir to /srv/shiny-server/cedar/data/
    # necessary for file persistence across docker restarts
    src_file <- output_filepath
    dest_file <- file.path(data_dir, paste0(d_params$output_filename, ".html"))
    message("[reporting.R] Copying from: ", src_file)
    message("[reporting.R] Copying to: ", dest_file)
    
    copy_result <- tryCatch({
      file.copy(src_file, dest_file, overwrite = TRUE)
    }, error = function(e) {
      message("[reporting.R] Copy failed: ", e$message)
      FALSE
    })
    
    if (copy_result) {
      message("[reporting.R] Copy complete.")
    } else {
      message("[reporting.R] Copy operation failed!")
    }

  } # end docker rendering
  else { # non-docker rendering
    message("[reporting.R] Not in Docker mode, rendering in CLI mode...")
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc) # from config.R

    rmd_output <- rmarkdown::render(d_params$rmd_file,
                    output_file = output_filename,
                    params = d_params)
  }

  message("[reporting.R] Done rendering. Returning rmd_output as: ", rmd_output)

  return (rmd_output)
} # end create_report
