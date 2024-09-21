cedar_base_dir <- "FULL PATH TO YOUR CEDAR DIRECTORY" # this should end in /cedar/

# recommended to keep as is, but you can specify other folders you've created
cedar_output_dir <- paste0(cedar_base_dir,"output/")
cedar_data_dir <- paste0(cedar_base_dir,"data/")

# if you want to save the MyReports excel files you will download, update the following:
cedar_data_archive_dir <- "FULL PATH TO WHERE YOU WANT TO STORE DOWNLOADED MYREPORT FILES" 
# set the above to NULL (no quotes) to skip archiving MyReports downloads

# define the current term
cedar_current_term <- 202480

# these term codes control how much data appears on dept-reports
cedar_report_start_term <- 201980
cedar_report_end_term <- 202460

cedar_report_palette <- "Spectral"

# use Rstudio's pandoc version
rstudio_pandoc <- "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools"