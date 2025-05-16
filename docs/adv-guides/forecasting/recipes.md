---
title: Forecasting Recipes
layout: home
parent: Forecasting
nav_order: 5
---

# Forecasting Recipes
Forecasting always requires a course or course list (as defined in lists.R). A term is not required, but forecast will default to tl_recents (defined in lists.R), which is all terms from Fall 2021. Going back several years helps evaluate the accuracy of forecasting for each course. 

Forecasting is currently only available through the CLI, and not the web tool.

### How do I forecast for a certain course and term? 
- `Rscript cedar.R -f forecast -t 202560 -c 'ENGL 1120'` 

### How do I forecast for just summers (or falls or springs)? 
- `Rscript cedar.R -f forecast -c 'ENGL 1120' -t tl_summers (or tl_falls or tl_springs)`

### How do I forecast for a small set of courses? 
- `Rscript cedar.R -f forecast -t 202560 -c 'ENGL 1110,ENGL 1120'` 


### How do I forecast for an arbitrary  set of courses? 
Use basic course filtering and reporting functionality (via enrl) to create a CSV file, which is a list of courses (these go in your output/csv folder.  You can also Forecast with -f forecast -c NAME_OF_CSV_FILE.csv (with an optional term parameter) 
- `Rscript cedar.R -f enrl -d ECON --uel -l lower --output csv` 
- `Rscript cedar.R -f forecast -c enrollments.csv -t 202510` 


### How can I forecast for other terms for courses already in the forecast table? 
Set the course parameter to "forecasts". Optionally specify whatever term (or term list) you want to forecast for. 
- `Rscript cedar.R -f forecast -c forecasts -t tl_falls` 

 
### How do I forecast with a conduit term that isn’t the term immediately before the target term?  
This is useful, for instance, if forecasting for summer in October before spring registration starts). Indicate the desired conduit term and the target term it should be used with as a comma-separated pair. For instance, if you want to forecast summer 2025 based on Fall 2024 data (rather than Spring 2025 data as is default behavior): 
`Rscript cedar.R -f forecast -c 'MATH 1220' --forecast_conduit_term 202480,202560` 
