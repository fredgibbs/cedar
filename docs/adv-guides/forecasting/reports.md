---
title: Forecast Reports
layout: home
parent: Forecasting
nav_order: 10
---

# Forecast Reports
Forecasting functionality simply does the forecast calculation; examining the forecasts in historical context is the job of the forecast reporting functionality.


## Accuracy and demand 
When looking at forecast output, the accuracy columns report the extent to which forecasts matched prior enrollments (compared to all registrations, enrollment at census, and end-of-term enrollment). Despite the "accuracy" label, this doesn't necessarily indicate how accurate the forecasts were because enrollments are throttled by many variables, including cap sizes, instructor availability, historical precedent, funding, etc. 

Obviously, forecasts can be off for many reasons. BUT, seeing consistently higher predictions than enrollments isn't necessarily an overestimate, but perhaps a suggestion that there is more demand than being met.  

The forecasting function uses previous enrollment data (one term or one semester depending on the method) to forecast enrollment for specified courses in future semesters. You can do individual course forecasts by course and “target” term to forecast for. You can also use set course lists and term lists (defined in lists.R). You can use the [regstats function](../../regstats) to automatically flag courses to forecast for given recent enrollment shifts.   
 


### How do I view all forecasting results? 
This provides too much information, but if you want to see all current forecast data, for ALL courses and terms ever forecasted: 
- `Rscript cedar.R -f forecast-report` 

The --output csv flag will create two CSV files in the output/csv folder:  
- **forecast-short.csv** provides an executive summary view of forecasting stats 
- **forecast-long.csv** shows all the accuracy and evaluation calculations  


### How can I see forecast data for specific terms and/or courses? 
Use the standard filtering flags by course AND/OR term OR term type: 
- `Rscript cedar.R -f forecast-report -c 'MATH 1215'` 
- `Rscript cedar.R -f forecast-report -t 202510` 
- `Rscript cedar.R -f forecast-report -t fall` 
- `Rscript cedar.R -f forecast-report -c ‘ENGL 1120’ -t summer` 


### How can I get an HTML file of forecast data to share? 
- `Rscript cedar.R -f forecast-report -c 'MATH 1215' -output html` 

 
### How can I get an ASPX file of forecast data to share via OneDrive? 
- `Rscript cedar.R -f forecast-report -t summer --output aspx` 


### How can I get a CSV file of forecast data? 
- `Rscript cedar.R -f forecast-report -c 'MATH 1215' --output csv` 
- `Rscript cedar.R -f forecast-report -t 202560 --output csv` 

 
### What does a zero mean for a forecast? 
It usually means that course wasn't offered in the previous term_type 

 

### Why are there so many NAs for recommendations? 
Usually because there isn't enough data for the term_type. For instance, if you're trying to forecast for 202560, but the accuracy data and recommendations are all NAs, try forecasting for earlier summers with: 
- `Rscript cedar.R -f forecast -c forecasts -t tl_summers` 

Then, rerun forecast report with the term flag set to summer to see all summer data: 
- `Rscript cedar.R -f forecast-report -t summer` 

 
### What are all the forecast-report columns? 
- **enrolled**: MyReports final enrollment for that semester (all registered students) 
- **cl_total**: total number of students who registered for the course, including all drops (theory: the sheer number of registrations tell us something about demand apart from final or even census enrollments)  
- **de_mean**: mean number of drops before census date (de = drops_early) 
- **dl_mean**: mean number of drops after census date (de = drops_late) 
- **conduit**: forecasted enrollment from the “conduit” method 
- **major**: forecasted enrollment from the “major” method 
- **_accr columns**: how accurate the forecast was for terms with enrollment (with respect to enrl or cl enrollment totals) 
- **avg_ columns**: the average accuracy (with respect to enrl or cl enrollment totals) 
- **pref_enrl_method**: the method that seems to be most accurate with respect to final enrollment numbers 
- **pref_cl_method**: the method that seems to be most accurate with respect to the total number of registrations 
- **rec_sections**: the computed number of sections needed based on the number of forecasted students indicated by pref_method, and what seems to be the section cap size from last term type  
- **diff_fr_prev**:  difference in number of sections compared to a year prior. 

 

 

 

