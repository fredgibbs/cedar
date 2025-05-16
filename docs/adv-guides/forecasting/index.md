---
title: Forecasting
layout: home
parent: Advanced Guides
nav_order: 1
---

## CEDAR Forecasting Introduction

## Data updates 
Before forecasting, it's best to update DESR and Class List data from MyReports. See the [data page](../../data) for details on how to do this.
 
 
## Methods
For a given "target" course and term: 

### Conduit 
- Get student IDs for students in previous (1 year ago) target course 
- Identify what courses students are coming from before taking the given course. These are the "conduit" courses. 
- Identify how enrollments in the conduit courses changed from a year prior to target course. 
- Apply that pct diff (weighted by the amount of their mean contribution) to mean target course enrollments 

 

### Major 
Instead of just looking at course enrollments as the CONDUIT method does, this MAJOR method looks at how MAJORS changed from prev conduit term to conduit term (the conduit term is just the term before the target term; the prev conduit term is one year prior to maintain term type) 

- Find change in majors_percentage for previous target course
- Apply the change in majors percentage to prev_target_term enrollments 
- Compute how much that will contribute to target term enrollment based composition of majors in target course 
- Requires a course to have been offered a year previously 


