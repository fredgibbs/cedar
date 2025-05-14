---
title: CLI guide
parent: Guides 
layout: home
---

# CEDAR in RStudio

CEDAR is in GitHub as an R Project. Opening the project loads all data files once, which makes it faster than doing quick commands on via the CLI. 

Opening The same basic ideas from the terminal page apply to running code in the Studio console, except you need to call the cedar.R function with R function syntax.

For instance, in the terminal:
Rscript cedar.r -f enrl -t 202580

In RStudio, this would be:
cedar("enrl", t=202580, c="SUBJ_CRSE")