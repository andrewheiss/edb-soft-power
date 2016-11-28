#' ---
#' title: "Different cutpoints"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     code_folding: hide
#'     toc: yes
#'     toc_float: true
#'     toc_depth: 4
#'     highlight: pygments
#'     self_contained: no
#'     theme: flatly
#'     fig_height: 3
#'     fig_width: 4.5
#'     includes:
#'       after_body: ../html/add_home_link.html
#' ---

#+ message=FALSE
library(tidyverse)
library(pander)

cutpoints.ols <- read_csv(file.path(PROJHOME, "misc", "coefs_all.csv")) %>%
  arrange(type)

cutpoints.its <- read_csv(file.path(PROJHOME, "misc", "coefs_its.csv")) %>%
  arrange(type, year)

#' ## Expanded version of Table 4
#' 
#' This is Table 4 from the paper (OLS models), but with cutpoints at 2003, 2004, 2011, and 2012. The coefficients come from these models:
#' 
#' - [2005 and 2006](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series.html#models_for_all_countries_in_2001_report) (original; currently in paper)
#' - [2003 and 2004](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series_2003.html#models_for_all_countries_in_2001_report)
#' - [2011 and 2012](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series_2011.html#models_for_all_countries_in_2001_report)
#' 

#+ results="asis"
pandoc.table(cutpoints.ols, split.tables=Inf)

#' ## Expanded version of Table 5
#' 
#' And this is Table 5 from the paper (ITS models), with coefficients for the year × ranked interaction term. The coefficients come from these models:
#' 
#' - [2005 and 2006](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series.html#simple:_in_since_2001) (original; currently in paper)
#' - [2003 and 2004](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series_2003.html#simple:_in_since_2001)
#' - [2011 and 2012](http://stats.andrewheiss.com/judith/EDB/analysis/interrupted_time_series_2011.html#simple:_in_since_2001)
#' 

#+ results="asis"
pandoc.table(cutpoints.its, split.tables=Inf)
