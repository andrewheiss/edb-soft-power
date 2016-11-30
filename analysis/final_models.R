#' ---
#' title: "Final models for paper"
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
#'     fig_height: 4
#'     fig_width: 6
#'     includes:
#'       after_body: ../html/add_home_link.html
#' ---

#+ message=FALSE
library(tidyverse)
library(stringr)
library(broom)
library(pander)

knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120),  # For output
                      warning=FALSE)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "lib", "functions.R"))
source(file.path(PROJHOME, "data", "clean_data.R"))

# Define all the models that need to be run
models.to.run <- expand.grid(year = 2003:2013,
                             outcome = c("sb_proced", "sb_days_ln",
                                         "sb_cost_ln", "sb_capital_ln"),
                             grouping = c("All countries", 
                                          "No EDB reform committee", 
                                          "Special EDB reform committee"),
                             stringsAsFactors=FALSE)

# Splitting the data into separate data frame columns is trivial with
# group_by() + nest(), but putting the whole unsplit data frame into a column
# is trickier, since nest() needs some grouping to work. So I have to make two
# separate data frames with data frame columns and then combine them.
dfs.split <- edb.its.2001 %>%
  group_by(has.bureau) %>%
  nest() %>%
  mutate(has.bureau = as.character(has.bureau)) %>%
  rename(grouping = has.bureau)

df.full <- edb.its.2001 %>%
  mutate(grouping = "All countries") %>%
  group_by(grouping) %>%
  nest()

# Need to use right_join with the nested data frame columns first, since
# left_join()ing to models.to.run apparently breaks the nested column and makes
# R crash?
models.to.run.full <- bind_rows(dfs.split, df.full) %>%
  right_join(models.to.run, by="grouping")


#' **Important**: All models only use countries included in the original 2001
#' report.
#' 
#' ## Difference models
#' 
#' These difference models use the lag of the outcome variable and a year
#' indicator variable to determine the change in outcome variable.
#' 
#' Formally, this is defined as:
#' 
#' $$ y_t = \beta_0 + \beta_1 y_{t-1} + \beta_2 X + \epsilon $$
#' 
#' - *t* = Year
#' - *X* = {1 if *t* = cutpoint year, 0 otherwise}
#' - β~0~ = Constant
#' - β~1~ = Change in outcome from the previous year
#' - β~2~ = Effect of the event
#' 
#' Or an example R formula:
#' 
#'     sb_proced ~ sb_proced_lag + ranked.2005
#' 
run.lagged <- function(outcome, year, df) {
  outcome.lag <- paste0(outcome, "_lag")
  year.variable <- paste0("ranked.", year)
  
  form <- as.formula(paste0(outcome, " ~ ", outcome.lag, " + ", year.variable))
  
  lm(form, data=df)
}

# Run all the models within the data frame (so magic)
models.lagged <- models.to.run.full %>%
  mutate(model = pmap(.l=list(outcome, year, data), run.lagged),
         robust.se = pmap(.l=list(model, data, "ccode"), robust.clusterify),
         glance = model %>% map(broom::glance),
         # tidy = model %>% map(broom::tidy, conf.int=TRUE),
         tidy.robust = robust.se %>% map(~ broom::tidy(.$coef)))

coefs.lagged.all <- models.lagged %>%
  unnest(tidy.robust) %>%
  filter(str_detect(term, "ranked")) %>%
  mutate(value = paste0(round(estimate, 3), p.stars(p.value)),
         term = str_replace(term, "\\.\\d+TRUE", "")) %>%
  select(-c(estimate, std.error, statistic, p.value)) %>%
  spread(outcome, value) %>%
  select(Subset = grouping, Year = year, 
         Procedures = sb_proced, `Cost (log)` = sb_cost_ln, 
         `Days (log)` = sb_days_ln, `Capital (log)` = sb_capital_ln)

#' ### Table 4 in paper
#' 
#' Coefficients from just 2005 and 2006
#' 
#+ results="asis"
pandoc.table(coefs.lagged.all %>% 
               filter(Year %in% c(2005, 2006)),
             caption='Summary of β~2~ coefficients (i.e. "ranked.200x") for difference models in Tables A.1-6 in the appendix')

#' ### Cutpoint at every possible year
#' 
#' To check robustness, this table includes coefficients for models from 2003–2013
#' 
#+ results="asis"
pandoc.table(coefs.lagged.all,
             caption='Summary of β~2~ coefficients (i.e. "ranked.200x") for difference models for all years')


#' ## Interrupted time series models
#' 
#' These interrupted times series models use an interaction term of the years
#' since an intervention and the intervention itself to determine the change in
#' slope after the intervention.
#' 
#' $$ y_t = \beta_0 + \beta_1 T + \beta_2 X_t + \beta_3 (T \times X_t) + \epsilon $$
#' 
#' - *t* = Year
#' - *T* = Years before/after cutpoint
#' - *X* = {1 if *t* = cutpoint year, 0 otherwise}
#' - β~0~ = Constant: pre-period intercept - baseline pre intervention
#' - β~1~ = pre-period slope - baseline time trend - level of increase prior to intervention
#' - β~2~ = immediate effect of the event - change in intercept at point of experiment
#' - β~3~ = change in slope after the experiment - what happens after
#' 
#' Or an example R formula:
#' 
#'     sb_proced ~ year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005
#' 

run.its <- function(outcome, year, df) {
  year.variable <- paste0("ranked.", year)
  year.centered <- paste0("year.centered.", year)
  
  form <- as.formula(paste0(outcome, " ~ ", year.centered, " + ", year.variable,
                            " + ", year.centered, " * ", year.variable))
  
  lm(form, data=df)
}

models.its <- models.to.run.full %>%
  mutate(model = pmap(.l=list(outcome, year, data), run.its),
         robust.se = pmap(.l=list(model, data, "ccode"), robust.clusterify),
         glance = model %>% map(broom::glance),
         tidy.robust = robust.se %>% map(~ broom::tidy(.$coef)))

coefs.its.all <- models.its %>%
  unnest(tidy.robust) %>%
  filter(str_detect(term, ":")) %>%
  mutate(value = paste0(round(estimate, 3), p.stars(p.value)),
         term = str_replace(term, "(.+)\\.\\d+:(.+)\\.\\d+TRUE", "\\1 × \\2")) %>%
  select(-c(estimate, std.error, statistic, p.value)) %>%
  spread(outcome, value) %>%
  select(Subset = grouping, Year = year, 
         Procedures = sb_proced, `Cost (log)` = sb_cost_ln, 
         `Days (log)` = sb_days_ln, `Capital (log)` = sb_capital_ln)

#' ### Table 5 in paper
#' 
#' Coefficients from just 2005 and 2006
#' 
#+ results="asis"
pandoc.table(coefs.its.all %>% 
               filter(Year %in% c(2005, 2006)),
             caption='Summary of β~3~ coefficients (i.e. "year.centered.200x × ranked.200x") for ITS models in Tables A.7-12 in the appendix')

#' ### Cutpoint at every possible year
#' 
#' To check robustness, this table includes coefficients for models from 2003–2013
#' 
#+ results="asis"
pandoc.table(coefs.its.all,
             caption='Summary of β~3~ coefficients (i.e. "year.centered.200x × ranked.200x") for ITS models for all years')
