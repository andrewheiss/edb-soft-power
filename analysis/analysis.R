#' ---
#' title: "EDB analysis"
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
library(countrycode)
library(pander)

knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120),  # For output
                      warning=FALSE)

source(file.path(PROJHOME, "lib", "functions.R"))
source(file.path(PROJHOME, "data", "clean_data.R"))

#' ## Country summaries
#' 
#' ### Number of countries in the report over time
#' 
#' 2009 had been behaving strangely in the data, so verify that the correct
#' number of countries appear in each year.
#' 
#' There seems to be some weirdness with the data, though. `num.in.report` (the
#' number of countries in a year where `sb_days` isn't missing) increases as
#' expected, and ranking kicks in in 2005, also as expected. However, not all
#' countries that have data on `sb_days` get ranked—in 2005, 143 of the 172
#' countries in the report are ranked. The number of reported and ranked
#' countries doesn't match until 2014.
#' 
#' Regardless, 2009 isn't the problem. 
#' 
countries.ranked.year <- edb.clean %>%
  group_by(year) %>%
  summarise(num.in.report = sum(!is.na(sb_days)),
            num.ranked = sum(!is.na(p_edb_rank)))

#+ results="asis"
pandoc.table(countries.ranked.year)

# ### List of countries in initial 2001 report
# 
# \* indicates country has an EDB reform committee by 2015
country.names <- edb.its %>%
  filter(in.2001 == 1) %>%
  group_by(ccode) %>%
  summarise(cow = unique(ccode)) %>%
  ungroup() %>%
  mutate(has.committee = ifelse(ccode %in% countries.with.edb.bureau$cowcode, "\\*", ""),
         Country = countrycode(cow, "cown", "country.name")) %>%
  mutate(Country = case_when(
    .$cow == 1001 ~ "Serbia",
    .$cow == 1005 ~ "Hong Kong",
    TRUE ~ .$Country
  )) %>%
  arrange(Country) %>%
  mutate(Country = paste0(Country, has.committee)) %>%
  select(Country)

# results="asis"
# pandoc.table(matrix(c(country.names$Country, rep("", 2)), ncol=4),
#              split.tables=Inf)

#' ## Bumps in rankings
#' 
#' When averaging the changes in rankings after 1 and 2 years, the expected
#' trend is backward. It seems that doing a reform like registering property
#' makes you drop 6 places in the rankings. Pretty much every reform leads to
#' an increase in the rankings in the next year. Which tells the story that
#' reform doesn’t help. Which doesn’t make sense.
#' 
#' This effect seems to be driven by countries like Rwanda, which in 2009 made
#' 1 registering property reform and moved from 67th place to 139th place one 
#' year later (and 150th place two years later). This gives it a 
#' change-per-reform value of 72, which cancels out the improvements made by 
#' other countries.
#' 
#' We thought that this might be because the number of countries in the
#' rankings changes over time—perhaps a country could drop significantly in the
#' rankings one year because 50 new countries were added to the rankings, so
#' the country worsens without doing anything. To address this, I rescaled the
#' rankings to a 0-100 scale (0 = best).
#' 
#' But, as seen in the figure below, this isn't the case. Rwanda in 2009 moved
#' from 36.26 normalized to 76.67 normalized the next year (and 84.18 two years
#' later), resulting in a change-per-reform value of 40.4 normalized positions.
#' 
change.rankings <- edb.clean %>%
  select(ccode, country_name, year, p_edb_rank, has.bureau) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lag(p_edb_rank, 1),
         rank2 = lag(p_edb_rank, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0) %>%
  filter(year >= 2005)

change.rankings.norm <- edb.clean %>%
  select(ccode, country_name, year, p_edb_rank, p_edb_rank_normalized, has.bureau) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lag(rank0, 1),
         rank2 = lag(rank0, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0,
         rank0_norm = p_edb_rank_normalized,
         rank1_norm = lag(rank0_norm, 1),
         rank2_norm = lag(rank0_norm, 2),
         change1_norm = rank1_norm - rank0_norm,
         change2_norm = rank2_norm - rank0_norm) %>%
  filter(year >= 2005)

edb.reforms.rankings.bumps <- edb.reforms %>%
  filter(!str_detect(reform.type, "lag")) %>%
  group_by(ccode, year, reform.type.clean) %>%
  summarise(num.reforms = sum(reform.num.no.na)) %>%
  left_join(change.rankings.norm, by=c("ccode", "year")) %>%
  filter(num.reforms != 0) %>%
  mutate(change.per.reform1 = change1 / num.reforms,
         change.per.reform2 = change2 / num.reforms,
         change.per.reform1_norm = change1_norm / num.reforms,
         change.per.reform2_norm = change2_norm / num.reforms) 

edb.reforms.rankings.bumps.summarized <- edb.reforms.rankings.bumps %>%
  group_by(reform.type.clean, has.bureau) %>%
  summarise(avg.bump_norm = mean(change.per.reform1_norm, na.rm=TRUE),
            avg.bump = mean(change.per.reform1, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(desc(has.bureau), desc(avg.bump)) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=unique(reform.type.clean), 
                                    ordered=TRUE))

plot.ranking.bumps <- ggplot(edb.reforms.rankings.bumps.summarized,
                             aes(x=avg.bump, y=reform.type.clean, colour=has.bureau)) +
  geom_point() +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Average change in EDB rankings one year after reform", y="Type of reform",
       title="Changes in rankings per reform",
       subtitle="After 1 year") +
  theme_edb()
plot.ranking.bumps

plot.ranking.bumps_norm <- ggplot(edb.reforms.rankings.bumps.summarized,
                             aes(x=avg.bump_norm, y=reform.type.clean, colour=has.bureau)) +
  geom_point() +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Average change in EDB rankings one year after reform (normalized rankings)",
       y="Type of reform",
       title="Changes in normalized rankings per reform",
       subtitle="After 1 year") +
  theme_edb()
plot.ranking.bumps_norm


#' But, the more I look at this, the more I think that we have it backwards in
#' the paper. Positive correlation coefficients mean that *rising* in the
#' rankings is associated with the number of reforms. Here's the original
#' Figure 2 from the paper:
#' 
edb.reforms.rankings <- edb.reforms %>%
  group_by(ccode, year) %>%
  summarise(num.reforms = sum(reform.num.no.na)) %>%
  left_join(select(edb.clean, ccode, year, p_edb_rank, has.bureau), by=c("ccode", "year")) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lag(p_edb_rank, 1),
         rank2 = lag(p_edb_rank, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0) %>%
  filter(year >= 2005)

reform.rankings.type <- edb.reforms %>%
  select(ccode, year, has.bureau, reform.type, reform.type.clean, reform.num.no.na) %>%
  left_join(edb.reforms.rankings, c("ccode", "year", "has.bureau")) %>%
  filter(year >= 2005)

reform.rankings.type.summary <- reform.rankings.type %>%
  filter(!is.na(reform.type.clean)) %>%
  group_by(reform.type.clean, has.bureau) %>%
  do(change = summary.corr(.$change1, .$reform.num.no.na),
     reforms = summary.corr(.$reform.num.no.na))

reform.rankings.type.cor <- reform.rankings.type.summary %>%
  unnest(change) %>%
  arrange(desc(has.bureau), correlation) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=unique(reform.type.clean), 
                                    ordered=TRUE))

plot.ranking.cor <- ggplot(reform.rankings.type.cor, 
                           aes(x=correlation, y=reform.type.clean, colour=has.bureau)) +
  geom_point() +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Correlation between reform and movement in rankings", y="Type of reform",
       title="Correlation between reform and movement in rankings") +
  theme_edb()
plot.ranking.cor


#' And here are the scatterplots for each of these types of reforms. All of
#' them have a positive relationship, showing that as countries undertake more
#' reforms, they move upward in the rankings. This is the same phenomenon as
#' Rwanda—more reforms are associated with *increases* in rankings.
#' 
plot.reform.rankings <- reform.rankings.type %>%
  select(reform.type.clean, reform.num.no.na, change1) %>%
  filter(complete.cases(.)) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=rev(levels(reform.rankings.type.cor$reform.type.clean)), 
                                    ordered=TRUE))

#+ fig.width=8, fig.height=4
ggplot(plot.reform.rankings, aes(x=reform.num.no.na, y=change1)) +
  geom_point(size=0.5, alpha=0.5) + 
  geom_smooth(method="lm") +
  labs(x="Number of reforms", y="Change in rankings (negative = good)") +
  theme_edb() +
  facet_wrap(~ reform.type.clean)
