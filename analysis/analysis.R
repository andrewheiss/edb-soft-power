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
library(ggstance)
library(gridExtra)

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
#' No anomalies here.
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
#' Every reform leads to a negative change in rankings (which is good) in the
#' following year for countries with special EDB reform committees. For
#' instance, doing an enforcing contracts reform will move a country down (i.e.
#' to a better position) nearly 6 positions.
#' 
#' This is also the case when normalizing rankings to a 0-100 scale to account
#' for the changing number of rank positions over time.
#' 
change.rankings <- edb.clean %>%
  select(ccode, country_name, year, p_edb_rank, has.bureau) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lead(p_edb_rank, 1),
         rank2 = lead(p_edb_rank, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0) %>%
  filter(year >= 2005)

change.rankings.norm <- edb.clean %>%
  select(ccode, country_name, year, p_edb_rank, p_edb_rank_normalized, has.bureau) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lead(rank0, 1),
         rank2 = lead(rank0, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0,
         rank0_norm = p_edb_rank_normalized,
         rank1_norm = lead(rank0_norm, 1),
         rank2_norm = lead(rank0_norm, 2),
         change1_norm = rank1_norm - rank0_norm,
         change2_norm = rank2_norm - rank0_norm) %>%
  filter(year >= 2005)

edb.reforms.rankings.bumps <- edb.reforms %>%
  filter(!str_detect(reform.type, "lead")) %>%
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
            stderr.bump_norm = sd(change.per.reform1_norm, na.rm=TRUE) / 
              sqrt(length(change.per.reform1_norm)),
            lower_norm = avg.bump_norm + (qnorm(0.025) * stderr.bump_norm),
            upper_norm = avg.bump_norm + (qnorm(0.975) * stderr.bump_norm),
            avg.bump = mean(change.per.reform1, na.rm=TRUE),
            stderr.bump = sd(change.per.reform1, na.rm=TRUE) / 
              sqrt(length(change.per.reform1)),
            lower = avg.bump + (qnorm(0.025) * stderr.bump),
            upper = avg.bump + (qnorm(0.975) * stderr.bump)) %>%
  ungroup() %>%
  arrange(desc(has.bureau), desc(avg.bump)) %>%
  filter(!is.na(reform.type.clean)) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=unique(reform.type.clean), 
                                    ordered=TRUE))

plot.ranking.bumps <- ggplot(edb.reforms.rankings.bumps.summarized,
                             aes(x=avg.bump, y=reform.type.clean, colour=has.bureau)) +
  geom_vline(xintercept=0, colour="darkred", size=0.5) +
  geom_pointrangeh(aes(xmin=lower, xmax=upper), size=0.5,
                   position=position_dodgev(0.5)) +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Average change in EDB rankings one year after reform", y="Type of reform",
       title="Changes in rankings per reform",
       subtitle="After 1 year") +
  theme_edb()
plot.ranking.bumps

plot.ranking.bumps_norm <- ggplot(edb.reforms.rankings.bumps.summarized,
                             aes(x=avg.bump_norm, y=reform.type.clean, colour=has.bureau)) +
  geom_vline(xintercept=0, colour="darkred", size=0.5) +
  geom_pointrangeh(aes(xmin=lower_norm, xmax=upper_norm), size=0.5,
                   position=position_dodgev(0.5)) +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Average change in EDB rankings one year after reform (normalized rankings)",
       y="Type of reform",
       title="Changes in normalized rankings per reform",
       subtitle="After 1 year") +
  theme_edb()
plot.ranking.bumps_norm


#' There's a clear negative correlation between reform and movement in
#' rankings, too. This is contrary to what was in the APSA version of the
#' paper, but that's because the figure there accidentally used lagged values
#' of changes in rankings instead of leaded values. This is the correct figure:
#' 
edb.reforms.rankings <- edb.reforms %>%
  group_by(ccode, year) %>%
  summarise(num.reforms = sum(reform.num.no.na)) %>%
  left_join(select(edb.clean, ccode, year, p_edb_rank,
                   p_edb_rank_normalized, has.bureau), by=c("ccode", "year")) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lead(rank0, 1),
         rank2 = lead(rank0, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0,
         rank0_norm = p_edb_rank_normalized,
         rank1_norm = lead(rank0_norm, 1),
         rank2_norm = lead(rank0_norm, 2),
         change1_norm = rank1_norm - rank0_norm,
         change2_norm = rank2_norm - rank0_norm) %>%
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
                                    levels=rev(unique(reform.type.clean)), 
                                    ordered=TRUE))

plot.ranking.cor <- ggplot(reform.rankings.type.cor, 
                           aes(x=correlation, y=reform.type.clean, colour=has.bureau)) +
  geom_point() +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Correlation between reform and movement in rankings", y="Type of reform",
       title="Correlation between reform and movement in rankings") +
  theme_edb()
plot.ranking.cor

#' ### Scatterplots of changes in rankings
#' 
#' This is also visible in scatterplots for each type of reform. All have a
#' negative relationship, showing that countries will move down more in the
#' rankings as they undertake more reforms
#' 
plot.reform.rankings <- reform.rankings.type %>%
  select(reform.type.clean, reform.num.no.na, change1) %>%
  filter(complete.cases(.)) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=rev(levels(reform.rankings.type.cor$reform.type.clean)), 
                                    ordered=TRUE))

#+ fig.width=8, fig.height=4
ggplot(plot.reform.rankings, aes(x=reform.num.no.na, y=change1)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.05, fill="green") +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=0, ymax=Inf, alpha=0.05, fill="red") +
  geom_point(size=0.5, alpha=0.5) + 
  geom_smooth(method="lm") +
  labs(x="Number of reforms", y="Change in rankings (negative = good)") +
  theme_edb() +
  facet_wrap(~ reform.type.clean)


#' ## Rankings and reform committees
#' 
#' Do reform committees start out with worse rankings?
#' 
#' ### All years aggregated
reform.rankings.summary <- edb.reforms.rankings %>%
  filter(!(year %in% c(2005, 2006, 2015))) %>%
  group_by(has.bureau) %>%
  do(change = summary.corr(.$change1, .$num.reforms),
     reforms = summary.corr(.$num.reforms),
     ranking = summary.corr(.$p_edb_rank),
     ranking_norm = summary.corr(.$p_edb_rank_normalized))

plot.avg.reforms <- ggplot(unnest(reform.rankings.summary, reforms), 
                           aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Reforms",
       title="Reform committees and EDB rankings",
       subtitle="Average number of EDB reforms (all years)") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.rank <- ggplot(unnest(reform.rankings.summary, ranking), 
                        aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Ranking",
       subtitle="Average EDB ranking") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.rank_norm <- ggplot(unnest(reform.rankings.summary, ranking_norm), 
                             aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Ranking (normalized)",
       subtitle="Average EDB ranking (normalized)") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.rank.change <- ggplot(unnest(reform.rankings.summary, change), 
                               aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_vline(xintercept=0, colour="darkred", size=0.5) +
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Change in absolute ranking",
       subtitle="Average change in EDB rankings in the following year") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.reforms.rank <- arrangeGrob(plot.avg.reforms, blank, 
                                     plot.avg.rank, blank,
                                     plot.avg.rank_norm, blank,
                                     plot.avg.rank.change, 
                                     heights=c(0.23, 0.04, 0.2166, 0.04, 0.2166, 0.04, 0.2166), 
                                     ncol=1)
#+ fig.width=5, fig.height=6
grid::grid.draw(plot.avg.reforms.rank)

#' ### Individual years
reform.rankings.summary.indiv <- edb.reforms.rankings %>%
  filter(!(year %in% c(2005, 2006, 2013, 2014, 2015))) %>%
  group_by(has.bureau, year) %>%
  do(change = summary.corr(.$change1, .$num.reforms),
     reforms = summary.corr(.$num.reforms),
     ranking = summary.corr(.$p_edb_rank),
     ranking_norm = summary.corr(.$p_edb_rank_normalized))

#' #### Average number of reforms
plot.avg.reforms.indiv <- ggplot(unnest(reform.rankings.summary.indiv, reforms), 
                                 aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Reforms",
       subtitle="Average number of EDB reforms (all years)") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0)) +
  facet_wrap(~ year)
plot.avg.reforms.indiv

#' #### Average ranking
plot.avg.rank.indiv <- ggplot(unnest(reform.rankings.summary.indiv, ranking), 
                              aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Ranking",
       subtitle="Average EDB ranking") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0)) +
  facet_wrap(~ year)
plot.avg.rank.indiv

#' #### Average ranking (normalized)
plot.avg.rank.indiv <- ggplot(unnest(reform.rankings.summary.indiv, ranking_norm), 
                              aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Ranking",
       subtitle="Average EDB ranking (normalized)") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0)) +
  facet_wrap(~ year)
plot.avg.rank.indiv

#' ### Average change in ranking
plot.avg.rank.change.indiv <- ggplot(unnest(reform.rankings.summary.indiv, change), 
                                     aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_vline(xintercept=0, colour="darkred", size=0.5) +
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Change in absolute ranking",
       subtitle="Average change in EDB rankings in the following year") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0)) +
  facet_wrap(~ year)
plot.avg.rank.change.indiv
