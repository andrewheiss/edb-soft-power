#' ---
#' title: "Average percent change from floor"
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

knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120),  # For output
                      warning=FALSE)

#+ message=FALSE
library(tidyverse)
library(haven)
library(pander)
library(gridExtra)

# Load data
countries.with.edb.bureau <- read_csv(file.path(PROJHOME, "data/countries_with_edb_bureau.csv"))
edb.its <- read_dta(file.path(PROJHOME, "data/MasterWBMarch16_15.dta")) %>%
  filter(year > 1999) %>%
  rename(p_edb_rank = p_ebd_rank) %>%
  select(ccode, year, 
         sb_proced, sb_days, sb_capital, sb_cost, con_proced, con_days, 
         gdp, gdpcap, pop, gdpgrowth, polity = polity2, ibrd,
         p_edb_rank) %>%
  mutate_each(funs(ln = log1p(.)), 
              starts_with("sb"), starts_with("con"), gdp, gdpcap, pop) %>%
  mutate(year.centered.2005 = year - 2005,
         year.centered.2006 = year - 2006,
         ranked.2005 = year.centered.2005 >= 0,
         ranked.2006 = year.centered.2006 >= 0) %>%
  group_by(ccode) %>%
  mutate(loan_ln = log1p(sum(ibrd, na.rm=TRUE))) %>%
  mutate_each(funs(lag = lag(.))) %>%
  ungroup()

edb.its.constrained.countries <- edb.its %>%
  mutate(in.report.in.2004 = year == 2004 & !is.na(sb_days),
         in.report.in.2001 = year == 2001 & !is.na(sb_days)) %>%
  group_by(ccode) %>%
  summarise(in.2004 = sum(in.report.in.2004),
            in.2001 = sum(in.report.in.2001))

edb.its <- edb.its %>%
  left_join(edb.its.constrained.countries, by="ccode") %>%
  filter(in.2004 == 1)

edb.its.2001 <- edb.its %>%
  filter(in.2001 == 1)

# Theme
theme_edb <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("label", list(family="Clear Sans Light"))
  update_geom_defaults("text", list(family="Clear Sans Light"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans Light"),
          plot.caption=element_text(family="Clear Sans Light",
                                    size=rel(0.8), colour="grey70"),
          panel.border = element_blank(), 
          axis.line=element_line(colour="grey50", size=0.2),
          #panel.grid=element_blank(), 
          axis.ticks=element_blank(),
          legend.position="bottom", 
          legend.title=element_text(size=rel(0.8)),
          axis.title=element_text(size=rel(0.8), family="Clear Sans", face="bold"),
          strip.text=element_text(size=rel(1), family="Clear Sans", face="bold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.key=element_blank(),
          legend.spacing=unit(0.2, "lines"))
  
  ret
}

#' Fake data from one fake country, pretending there's a 2-procedure floor. In
#' this case there's a constant decrease in procedures each year until 2004,
#' and the % change in distance to the floor increases (since they're getting
#' closer to the bottom; each change is more meaningful). In 2004 there's a
#' change in 4, which is a dramatic 400% decrease. They hit the floor in 2005,
#' hence the -∞ value.
#' 
example.data <- data_frame(Year = 2000:2005, Procedures = c(10:7, 3:2)) %>%
  mutate(`Distance to floor` = Procedures - 2,
         `Change in distance to floor` = `Distance to floor` - lag(`Distance to floor`),
         `% change in distance to floor` = `Change in distance to floor` / `Distance to floor`)

#+ results="asis"
pandoc.table(example.data, split.tables=Inf)


p1 <- ggplot(example.data, aes(x=Year, y=Procedures)) + 
  geom_line() + theme_edb()

p2 <- ggplot(example.data, aes(x=Year, y=`% change in distance to floor`)) +
  geom_line() + scale_y_continuous(labels=scales::percent) + theme_edb()

p.all <- rbind(ggplotGrob(p1), ggplotGrob(p2))
grid::grid.draw(p.all)

#' Here's how this applies to the EDB data.
#' 
#' Not all the variables have 0 as their floor. The lowest number of days
#' visible in the data is 0.5 and there's always at least 1 procedure.
#' 
edb.floors <- edb.its.2001 %>%
  select(sb_days, sb_proced, sb_cost, sb_capital) %>%
  summarise_all(funs(min(., na.rm=TRUE))) %>%
  gather(variable, floor)

#+ results="asis"
pandoc.table(edb.floors)

#' We can apply the same "% change in distance from floor" algorithm to the
#' real EDB data.
#' 
plot.edb.committee.2001 <- edb.its.2001 %>%
  select(year, ccode, sb_days, sb_proced, sb_cost, sb_capital) %>%
  gather(variable, value, -year, -ccode) %>%
  left_join(edb.floors, by="variable") %>%
  group_by(variable, ccode) %>%
  mutate(distance = value - floor,
         reduction = distance - lag(distance),
         perc.change = reduction / distance) %>%
  mutate(has.committee = factor(ccode %in% countries.with.edb.bureau$cowcode,
                                levels=c(FALSE, TRUE),
                                labels=c("No committee", "Committee"),
                                ordered=TRUE)) %>%
  mutate(perc.change = ifelse(perc.change == -Inf, NA, perc.change)) %>%
  group_by(year, variable, has.committee) %>%
  filter(!is.nan(perc.change)) %>%
  summarise(avg = mean(perc.change, na.rm=TRUE)) %>%
  filter(!is.nan(avg)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels=c("sb_proced", "sb_days",
                                              "sb_cost", "sb_capital"),
                           labels=c("Procedures", "Days", "Cost", "Capital"),
                           ordered=TRUE))

plot.interventions <- data_frame(year = 2005:2006,
                                 intervention = c("2005", "2006"))

plot.pct.change <- ggplot(plot.edb.committee.2001, 
                          aes(x=year, y=avg)) +
  geom_line() + 
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  labs(x=NULL, y="Percent") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  scale_y_continuous(labels=scales::percent) +
  # coord_cartesian(ylim=c(0, -2)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ has.committee + variable, nrow=2, scales="free_y") + 
  theme_edb()

#+ fig.width=8, fig.height=4
plot.pct.change
