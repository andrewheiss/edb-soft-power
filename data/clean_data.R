library(tidyverse)
library(haven)

# Clean up data
countries.with.edb.bureau <- read_csv(file.path(PROJHOME, "data",
                                                "countries_with_edb_bureau.csv"))

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

edb.its.committee <- edb.its %>%
  filter(ccode %in% countries.with.edb.bureau$cowcode)

edb.its.2001.committee <- edb.its %>%
  filter(in.2001 == 1) %>%
  filter(ccode %in% countries.with.edb.bureau$cowcode)

edb.its.2001.nocommittee <- edb.its %>%
  filter(in.2001 == 1) %>%
  filter(!(ccode %in% countries.with.edb.bureau$cowcode))

edb.its.cap.constrained <- filter(edb.its, year >= 2003)

edb.its.2001.cap.constrained <- filter(edb.its.2001, year >= 2003)

edb.its.committee.cap.constrained <- filter(edb.its.committee, year >= 2003)

edb.its.2001.committee.cap.constrained <- filter(edb.its.2001.committee, year >= 2003)
edb.its.2001.nocommittee.cap.constrained <- filter(edb.its.2001.nocommittee, year >= 2003)
