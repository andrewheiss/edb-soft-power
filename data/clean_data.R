library(tidyverse)
library(haven)

normalize <- function(x) {
  if (length(na.omit(x)) > 0) {
    (x - 1) / (max(x, na.rm=TRUE) - 1) * 100
  } else {
    x
  }
}

# Clean up data
countries.with.edb.bureau <- read_csv(file.path(PROJHOME, "data",
                                                "countries_with_edb_bureau.csv"))

edb.clean <- read_dta(file.path(PROJHOME, "data/MasterWBMarch16_15.dta")) %>%
  filter(year > 1999) %>%
  rename(p_edb_rank = p_ebd_rank) %>%
  select(ccode, country_name, year, 
         sb_proced, sb_days, sb_capital, sb_cost, con_proced, con_days, 
         contains("_reform"),
         gdp, gdpcap, pop, gdpgrowth, polity = polity2, ibrd,
         p_edb_rank) %>%
  mutate(has.bureau = ccode %in% countries.with.edb.bureau$cowcode,
         has.bureau = factor(has.bureau, 
                             labels=c("No EDB reform committee    ", 
                                      "Special EDB reform committee"))) %>%
  mutate_each(funs(ln = log1p(.)), 
              starts_with("sb"), starts_with("con"), gdp, gdpcap, pop, 
              -contains("_reform")) %>%
  mutate(year.centered.2005 = year - 2005,
         year.centered.2006 = year - 2006,
         ranked.2005 = year.centered.2005 >= 0,
         ranked.2006 = year.centered.2006 >= 0) %>%
  group_by(ccode) %>%
  mutate(loan_ln = log1p(sum(ibrd, na.rm=TRUE))) %>%
  mutate_each(funs(lag = lag(.))) %>%
  group_by(year) %>%
  mutate(p_edb_rank_normalized = normalize(p_edb_rank)) %>%
  ungroup() 

reform.types <- tribble(
  ~var.name,          ~clean.name,
  "sb_reform",        "Starting a business",
  "cp_reform",        "Construction permits",
  "el_reform",        "Getting electricity",
  "rp_reform",        "Registering property",
  "cred_reform",      "Getting credit",
  "pmi_reform",       "Protecting minority investors",
  "tx_reform",        "Paying taxes",
  "trade_reform",     "Trading across borders",
  "con_reform",       "Enforcing contracts",
  "insolv_reform",    "Resolving insolvency"
)

edb.reforms <- edb.clean %>%
  select(ccode, year, contains("_reform"), has.bureau, 
         p_edb_rank, p_edb_rank_normalized) %>%
  gather(reform.type, reform.num, contains("_reform")) %>%
  mutate(reform.num.no.na = ifelse(is.na(reform.num), 0, reform.num),
         reform.positive = ifelse(reform.num.no.na > 0, reform.num.no.na, 0),
         reform.type.clean = factor(reform.type, levels=reform.types$var.name,
                                    labels=reform.types$clean.name, ordered=TRUE))

edb.its.constrained.countries <- edb.clean %>%
  mutate(in.report.in.2004 = year == 2004 & !is.na(sb_days),
         in.report.in.2001 = year == 2001 & !is.na(sb_days)) %>%
  group_by(ccode) %>%
  summarise(in.2004 = sum(in.report.in.2004),
            in.2001 = sum(in.report.in.2001))

edb.its <- edb.clean %>%
  left_join(edb.its.constrained.countries, by="ccode") %>%
  filter(in.2004 == 1)

edb.its.2001 <- edb.clean %>%
  left_join(edb.its.constrained.countries, by="ccode") %>%
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
