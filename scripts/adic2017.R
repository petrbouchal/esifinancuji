library(tidyverse)
library(readxl)

adic17_out <- read_excel("data-input/adic2017/vydaje_dataset.xlsx", 2)

adic17_out %>%
  filter(typ == "úřo") %>%
  count(year, wt = EU/1e3)

adic17_out %>%
  filter(typ == "úřo") %>%
  count(year, wt = celkem/1e3)

adic17_out %>%
  filter(skupina_nazev == "Sociální věci a politika zaměstnanosti",
         year == 2015, typ == "úřo") %>%
  count(oddil_nazev, wt = celkem/1e6)

adic17_out %>%
  filter(year == 2015) %>%
  count(typ, wt = celkem/1e6)

adic17_out %>%
  filter(typ == "úřo") %>%
  filter(year == 2015) %>%
  count(wt = sum(EU)/sum(celkem))

adic17_out %>%
  filter(typ == "mřo", year == 2015) %>%
  count(wt = EU)
