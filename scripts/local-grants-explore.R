library(tidyverse)
library(arrow)

targets::tar_load(sp_local_grants_arrdir)

ds <- open_dataset(sp_local_grants_arrdir)

ds %>%
  select(ucelznak, ucelznak_nazev, budget_grants,
         vykaz_year) %>%
  collect() %>%
  filter(vykaz_year > 2015) %>%
  count(ucelznak, ucelznak_nazev, wt = budget_grants/1e9, sort = T) %>%
  View()

ds %>%
  select(ucelznak, ucelznak_nazev, budget_grants,
         vykaz_year) %>%
  collect() %>%
  filter(vykaz_year == 2015) %>%
  mutate(eu = str_detect(ucelznak_nazev, "EU")) %>%
  count(ucelznak_nazev, eu, wt = budget_grants/1e9, sort = T) %>%
  View()

ds %>%
  select(vykaz_year, budget_grants, ucelznak_nazev) %>%
  filter(ucelznak_nazev != "Přímé náklady na vzdělávání") %>%
  collect() %>%
  mutate(eu = str_detect(ucelznak_nazev, "EU")) %>%
  count(vykaz_year, eu, wt = budget_grants/1e9) %>%
  spread(eu, n)

ds %>%
  head() %>%
  collect()
