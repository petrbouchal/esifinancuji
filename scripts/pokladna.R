library(arrow)
library(statnipokladna)
library(tidyverse)
options(scipen = 99)

cnf <- config::get()
ds <- open_dataset(cnf$sp_central_arrowdir_new)

targets::tar_load(sp_cl)
targets::tar_load(compiled_op_sum)


sf15 <- sp_load_table("sp_data/finsf/2015/12/finsf201512/FINSF01_2015012.csv") %>%
  mutate(vykaz_year = as.integer(vykaz_year)) %>%
  sp_add_codelist(polozka)

orgs <- read_parquet("data-processed/codelists/ucjed.parquet", as_data_frame = F) %>%
  select(ico, nazev) %>%
  collect() %>%
  distinct()

sf_hlouset <- sf15  %>%
  filter(!kon_pol, !kon_rep,
         druh == "Výdaje",
         !str_detect(paragraf, "^62"),
         !str_detect(paragraf, "^63"),
         !str_detect(paragraf, "^64"),
         !str_detect(paragraf, "^1"),
         !str_detect(seskupeni, "Národní"),
         !str_detect(seskupeni, "do zahraničí"),
         !str_detect(seskupeni, "půjčené"),
         !str_detect(podseskupeni, "transfery veřejným rozpočtům"),
         kapitola != "0396"
  ) %>%
  left_join(orgs) %>%
  filter(nazev != "Státní zemědělský intervenční fond")

sp_cl$sp_cl_1f1e99ba_zdroj

zdroj <- sp_cl$sp_cl_1f1e99ba_zdroj %>%
  rename(zdroj = zdroj_id)
nastroje_zdroje <- zdroj %>%
  filter(nastroj_id %in% c(cnf$sp_nastroj_ids_1420_ops, cnf$sp_nastroj_ids_1420_prv))

ds2021 <- sp_get_table("budget-central", 2021, 5)

polozka <- sp_cl$sp_cl_52371964_polozka
paragraf <- sp_cl$sp_cl_17831b9f_long

ds %>%
  select(zdroj, budget_spending, period_vykaz, per_yr) %>%
  collect() %>%
  sp_add_codelist(zdroj) %>%
  filter(nastroj_id %in% "107")

ds_vydaje_cons_esifmark <- ds %>%
  select(per_yr, budget_spending, zdroj, polozka, period_vykaz,
         paragraf_long = paragraf, kapitola) %>%
  collect() %>%
  bind_rows(ds2021 %>% mutate(per_yr = as.integer(per_yr),
                              period_vykaz = as.character(period_vykaz)) %>%
              select(per_yr, budget_spending, zdroj, polozka, period_vykaz,
                     paragraf_long = paragraf, kapitola)) %>%
  sp_add_codelist(zdroj) %>%
  mutate(esif = nastroj_id %in% c(cnf$sp_nastroj_ids_1420_ops,
                                  cnf$sp_nastroj_ids_1420_prv)) %>%
  sp_add_codelist(polozka) %>%
  select(-poznamka) %>%
  sp_add_codelist(paragraf) %>%
  filter(!kon_pol | !kon_rep | kon_okr | kon_kraj)

ds_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje") %>%
  # filter(str_detect(zdroj_nazev, "^SR")) %>%
  # filter(esif) %>%
  # count(nastroj_id, zdroj_nazev, wt = budget_spending/1e9, sort = T) %>%
  # count(nastroj_id, wt = budget_spending/1e9) %>%
  count(esif, per_yr, wt = budget_spending/1e9) %>%
  left_join(nastroj_op) %>%
  add_op_labels()

sf15 <- sp_get_table("budget-")

ds_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje") %>%
  count(esif, per_yr, wt = budget_spending/1e9) %>%
  spread(esif, n) %>%
  mutate(podil = `TRUE`/(`FALSE` + `TRUE`))

compiled_op_sum %>%
  count(wt = fin_vyuct_narodni_verejne/1e9)

compiled_op_sum %>%
  count(dt_zop_rok, wt = fin_vyuct_eu/1e9)

ds_vydaje_cons_esifmark %>%
  filter(skupina == "Sociální věci a politika zaměstnanosti",
         per_yr == 2015, !kon_rep) %>%
  count(oddil, wt = budget_spending/1e9)

ds %>%
  select(paragraf) %>%
  collect()

ds_hlouset <- ds_vydaje_cons_esifmark %>%
  mutate(period_vykaz = as.Date(period_vykaz)) %>%
  filter(!kon_rep | !kon_kraj | !kon_okr | !kon_pol,
         druh == "Výdaje",
         per_yr == 2015,
         !str_detect(paragraf_long, "^62"),
         !str_detect(paragraf_long, "^63"),
         !str_detect(paragraf_long, "^64"),
         !str_detect(paragraf_long, "^1"),
         !str_detect(seskupeni, "Národní"),
         !str_detect(seskupeni, "do zahraničí"),
         !str_detect(seskupeni, "půjčené"),
         !str_detect(podseskupeni, "transfery veřejným rozpočtům"),
         kapitola != "0396",
         oddil != "Dávky a podpory v sociálním zabezpečení")

ds_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje", str_detect(seskupeni, "ransf"), per_yr == 2015) %>%
  count(trida, seskupeni, podseskupeni, kon_rep, kon_pol, kon_okr, kon_kraj, wt = budget_spending/1e9) %>%
  arrange(kon_pol, kon_okr, kon_kraj, kon_rep, n) %>% View()

ds_vydaje_cons_esifmark %>%
  filter(!kon_rep,
         druh == "Výdaje",
         per_yr == 2015,
         str_detect(polozka, "^53")
  ) %>%
  count(wt = budget_spending/1e6)

ds_vydaje_cons_esifmark %>%
  mutate(kon = kon_pol | kon_rep | kon_okr | kon_kraj,
         cislo = str_sub(polozka, 1, 3)) %>%
  filter(druh == "Výdaje",
         per_yr == 2015) %>%
  count(trida, seskupeni, podseskupeni, cislo, kon, wt = budget_spending/1e9) %>%
  View()

ls <- open_dataset("data-processed/sp-local-arr/")

ls_hlouset <- ls %>%
  filter(druh == "Výdaje",
         per_yr == 2015,
         !str_detect(paragraf, "^62"),
         !str_detect(paragraf, "^63"),
         !str_detect(paragraf, "^64"),
         !str_detect(paragraf, "^1")) %>%
  select(-polozka_nazev, -druh, -trida, -seskupeni, -podseskupeni) %>%
  collect() %>%
  mutate(period_vykaz = as.Date(period_vykaz)) %>%
  sp_add_codelist(polozka) %>%
  select(-poznamka) %>%
  # sp_add_codelist(paragraf) %>%
  filter(!str_detect(seskupeni, "Národní"),
         !str_detect(seskupeni, "do zahraničí"),
         !str_detect(seskupeni, "půjčené"),
         !str_detect(podseskupeni, "transfery veřejným rozpočtům")) %>%
  filter(!kon_pol , !kon_rep , !kon_okr , !kon_kraj) %>%
  count(wt = budget_spending/1e9)

bind_rows(sf_hlouset, ds_hlouset) %>%
  count(wt = budget_spending/1e9)

ds_hlouset %>%
  count(per_yr, esif, wt = budget_spending/1e9)

library(tictoc)
tic()
ds %>%
  group_by(per_yr, ico) %>%
  summarise(x = sum(budget_spending, na.rm = TRUE),
            y = sum(budget_adopted, na.rm = TRUE),
            .engine = "duckdb") %>%
  collect()
toc()

tic()
ds %>%
  select(per_yr, budget_spending, budget_adopted, ico) %>%
  collect() %>%
  group_by(per_yr, ico) %>%
  summarise(x = sum(budget_spending, na.rm = TRUE),
            y = sum(budget_adopted, na.rm = TRUE))
toc()

tic()
ds %>%
  select(per_yr, budget_spending, budget_adopted, ico) %>%
  group_by(per_yr, ico) %>%
  summarise(x = sum(budget_spending, na.rm = TRUE),
            y = sum(budget_adopted, na.rm = TRUE))
toc()
