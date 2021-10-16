library(arrow)
library(statnipokladna)
library(tidyverse)
options(scipen = 99)

cnf <- config::get()

targets::tar_load(sp_cl)
targets::tar_load(compiled_op_sum)
targets::tar_load(sp_sf_arrdir)
targets::tar_load(sp_central_new_arrdir)
targets::tar_load(dt17_wide)
targets::tar_load(dt17_long)
targets::tar_load(sp_local_arrdir)
targets::tar_load(cb_esif_marked)
targets::tar_load(cb_sum_long)
targets::tar_load(cb_sum_hlousek_wide)
targets::tar_load(cb_sum_wide)
targets::tar_load(cb_sum_hlousek_long)

cf <- open_dataset(sp_sf_arrdir)
lb <- open_dataset(sp_local_arrdir)

polozka <- sp_cl$sp_cl_52371964_polozka
orgs <- read_parquet("data-processed/codelists/ucjed.parquet", as_data_frame = F) %>%
  select(ico, nazev) %>%
  collect() %>%
  distinct()

paragraf <- sp_cl$sp_cl_17831b9f_long

sf15 <- cf %>%
  filter(vykaz_year == 2015, druh == "Výdaje") %>%
  collect() %>%
  mutate(vykaz_date = as.Date(vykaz_date))

cf_hlousek <- sf15 %>%
  filter(vykaz_year == 2015) %>%
  filter_hlousek() %>%
  left_join(orgs) %>%
  filter(nazev != "Státní zemědělský intervenční fond")

# cb2021 <- sp_get_table("budget-central", 2021, 8) %>%
#   sp_add_codelist(polozka)

cb_esif_marked %>%
  # filter(str_detect(zdroj_nazev, "^SR")) %>%
  # filter(esif) %>%
  # count(nastroj_id, zdroj_nazev, wt = budget_spending/1e9, sort = T) %>%
  # count(nastroj_id, wt = budget_spending/1e9) %>%
  count(esif, nastroj_id, vykaz_year, wt = budget_spending/1e9) %>%
  left_join(nastroj_op) %>%
  add_op_labels()

cb_sum_wide %>%
  group_by(vykaz_year, trida) %>%
  summarise(x = sum(spending_eu)/sum(spending_celkem)) %>%
  spread(trida, x)

cb_sum_hlousek_wide %>%
  group_by(vykaz_year, trida) %>%
  summarise(x = sum(spending_eu)/sum(spending_celkem)) %>%
  spread(trida, x)

cb_sum_long %>%
  count(source, wt = spending/1e9)

cb_sum_wide %>%
  count(vykaz_year, wt = spending_celkem/1e6)

cb_esif_marked %>%
  count(vykaz_year, prv, obdobi, wt = budget_spending/1e9) %>%
  spread(obdobi, n)

cb_esif_marked %>%
  count(esif, prv, wt = budget_spending/1e9)

cb_esif_marked %>%
  filter(obdobi == "2007-13", vykaz_year == 2019) %>%
  count(nastroj_id, zdroj, zdroj_nazev, wt = budget_spending)

cb_esif_marked %>%
  filter(vykaz_year == 2015) %>%
  count(wt = budget_spending/1e9)

compiled_op_sum %>%
  count(wt = fin_vyuct_narodni_verejne/1e9)

compiled_op_sum %>%
  count(wt = fin_vyuct_czv/1e9)

compiled_op_sum %>%
  count(dt_zop_rok, wt = fin_vyuct_eu/1e9)

cb_vydaje_cons_esifmark %>%
  filter(skupina == "Sociální věci a politika zaměstnanosti",
         vykaz_year == 2015, !kon_rep) %>%
  count(oddil, wt = budget_spending/1e9)

cb_hlousek <- cb_vydaje_cons_esifmark %>%
  filter(vykaz_year == 2015) %>%
  filter_hlousek()

cb_hlousek %>%
  count(wt = budget_spending/1e9)

cb_hlousek %>%
  count(esif, obdobi, wt = budget_spending/1e9)

cb_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje", trida == "Kapitálové výdaje", vykaz_year == 2015) %>%
  count(esif, wt = budget_spending/1e9)

cb_hlousek %>%
  filter(druh == "Výdaje", trida == "Kapitálové výdaje") %>%
  count(esif, wt = budget_spending/1e9)

sf_hlousek %>%
  filter(druh == "Výdaje", trida == "Kapitálové výdaje") %>%
  count(wt = budget_spending/1e9)

dt17 %>%
  filter(typ == "úřo", druh == "KV", vykaz_year == 2015) %>%
  count(wt = spending_celkem/1e6)

dt17 %>%
  filter(typ == "úřo", druh == "KV", vykaz_year == 2015) %>%
  summarise(x = sum(spending_eu)/sum(spending_celkem))

cb_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje", str_detect(seskupeni, "ransf"), vykaz_year == 2015) %>%
  count(trida, seskupeni, podseskupeni, kon_rep, kon_pol, kon_okr, kon_kraj, wt = budget_spending/1e9) %>%
  arrange(kon_pol, kon_okr, kon_kraj, kon_rep, n)

cb_vydaje_cons_esifmark %>%
  filter(!kon_rep,
         vykaz_year == 2015,
         str_detect(polozka, "^53")
  ) %>%
  count(wt = budget_spending/1e6)

paragraf <- extract_cl(sp_cl, "paragraf")
lb_hlousek <- lb %>%
  filter(vykaz_year == 2015) %>%
  collect() %>%
  mutate(vykaz_date = as.Date(vykaz_date)) %>%
  sp_add_codelist(paragraf) %>%
  filter_hlousek()

lb_hlousek %>%
  count(wt = budget_spending/1e9)

bind_rows(cf_hlousek, cb_hlousek, lb_hlousek) %>%
  count(wt = budget_spending/1e9)

dt17_long %>% count(vykaz_year, wt = spending_celkem/1e6)

lg <- open_dataset(sp_local_grants_arrdir)

lg %>% collect() %>% View()

lg %>%
  # filter(vykaz_year == 2019) %>%
  count(polozka, ucelznak_nazev, wt = budget_grants/1e9, sort = T) %>%
  collect() %>% View()
  sp_add_codelist(polozka)
