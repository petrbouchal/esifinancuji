source("_targets_packages.R")
cnf <- config::get()

ds <- open_dataset(cnf$sp_central_arrowdir_new)

ds0 <- collect(ds)

sc_org_oss_ico <- read_parquet("data-processed/codelists/ucjed.parquet",
                               col_select = c("ico", "druhuj_id")) %>%
  filter(druhuj_id == "1")

sc_pol <- read_parquet("data-processed/codelists/polozka.parquet")

ds0 %>%
  filter(ico %in% sc_org_oss_ico$ico) %>%
  count(vykaz_year, druh, wt = sum((budget_spending), na.rm = T)/1e9) %>%
  spread(druh, n)

ds0 %>%
  filter(ico %in% sc_org_oss_ico$ico) %>%
  count(vykaz_year, trida, wt = sum((budget_spending), na.rm = T)/1e9) %>%
  spread(vykaz_year, n)

mr17 <- sp_load_table("sp_data/misris/2017/12/MIS-RIS_2017012.csv")

mr17 %>%
  count(vykaz_month)

mr17 %>%
  sp_add_codelist(sc_pol) %>%
  filter(!kon_pol | kon_rep | kon_okr | kon_kraj) %>%
  group_by(druh) %>%
  summarise(res = sum(budget_spending, na.rm = T)/1e3)

mr17 %>%
  sp_add_codelist(sc_pol) %>%
  group_by(across(c(starts_with("kon"), druh))) %>%
  summarise(res = sum(budget_spending)/1e6) %>%
  spread(druh, res)
