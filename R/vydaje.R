filter_hlousek <- function(data) {
  d <- data  %>%
    filter(druh == "Výdaje",
           !str_detect(paragraf, "^62"),
           !str_detect(paragraf, "^63"),
           !str_detect(paragraf, "^64"),
           !str_detect(paragraf, "^1"),
           !str_detect(seskupeni, "Národní"),
           !str_detect(seskupeni, "do zahraničí"),
           !str_detect(seskupeni, "půjčené"),
           !str_detect(podseskupeni, "transfery veřejným rozpočtům")
    )

  if("kapitola" %in% names(d)) {
    d <- d %>% filter(kapitola != "0396")
  }

  if("oddil" %in% names(d)) {
    d <- d %>% filter(oddil != "Dávky a podpory v sociálním zabezpečení")
  }

  return(d)
}

make_nastroje_zdroje <- function(codelists) {
  zdroj <- extract_cl(codelists, "zdroj") %>%
    rename(zdroj = zdroj_id)

  nastroje_zdroje <- zdroj %>%
    filter(nastroj_id %in% c(c_sp_nastroj_ids_1420_ops,
                             c_sp_nastroj_ids_1420_prv,
                             c_sp_nastroj_ids_0713_ops,
                             c_sp_nastroj_ids_0713_prv)) %>%
    mutate(obdobi = if_else(nastroj_id %in% c(c_sp_nastroj_ids_1420_ops,
                                              c_sp_nastroj_ids_1420_prv),
                            "2014-20",
                            "2007-13"),
           prv = nastroj_id %in% c(c_sp_nastroj_ids_0713_prv,
                                   c_sp_nastroj_ids_1420_prv)) %>%
    select(nastroj_id, obdobi, prv) %>%
    distinct()
}

make_cb_esif_marked <- function(dataset_path, codelists, nastroje_zdroje) {
  data <- open_dataset(dataset_path)

  polozka <- extract_cl(codelists, "polozka")
  paragraf <- extract_cl(codelists, "long")
  zdroj <- extract_cl(codelists, "zdroj") %>%
    rename(zdroj = zdroj_id)

  data %>%
    filter(druh == "Výdaje") %>%
    select(vykaz_year, budget_spending, zdroj, polozka, vykaz_date,
           paragraf_long = paragraf, kapitola) %>%
    collect() %>%
    mutate(vykaz_date = as.Date(vykaz_date)) %>%
    # bind_rows(cb2021 %>% mutate(vykaz_year = as.integer(vykaz_year),
    #                             vykaz_date = as.character(vykaz_date)) %>%
    #             filter(druh == "Výdaje") %>%
    #             select(vykaz_year, budget_spending, zdroj, polozka, vykaz_date,
    #                    paragraf_long = paragraf, kapitola)) %>%
    sp_add_codelist(zdroj) %>%
    left_join(nastroje_zdroje, by = "nastroj_id") %>%
    mutate(esif = !is.na(obdobi)) %>%
    sp_add_codelist(polozka) %>%
    select(-poznamka) %>%
    sp_add_codelist(paragraf) %>%
    filter(!kon_pol | !kon_rep | kon_okr | kon_kraj)
}


summarise_esif_marked <- function(data) {
  data %>%
    group_by(trida, paragraf, skupina, oddil, pododdil,
             esif, obdobi, paragraf_nazev,
             vykaz_date, vykaz_year) %>%
    summarise(across(starts_with("budget_"), sum, na.rm = T), .groups = "drop") %>%
    mutate(source = if_else(esif, "EU", "vlastní")) %>%
    rename(spending = budget_spending)
}

make_esif_sum_wide <- function(data) {
  data %>%
    select(-source, -obdobi) %>%
    pivot_wider(names_from = esif, values_from = spending, names_prefix = "esif",
                values_fn = sum) %>%
    rename(spending_eu = esifTRUE, spending_vlastni = esifFALSE) %>%
    replace_na(list(spending_eu = 0, spending_vlastni = 0)) %>%
    mutate(spending_celkem = spending_eu + spending_vlastni,
           share_eu = spending_eu/spending_celkem)
}
