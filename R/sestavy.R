load_efs_zop <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_zop0 <- read_excel(path, skip = 0, col_types = "guess", guess_max = 10000) %>%
    janitor::clean_names() %>%
    select(-nazev_projektu)

  efs_zop <- efs_zop0 %>%
    rename(
      prj_id = cislo_projektu,
      real_stav_kod = cislo_stavu,
      dt_zop_proplaceni = datum_proplaceni,
      real_stav_nazev = nazev_stavu,
      zop_cislo = cislo_zo_p,
      pomer_unie = prispevek_unie_7,
      pomer_narodni = narodni_zdroje,
      pomer_vlastni = vlastni_podil,
      fin_vyuct_czv = celkove_zdroje_pripadajici_na_zpusobile_vydaje,
      fin_vyuct_eu = prispevek_unie_11,
      fin_vyuct_soukr = soukrome_zdroje,
      fin_vyuct_sr = financni_prostredky_ze_statniho_rozpoctu,
      fin_vyuct_sf = financni_prostredky_ze_statnich_fondu,
      fin_vyuct_kraj = financni_prostredky_z_rozpoctu_kraju_kraje,
      fin_vyuct_obec = financni_prostredky_z_rozpoctu_obci_obce,
      fin_vyuct_jine_nar_ver = jine_narodni_verejne_financni_prostredky
    ) %>%
    mutate(
      across(starts_with("fin_"), as.double),
      across(starts_with("pomer_"), as.double),
      across(starts_with("dt_"), as.Date),
      fin_vyuct_narodni_verejne = fin_vyuct_czv - fin_vyuct_soukr - fin_vyuct_eu,
      fin_vyuct_narodni = fin_vyuct_czv - fin_vyuct_eu)

  return(efs_zop)
}

summarise_zop <- function(efs_zop, quarterly) {
  zop_prep <- efs_zop %>%
    mutate(dt_zop_rok = year(dt_zop_proplaceni),
           dt_zop_rok = if_else(is.na(dt_zop_rok),
                                as.double(str_extract(zop_cislo,
                                                      "(?<=/)20[12][0-9](?=/)")),
                                dt_zop_rok))

  if(quarterly) {
    zop_grp <- zop_prep %>%
      mutate(dt_zop_kvartal = month(dt_zop_proplaceni) %/% 4 + 1) %>%
      group_by(prj_id, dt_zop_rok, dt_zop_kvartal)
  } else {
    zop_grp <- zop_prep %>%
      group_by(prj_id, dt_zop_rok)
  }
  zop <- zop_grp %>%
    summarise(across(starts_with("fin_"), sum, na.rm = T), .groups = "drop")

  # fill in date for ZOP with no date
  # and spread the payment across quarters in that year

  if(quarterly) {
    zop_withq <- zop %>% filter(!is.na(dt_zop_kvartal)) %>%
      mutate(timing_inferred = FALSE)
    zop_noq   <- zop %>% filter( is.na(dt_zop_kvartal))

    # generate all quarters between start of programming period and now
    x <- seq.Date(as.Date("2014-01-01"), Sys.Date(), "quarter")
    qrtrs <- tibble(dt_zop_rok = year(x), dt_zop_kvartal = quarter(x))

    spread_over_quarters <- function(prj_rows, quarters) {

      qrtrs_local <- quarters %>% filter(dt_zop_rok %in% prj_rows$dt_zop_rok)

      # create rows for each quarter in each year
      prj_rows %>%
        select(-dt_zop_kvartal) %>%
        full_join(qrtrs_local, by = "dt_zop_rok") %>%
        group_by(dt_zop_rok) %>%
        # split the payment amount across the rows
        mutate(across(starts_with("fin_vyuct"),
                      ~.x/length(unique((dt_zop_kvartal)))))
    }

    zop_addedq <- zop_noq %>%
      ungroup() %>%
      group_split(prj_id) %>%
      # take each project, and split the payments of each year into rows
      map_dfr(~spread_over_quarters(.x, qrtrs)) %>%
      mutate(timing_inferred = TRUE)

    zop <- bind_rows(zop_addedq, zop_withq)%>%
      group_by(prj_id, dt_zop_rok, dt_zop_kvartal, timing_inferred) %>%
      # sum in case some projects had both missing-date and non-missing-date
      # payments in the same year
      summarise(across(starts_with("fin_vyuct"), sum, na.rm = T),
                .groups = "drop") %>%
      mutate(dt_zop_kvartal_datum = make_date(dt_zop_rok, dt_zop_kvartal * 3 - 2, 1))
  }
  return(zop)
}

load_prv <- function(path, cis_kraj) {
  readxl::read_excel(path) %>%
    janitor::clean_names() %>%
    mutate(opatreni_new = recode(opatreni,
                                 "G" = "16",
                                 "J" = "19",
                                 "K" = "20",
    ),
    operace_new = if_else(is.na(operace), "", operace)) %>%
    unite(opatreni_new, podopatreni, operace_new, col = "prv_operace_kod", sep = ".") %>%
    mutate(across(starts_with("dt_"), as.Date)) %>%
    mutate(dt_zop_rok = year(datum_platby),
           dt_zop_kvartal = month(datum_platby) %/% 4 + 1,
           prv_operace_kod = str_remove(prv_operace_kod, "\\.$")) %>%
    rename(prj_id = registracni_cislo,
           p_id = jednotny_identifikator_prijemce,
           prj_nazev = nazev_projektu,
           p_nazev = nazev_projektu,
           kraj_nazev = kraj_nuts3_text,
           p_pravniforma = pravni_forma_na_zadosti,
           dt_zadost = datum_prijeti_zadosti_o_dotaci,
           dt_platba = datum_platby,
           okres_nazev = okres_nuts4_text,
           fin_vyuct_eu = zdroje_ezfrv_czk,
           fin_vyuct_czv = zdroje_celkem_czk,
           fin_vyuct_narodni = zdroje_narodni_czk,
    ) %>%
    left_join(cis_kraj %>% select(kraj_nazev = TEXT, kraj_id = CZNUTS),
              by = "kraj_nazev")
}

summarise_prv <- function(efs_prv, quarterly) {
  zop_grp <- efs_prv %>%
    group_by(prj_id, dt_zop_rok, prv_operace_kod, kraj_id)
  if(quarterly) {
    zop_grp <- zop_grp %>%
      group_by(dt_zop_kvartal, .add = TRUE)
  }
  zop_grp %>%
    summarise(across(starts_with("fin_"), sum), .groups = "drop")
}

summarise_by_op <- function(efs_zop_quarterly, efs_prv_quarterly) {
  efs_zop_quarterly <- add_op_labels(efs_zop_quarterly) # fn defined in R/utils.R
  efs_prv_quarterly$op_nazev <- "Program rozvoje venkova"
  efs_prv_quarterly$op_nazev_zkr <- "Program rozvoje venkova"
  efs_prv_quarterly$op_zkr <- "PRV"
  efs_prv_quarterly$op_id <- "09"

  bind_rows(efs_zop_quarterly, efs_prv_quarterly) %>%
    group_by(across(starts_with("op_")), dt_zop_rok) %>%
    summarise(across(starts_with("fin_"), sum), .groups = "drop")
}
