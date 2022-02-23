library(magrittr)
library(dplyr)
library(stringr)


# OP labels ---------------------------------------------------------------



op_labels <- tibble::tribble(
  ~op_id,                                                ~op_nazev,    ~op_zkr,
  "01", "Operační program Podnikání a inovace pro konkurenceschopnost", "OP PIK",
  "02",                  "Operační program Výzkum, vývoj a vzdělávání", "OP VVV",
  "03",                                "Operační program Zaměstnanost", "OP Z",
  "04",                                     "Operační program Doprava", "OP D",
  "05",                           "Operační program Životní prostředí", "OP ŽP",
  "06",                      "Integrovaný regionální operační program", "IROP",
  "07",                        "Operační program Praha - pól růstu ČR", "OP PPR",
  "08",                             "Operační program Technická pomoc", "OP TP",
  "11",                        "INTERREG V-A Česká republika - Polsko", "OP ČR-PL"
) %>%
  mutate(op_nazev_zkr = str_replace(op_nazev, "[Oo]perační program|INTERREG V-A", "OP") %>%
           str_replace("Česká republika", "ČR"))

add_op_labels <- function(data, abbrevs = op_labels,
                          drop_orig = TRUE, drop_duplicate_cols = T) {

  if(!"op_id" %in% names(data) & "prj_id" %in% names(data)) {
    data$op_id <- str_sub(data$prj_id, 4, 5)
  } else if ("op_zkr" %in% names(data)) {
    if(drop_orig) data$op_zkr <- NULL else data <- rename(data, op_zkr_orig = op_zkr)
  } else if ("op_nazev" %in% names(data)) {
    if(drop_orig) data$op_nazev <- NULL else data <- rename(data, op_nazev_org = op_nazev)
  }

  data2 <- data %>%
    left_join(abbrevs, by = "op_id", suffix = c("", "_lblx"))

  if(drop_duplicate_cols) data2 <- data2 %>% select(-ends_with("lblx"))

  return(data2)
}

nastroj_op <- tibble::tribble(~nastroj_id, ~op_id, ~obdobi,
                      "101",       "XX", "2014-20",
                      "102",       "01", "2014-20",
                      "103",       "02", "2014-20",
                      "104",       "05", "2014-20",
                      "105",       "04", "2014-20",
                      "106",       "05", "2014-20",
                      "107",       "06", "2014-20",
                      "108",       "07", "2014-20",
                      "109",       "08", "2014-20",
                      "110",       "11", "2014-20",
                      "130",       "YY", "2014-20",
                      "187",       "YY", "2014-20")

extract_cl <- function(cl_target_list, cl_string) {

  cl_branchname <- names(cl_target_list)[str_detect(names(cl_target_list),
                                                  paste0(cl_string, "$"))]
  cl <- cl_target_list[[cl_branchname]]
  return(cl)
}


# Datavis utils -----------------------------------------------------------

theme_studie <- function(...) {
  ptrr::theme_ptrr(family = "Helvetica", title_family = "Helvetica",
                   tonecol = "grey95", base_size = 9,
                   legend.key.width = unit(6, "pt"),
                   margin_side = 0,
                   legend.key.height = unit(10, "pt"),
                   ...)
}

yr_brks <- lubridate::make_date(2015:2021, 12, 31)
yr_lbls <- c("2015", "'16", "'17", "'18", "'19", "'20", "2021")
yr_lbls_m <- c("2015", "'16", "'17", "'18", "'19", "'20", "'21")

yr_brks_old <- lubridate::make_date(2007:2015, 12, 31)
yr_lbls_old <- c("2007", "'08", "'09", "'10", "'11",
                 "'12", "'13", "'14", "2015")

clrs <- c(# "b" = "#24a7af", # bezne - mmr
  # "e" = "#001489", # evropa - mmr
  # "n" = "#e21c18", # narodni - mmr
  # "m" = "#00af3f", # mistni - mmr
  "k" = "#ae254b", # kapital
  "b" = "#4c6d16", # bezne
  "e" = "#1c286e", # evropa - tone
  "n" = "#C83432", # narodni - tone
  "m" = "#d9b830", # mistni
  "u" = "#1c286e", # ustredni
  "c" = "grey20") # celkem

export_table <- function(data, path, fun, ...) {

  fun(data, path, ...)

  return(path)
}


pc <- function(x, accuracy = 1) {
  ptrr::label_percent_cz(accuracy = accuracy)(x)
}

nm <- function(x, accuracy = 1) {
  ptrr::label_number_cz(accuracy = accuracy)(x)
}

bn <- function(x, accuracy = 1) {
  ptrr::label_number_cz(accuracy = accuracy)(x/1e9)
}

mn <- function(x, accuracy = 1) {
  ptrr::label_number_cz(accuracy = accuracy)(x/1e6)
}

get_nm <- function(data, rok, eu = NULL) {
  ddd <- data |>
    filter(year(vykaz_date) == rok)

  if(!is.null(eu)) {

    fltr <- if(eu) "EU" else "vlastní"

    ddd <- ddd |>
      filter(source == fltr)
  }

  return(ddd |> pull(spending))

}

listize <- function(data, ...) {
  dd <- data |>
    ungroup() |>
    mutate(yr = paste0("y", year(vykaz_date))) |>
    # mutate(across(c(...), janitor::make_clean_names)) |>
    rename_with(~"sp", matches("spending|perc")) |>
    select(yr, sp, ...)

  if(!missing(...)) {
    dd <-  pivot_wider(dd, names_from = c(...), values_from = sp) |>
      janitor::clean_names() |>
      rename_with(.fn = str_remove, pattern = "_vydaje")
    dd <- split(dd, dd$yr)
    map(dd, ~select(.x, -yr))
  } else {
    dd <- pivot_wider(dd, names_from = yr, values_from = sp) |>
      janitor::clean_names()
  }
}
