library(targets)
library(tarchetypes)
library(future)

# Config ------------------------------------------------------------------

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "statnipokladna", "here", "readxl",
                            "janitor", "curl", "httr", "stringr", "config",
                            "dplyr", "purrrow", "future", "arrow", "tidyr",
                            "ragg", "magrittr", "czso", "lubridate", "writexl",
                            "readr", "purrr", "pointblank", "tarchetypes",
                            "details", "forcats", "ggplot2", "gt"),
               # debug = "compiled_macro_sum_quarterly",
               imports = c("purrrow", "statnipokladna"),
)

options(crayon.enabled = TRUE,
        scipen = 100,
        statnipokladna.dest_dir = "sp_data",
        czso.dest_dir = "~/czso_data",
        yaml.eval.expr = TRUE)

future::plan(multicore)

source("R/utils.R")
source("R/functions.R")

cnf <- config::get(config = "default")
names(cnf) <- paste0("c_", names(cnf))
list2env(cnf, envir = .GlobalEnv)

# tar_renv()


# Config as target --------------------------------------------------------

t_config <- list(
  tar_file(config_yml, "config.yml")
)

# ESIF data ---------------------------------------------------------------

## Public project data -----------------------------------------------------

t_public_list <- list(
  tar_download(ef_pubxls, c_ef_pubxls_url,
               here::here("data-input/ef_publish.xls")),
  tar_target(ef_pub, read_pubxls(ef_pubxls))
)


## Custom MS sestavy -------------------------------------------------------


t_sestavy <- list(
  # seznam ŽOPek
  tar_target(efs_zop, load_efs_zop(c_sest_dir, c_sest_xlsx_zop)),
  # sečíst ŽOP za každý projekt po letech
  tar_target(efs_zop_annual, summarise_zop(efs_zop, quarterly = FALSE)),
  # načíst PRV
  tar_target(efs_prv, load_prv(c_prv_data_path, cis_kraj)),
  # posčítat platby PRV za projekt po letech
  tar_target(efs_prv_annual, summarise_prv(efs_prv, quarterly = FALSE))
)

t_op_compile <- list(
  tar_target(compiled_op_sum,
             summarise_by_op(efs_zop_annual, efs_prv_annual)))


# 2017 data ---------------------------------------------------------------

t_2017 <- list(
  tar_file(first_report_xlsx, c_first_report_xlsx),
  tar_target(dt17_wide, read_xlsx(first_report_xlsx, 2) %>%
               rename(paragraf = Paragraf,
                      spending_eu = EU,
                      spending_celkem = celkem,
                      spending_vlastni = vlastni,
                      trida = variable,
                      paragraf_nazev = par_nazev,
                      pododdil_kod = pododdil,
                      pododdil = pododdil_nazev,
                      oddil = oddil_nazev,
                      skupina = skupina_nazev,
                      druh_typ = typvar,
                      vykaz_year = year) %>%
               select(-par_nazev_kr, -par_nazev_str)),
  tar_target(dt17_long, dt17_wide %>%
               select(-spending_celkem) %>%
               pivot_longer(c(spending_eu, spending_vlastni),
                            names_to = "source", values_to = "spending") %>%
               mutate(source = recode(source,
                                      spending_eu = "EU",
                                      spending_vlastni = "vlastní")))
)

# Geo IDs -----------------------------------------------------------------

t_geometa <- list(
  tar_target(zuj_obec, get_zuj_obec()),
  # číselník krajů pro vložení kódu kraje v PRV
  tar_target(cis_kraj, czso::czso_get_codelist("cis100")),
  # populace obcí pro vážení projektů mezi kraji
  tar_target(pop_obce, get_stats_pop_obce(c_czso_pop_table_id))
)

# Statnipokladna ------------------------------------------

## Císelníky ----------------------------------------------

# keep downloaded data in project directory
options(statnipokladna.dest_dir = "sp_data")
codelist_names <- c("druhuj", "poddruhuj", "nuts",
                    "paragraf", "paragraf_long",
                    "ucelznak", "nastroj", "nastrojanal",
                    "polozka", "polvyk", "zdroj", "ucjed")

t_sp_codelists <- list(
  tar_target(codelists, codelist_names),

  # track changes at URL via {targets}/{tarchetypes}

  tar_url(sp_cl_urls, sp_get_codelist_url(codelists),
          pattern = map(codelists)),

  # track changes to file: if deleted/changed, redownload it

  tar_file(sp_cl_paths,
           sp_get_codelist_file(url = sp_cl_urls),
           pattern = map(sp_cl_urls)),

  # keep all codelists in one list tracked by {targets}:

  tar_target(sp_cl, sp_load_codelist_named(sp_cl_paths),
             pattern = map(sp_cl_paths)),
  tar_file(sp_clp, codelist_to_parquet(sp_cl, codelists, "codelists"),
           pattern = map(sp_cl, codelists))

)

## Central state budget data -----------------------------------------------

# read_tarrow(target_name)

t_sp_data_central_new <- list(
  tar_target(d_years, c_sp_years_central_new),
  tar_target(d_years_ptrn, d_years, pattern = map(d_years)),
  tar_target(d_months, c_sp_months_central_new),
  tar_target(d_id, "misris"),
  tar_url(d_url, sp_get_dataset_url(d_id, d_years_ptrn, d_months),
          pattern = map(d_years_ptrn)),
  tar_file(d_file, {is.character(d_url)
    sp_get_dataset(d_id, d_years, d_months)},
    pattern = map(d_years)), # to make sure target runs when data at URL changes
  tar_target(table_file, sp_get_table_file("budget-central", d_file),
             format = "file", pattern = map(d_file)),
  tar_target(sp_central_new_arrdir,
             budget_arrow_months(table_file, c_sp_central_arrowdir_new,
                                 c_sp_months_central_new, load_budget_yearsum_central_new,
                                 codelists = sp_cl),
             format = "file"),
  tar_target(sp_central_new_ops, budget_new_ops(sp_central_new_arrdir, nastroj_op, sp_cl))
)

## Pre-2015 central budget data --------------------------------------------

t_sp_data_central_old <- list(
  tar_target(d_years_o, c_sp_years_central_old),
  tar_target(d_years_o_ptrn, d_years_o, pattern = map(d_years_o)),
  tar_target(d_months_o, c_sp_months_central_old),
  tar_target(d_id_o, "finu"),
  tar_url(d_url_o, sp_get_dataset_url(d_id_o, d_years_o_ptrn, d_months_o),
          pattern = map(d_years_o_ptrn)),
  tar_file(d_file_o, {is.character(d_url_o)
    sp_get_dataset(d_id_o, d_years_o, d_months_o)},
    pattern = map(d_years_o)), # to make sure target runs when data at URL changes
  tar_target(table_file_o, sp_get_table_file("budget-central-old", d_file_o),
             format = "file", pattern = map(d_file_o)),
  tar_target(sp_central_old_arrdir,
             budget_arrow_months(table_file_o, c_sp_central_arrowdir_old,
                                 c_sp_months_central_old, load_budget_yearsum_central_old,
                                 codelists = sp_cl),
             format = "file")
)

## State fund budgets ------------------------------------------------------

t_sp_statefunds <- list(
  tar_target(d_id_sf, "finsf"),
  tar_url(d_url_sf, sp_get_dataset_url(d_id_sf, d_years_ptrn, d_months),
          pattern = map(d_years_ptrn)),
  tar_file(d_file_sf, {is.character(d_url_sf)
    sp_get_dataset(d_id_sf, d_years, d_months)},
    pattern = map(d_years)), # to make sure target runs when data at URL changes
  tar_target(table_file_sf, sp_get_table_file("budget-statefunds", d_file_sf),
             format = "file", pattern = map(d_file_sf)),
  tar_target(sp_sf_arrdir,
             budget_arrow_months(table_file_sf, c_sp_sf_arrowdir,
                                 c_sp_months_sf, load_budget_yearsum_sf,
                                 codelists = sp_cl),
             format = "file")
)

## Local budget data -----------------------------------------------

# read_tarrow(target_name)

t_sp_data_local <- list(
  tar_target(d_years_l, c_sp_years_local),
  tar_target(d_years_l_ptrn, d_years_l, pattern = map(d_years_l)),
  tar_target(d_months_l, c_sp_months_local),
  tar_target(d_id_l, "finm"),
  tar_url(d_url_l, sp_get_dataset_url(d_id_l, d_years_l_ptrn, d_months_l),
          pattern = map(d_years_l_ptrn)),
  tar_file(d_file_l, {is.character(d_url_l)
    sp_get_dataset(d_id_l, d_years_l, d_months_l)},
    pattern = map(d_years_l)), # to make sure target runs when data at URL changes
  tar_target(table_file_l, sp_get_table_file("budget-local", d_file_l),
             format = "file", pattern = map(d_file_l)),
  tar_target(sp_local_arrdir,
             budget_arrow_months(table_file_l, c_sp_local_arrowdir,
                                 c_sp_months_local, load_budget_yearsum_local,
                                 codelists = sp_cl),
             format = "file")
)


## Local budgets - grants --------------------------------------------------

t_sp_data_local_grants <- list(
  tar_target(table_file_lg, sp_get_table_file("budget-local-purpose-grants",
                                              d_file_l),
             format = "file", pattern = map(d_file_l)),
  tar_target(sp_local_grants_arrdir,
             budget_arrow_months(table_file_lg, c_sp_local_grants_arrowdir,
                                 c_sp_months_local, load_budget_local_grants,
                                 codelists = sp_cl),
             format = "file")
)


# HTML output -------------------------------------------------------------

source("R/html_output.R")


# Compile targets lists ---------------------------------------------------

list(t_public_list, t_sp_codelists, t_sp_data_central_new,
     t_sestavy, t_op_compile, t_2017,
     t_sp_statefunds, t_vydaje_core,
     t_sp_data_central_old, t_html, t_sp_data_local,
     t_sp_data_local_grants, t_geometa, t_config)
