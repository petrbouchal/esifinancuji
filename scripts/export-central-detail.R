source("_targets_packages.R")

targets::tar_load(cb_esif_marked)

cb_esif_marked |> sp_add_codelist("paragraf_long", by = "paragraf_long") |>
  count(paragraf_long, paragraf.x, paragraf_long, paragraf_nazev, paragraf_long_nazev) |>
  group_by(paragraf.x) |>
  mutate(pocet_long = n()) |>
  filter(pocet_long > 1) |>
  ungroup() |>
  select(starts_with("paragraf"))

dt <- cb_esif_marked |>
  select(-paragraf_nazev, -skupina, -oddil, -pododdil, -poznamka) |>
  sp_add_codelist("paragraf_long") |>
  select(vykaz_year, budget_spending, paragraf, paragraf_long, skupina, oddil,
         pododdil,
         esif,
         polozka, polozka_nazev, druh, seskupeni, podseskupeni, trida, kapitola,
         zdroj, zdroj_nazev)

write_parquet(dt, "data-export/sr_detail_nokap.parquet")

targets::tar_load(sp_central_new_arrdir)

cnt <- open_dataset(sp_central_new_arrdir)

cb_all <- cnt |>
  rename(kapitola_id = kapitola) |>
  collect() |>
  sp_add_codelist("polozka") |>
  filter(!kon_pol | !kon_rep | kon_okr | kon_kraj) |>
  sp_add_codelist("paragraf_long") |>
  sp_add_codelist("kapitola") |>
  select(-matches("(start|end)_date$")) |>
  rename(kapitola_id_nazev = id_nazev)

write_parquet(cb_all, "data-export/sr_detail_ico.parquet")
