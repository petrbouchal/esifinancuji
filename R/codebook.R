make_export_codebook <- function(cb_semisum_long) {
  create_informant(tbl = cb_semisum_long,
                   label = "Codebook hlavního výstupu") %>%
    info_tabular(Info = str_c("Tabulka se součty výdajů podle času, zdroje (ESIF nebo ne) a druhového i odvětvového třídění",
                          "Jde o konsolidované výdaje, tj. součet odpovídá celkovým výdajům státního rozpočtu.",
                          "Detaily viz v draftu studie.", sep = "<br />"),
                 `Varianty` = "Varianta pojmenovaná 'detail' obsahuje kompletní rozpad na druhové třídění; standardní verze je třídění na běžné a kapitálové výdaje.",
                 `Celková struktura` = "dlouhý formát: čas a kraj jsou v řádcích, metadata a jednotlivé zdroje financí jsou ve sloupcích") %>%

    info_snippet(
      snippet_name = "rok_od",
      fn = snip_lowest(column = "vykaz_year")
    ) %>%
    info_snippet(
      snippet_name = "rok_do",
      fn = snip_highest(column = "vykaz_year")
    ) %>%
    info_columns("vykaz_date",
                 Popis = "poslední den období výkazu",
                 Rozpětí = "viz `vykaz_year`") %>%
    info_columns("vykaz_year",
                 Popis = "rok výkazu",
                 Rozpětí = "od {rok_od} do {rok_do}")%>%
    info_columns("paragraf",
                 Popis = "čtyřmístný kód paragrafu",
                 Třídění = "odvětvové") %>%
    info_columns("paragraf_long",
                 Popis = "čtyřmístný kód paragrafu",
                 Třídění = "odvětvové") %>%
    info_columns("paragraf_nazev",
                 Popis = "čtyřmístný kód paragrafu",
                 Třídění = "odvětvové") %>%
    info_columns("skupina",
                 Popis = "Skupina - název",
                 Třídění = "odvětvové") %>%
    info_columns("oddil",
                 Popis = "Oddíl - název",
                 Třídění = "odvětvové") %>%
    info_columns("pododdil",
                 Popis = "Pododdil - název",
                 Poznámka = "jen v detailním exportu",
                 Třídění = "odvětvové") %>%
    info_columns("trida",
                 Popis = "Třída - název",
                 Třídění = "druhové") %>%
    info_columns("seskupeni",
                 Popis = "Seskupeni - název",
                 Poznámka = "jen v detailním exportu",
                 Třídění = "druhové") %>%
    info_columns("podseskupeni",
                 Popis = "Podseskupeni - název",
                 Poznámka = "jen v detailním exportu",
                 Třídění = "druhové") %>%
    info_columns("polozka_id",
                 Popis = "Položka - kód",
                 Poznámka = "jen v detailním exportu",
                 Třídění = "druhové") %>%
    info_columns("polozka_nazev",
                 Popis = "Položka - název",
                 Poznámka = "jen v detailním exportu",
                 Třídění = "druhové") %>%
    info_columns("esif",
                 Popis = "ESIF ano/ne",
                 Zdroj = "odvozeno ze seznamu zdrojů",
                 Třídění = "zdrojové") %>%
    info_columns("budget_spending",
                 Popis = "Výdaje - skutečnost") %>%
    info_columns("kapitola",
                 Popis = "Rozpočtová kapitola - kód",
                 Třídění = "odpovědnostní") %>%
    incorporate()
}
