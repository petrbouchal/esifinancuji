esifinancuji
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Tento repozitář obsahuje kód na zpracování dat o výdajích financovaných
z ESI fondů v ČR primárně v letech 2014-2020. Cílem je:

-   připravit dataset pro vyhodnocení role ESI fondů ve veřejných
    výdajích
-   analýza role ESI fondů ve veřejných výdajích

**Vytvořeno pro Úřad vlády ČR jako součást projektu *Systémová podpora
společné evropské politiky podpory a pomoci, ESI a obdobných fondů na
Úřadu vlády ČR* (CZ.08.1.125/0.0/0.0/15_001/0000176)**

**[Draft studie o roli ESIF ve veřejných financích](s_pokladna.html)**

**Konečná verze studie prošla editací ve wordu a je dostupná na webu
[Úřadu
vlády](https://www.vlada.cz/cz/evropske-zalezitosti/analyzy-eu/analyzy-uvod-125732/)**:

-   [Veřejné výdaje a ESI fondy v letech 2015-2020: hlavní poznatky
    (prosinec
    2021](https://www.vlada.cz/cz/evropske-zalezitosti/analyzy-eu/analyzy-uvod-125732/)

Zdrojový kód textově téměř konečné verze je v souboru
\[s_analyza.docs/s_analyza.html), výstup v
[s_analyza.docx](s_analyza.docx).

Detailnější dokumentace k pipelinu:

-   [obsahová dokumentace](s_doc.html)
-   [dokumentace a validace výstupu](s_output.html)
-   [technická dokumentace](dev.html)

Relevantní předchozí publikace v této oblasti:

-   [MMR (2017) o roli ESI fondů ve veřejných
    výdajích](https://dotaceeu.cz/cs/evropske-fondy-v-cr/narodni-organ-pro-koordinaci/evaluace/knihovna-evaluaci/verejne-vydaje-a-fondy-eu-2007%e2%80%932015)

## Časový rozsah

Publikovaná studie pracuje s daty do roku 2020, kód v tomto repozitáři a
exportovaná data ale zahrnují i data za 2021, publikovaná v únoru 2022
(update 2022-02-23).

## Dokumentace souborů

-   `esifunguji.Rproj`: konfigurace RStudio projektu
-   `_targets.R`: hlavní soubor definující datový pipeline
-   `_site.yml`: konfigurace webu generovaného uvnitř pipeline do složky
    `docs`
-   `_interactive.R`: utilita pro rychlé načtení objektů pro
    interaktivní práci
-   `build.R`: utilita - spouští pipeline, v RStudio projectu navázáno
    na Build command
-   `*.Rmd`: zdroje webové dokumentace
-   `docs`: vygenerovaná webová dokumentace
-   `data-export`: exporty výstupních dat
-   `data-input`: vstupní data
-   `data-output`: výstupní data ve formátu pro R
-   `data-processed`: mezidata
-   `renv`: skladiště systému renv pro reprodukovatelnost prostředí
    (needitovat ručně)
-   `R`: kód funkcí, které dohromady vytváří pipeline
-   `scripts`: jiný kód mimo pipeline - odkladiště
-   `sp_data`: cache dat Státní pokladny

Detaily v [technické dokumentaci](dev.html).
