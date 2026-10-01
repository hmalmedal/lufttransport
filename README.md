# Flytrafikk i Norge

Shiny-app for sammenligning av månedlige passasjertall fra [SSB tabell 08507](https://www.ssb.no/statbank/table/08507).

## Kjør appen

Åpne `lufttransport.Rproj` i RStudio. Installer pakkene én gang:

```r
install.packages(c("shiny", "PxWebApiData", "httr", "ggplot2", "scales",
                   "dplyr", "tidyr", "purrr", "tibble", "readr"))
```

Start fra prosjektmappen:

```r
shiny::runApp()
```

Hvis R på Windows gir advarsler om `C.UTF-8` eller feil ved lesing av norske tegn, kjør `Sys.setlocale("LC_CTYPE", ".UTF-8")` før du starter appen.

Velg flyplasser og trafikkavgrensning, og trykk **Hent passasjertall**. Velg deretter periode og visning: antall passasjerer, endring fra samme måned året før, 12 måneders glidende sum eller rullerende 12-måneders endring. Datatabellen og CSV-eksporten inneholder både rå passasjertall og beregnet verdi. Eksporten inkluderer trafikkavgrensning og hentetidspunkt.

Appen trenger internettilgang til `https://data.ssb.no`. Metadata hentes ved oppstart og kan oppdateres med egen knapp. Passasjertall hentes ved knappetrykk og holdes i minnet per økt. Endring av periode eller visning utløser ikke nye API-kall. Hele tidsserien hentes for valgte flyplasser, slik at årsvekst også kan beregnes ved starten av visningsperioden.

Appen bruker [PxWebApiData](https://cran.r-project.org/package=PxWebApiData) (versjon 2.0.0 eller nyere) mot SSBs [PxWebApi v2](https://www.ssb.no/api/pxwebapiv2). `meta_frames("08507")` henter norske metadata som brukes til flyplassvalg og trafikkategorier.

`api_data_2()` gjenbruker metadataene og henter data med GET. Alle dimensjoner velges eksplisitt, med én kategori per trafikkdimensjon og `Tid = "*"` for hele tidsserien. Pakken returnerer en dataramme med stabile dimensjonskoder; appen kobler på flyplassnavn og konverterer måneder til datoer. `httr::with_config()` setter tidsavbrudd på 45 sekunder for metadata og 90 sekunder for data. Tomme svar (`NULL`) behandles som feil og vises i appens feilmeldinger.

Tallene er ikke sesongjustert og måler ikke unike reisende. Manglende verdier blir ikke erstattet med null. Årsvekst krever en positiv verdi i samme måned året før.

## Kontroller

**Rullerende 12-måneders endring (%)** viser `100 × (sum siste 12 måneder / sum foregående 12 måneder − 1)`, separat for hver flyplass. Inneværende måned inngår i siste periode. Beregningen krever tall for alle 24 måneder og positiv sammenligningssum; ellers vises NA. Historikk før valgt visningsperiode brukes også her.

Visningen **12 måneders glidende sum** summerer passasjertallene i inneværende måned og de elleve foregående månedene, separat for hver flyplass. Alle tolv kalendermåneder må ha tall. Historikk før valgt visningsperiode inngår i beregningen, og manglende grunnlag vises som NA.

```r
source("tests/check.R", encoding = "UTF-8")
source("tests/server.R", encoding = "UTF-8")
```

Dette tester årsvekst, glidende sum og rullerende endring med nuller og manglende måneder, uten nettverk. En valgfri integrasjonstest henter et lite utvalg fra SSB:

```r
Sys.setenv(TEST_SSB_LIVE = "true")
source("tests/check.R", encoding = "UTF-8")
```
