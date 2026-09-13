# Flytrafikk i Norge

Shiny-app for sammenligning av månedlige passasjertall fra [SSB tabell 08507](https://www.ssb.no/statbank/table/08507).

## Kjør appen

Åpne `lufttransport.Rproj` i RStudio. Installer pakkene én gang:

```r
install.packages(c("shiny", "rjstat", "httr", "ggplot2", "scales"))
```

Start fra prosjektmappen:

```r
shiny::runApp()
```

Hvis R på Windows gir advarsler om `C.UTF-8` eller feil ved lesing av norske tegn, kjør `Sys.setlocale("LC_CTYPE", ".UTF-8")` før du starter appen.

Velg flyplasser og trafikkavgrensning, og trykk **Hent passasjertall**. Velg deretter periode og visning: antall, indeks eller endring fra samme måned året før. Datatabellen og CSV-eksporten inneholder både rå passasjertall og beregnet verdi. Eksporten inkluderer trafikkavgrensning og hentetidspunkt.

Appen trenger internettilgang til `https://data.ssb.no`. Metadata hentes ved oppstart og kan oppdateres med egen knapp. Passasjertall hentes ved knappetrykk og holdes i minnet per økt. Endring av periode eller visning utløser ikke nye API-kall. Hele tidsserien hentes for valgte flyplasser, slik at årsvekst også kan beregnes ved starten av visningsperioden.

`httr` sender en eksplisitt POST-spørring med én kategori per trafikkdimensjon. `rjstat::fromJSONstat()` konverterer JSON-stat2-svaret til en dataramme. Flyplassvalg og tilgjengelige måneder leses fra SSBs metadata.

Tallene er ikke sesongjustert og måler ikke unike reisende. Manglende verdier blir ikke erstattet med null. Indeksen krever en positiv verdi i felles startmåned; årsvekst krever en positiv verdi i samme måned året før.

## Kontroller

```r
source("tests/check.R", encoding = "UTF-8")
source("tests/server.R", encoding = "UTF-8")
```

Dette tester indeks og årsvekst med nuller og manglende måneder, uten nettverk. En valgfri integrasjonstest henter et lite utvalg fra SSB:

```r
Sys.setenv(TEST_SSB_LIVE = "true")
source("tests/check.R", encoding = "UTF-8")
```
