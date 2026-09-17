# Kjor fra prosjektmappen. Syntetiske data, uten nettverk.
source("app.R", encoding = "UTF-8")
variable <- function(code, values) list(code = code, values = as.list(values), valueTexts = as.list(values))
ssb_metadata <- function() list(variables = list(
  variable("Lufthavn", c("ENGM", "ENBR")), variable("TrafikkType", "000"),
  variable("TrafikkFly", "IU"), variable("PassasjerType", "AAT"),
  variable("Tid", c("2023M01", "2024M01"))))
ssb_fetch <- function(...) data.frame(Flyplass = rep(c("Oslo", "Bergen"), each = 2),
  Dato = rep(as.Date(c("2023-01-01", "2024-01-01")), 2), Passasjerer = c(100, 120, 80, 100))
shiny::testServer(server, {
  session$setInputs(metadata_refresh = 0, airports = c("ENGM", "ENBR"),
    traffic = "000", route = "IU", passengers = "AAT", mode = "count")
  session$setInputs(fetch = 1)
  session$setInputs(start = "2023-01-01", end = "2024-01-01")
  stopifnot(nrow(displayed()) == 4, !is.null(output$plot))
  stopifnot(nrow(kpi_data()) == 8,
    identical(kpi_data()$Verdi[kpi_data()$Visning == "count"], c(120, 100)), !is.null(output$kpis))
  session$setInputs(end = "2023-01-01")
  stopifnot(identical(kpi_data()$Verdi[kpi_data()$Visning == "count"], c(100, 80)))
  session$setInputs(end = "2023-02-01")
  stopifnot(nrow(kpi_data()) == 8, all(is.na(kpi_data()$Verdi)),
    grepl("Ikke tilgjengelig", output$kpis$html))
  session$setInputs(end = "2024-01-01")
  session$setInputs(mode = "yoy", start = "2024-01-01")
  stopifnot(isTRUE(all.equal(displayed()$Verdi, c(20, 25))))
  stopifnot(isTRUE(all.equal(kpi_data()$Verdi[kpi_data()$Visning == "yoy"], c(20, 25))),
    grepl("20,0 %", output$kpis$html))
  before <- kpi_data()
  session$setInputs(mode = "change12")
  stopifnot(identical(kpi_data(), before))
  session$setInputs(airports = "ENGM")
  stopifnot(grepl("Valgene er endret", output$status))
})
cat("Shiny-server OK\n")
