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
  session$setInputs(mode = "index")
  stopifnot(isTRUE(all.equal(displayed()$Verdi, c(100, 120, 100, 125))))
  session$setInputs(mode = "yoy", start = "2024-01-01")
  stopifnot(isTRUE(all.equal(displayed()$Verdi, c(20, 25))))
  session$setInputs(airports = "ENGM")
  stopifnot(grepl("Valgene er endret", output$status))
})
cat("Shiny-server OK\n")
