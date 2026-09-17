# Kjor fra prosjektmappen. Syntetiske data, uten nettverk.
source("app.R", encoding = "UTF-8")
stopifnot(
  identical(kpi_format(2481320), "2 481 320"),
  identical(kpi_format(4.7, TRUE), "+4,7 %"),
  identical(kpi_format(-3.2, TRUE), "−3,2 %"),
  identical(kpi_format(0, TRUE), "0,0 %"),
  identical(kpi_format(-0.01, TRUE), "0,0 %"),
  identical(kpi_format(NA_real_, TRUE), "–"),
  identical(kpi_format(Inf, TRUE), "–"),
  identical(kpi_format(NaN), "–"),
  identical(kpi_direction(4.7), "positive"),
  identical(kpi_direction(-3.2), "negative"),
  identical(kpi_direction(0.01), "neutral"),
  identical(kpi_direction(NA_real_), "missing"),
  identical(kpi_month("2026-09-01"), "september 2026")
)
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
    grepl("kpi-missing", output$kpis$html), grepl(">–</td>", output$kpis$html),
    grepl("Oslo</th>", output$kpis$html), grepl("Bergen</th>", output$kpis$html),
    grepl("Nøkkeltall for februar 2023", output$kpis$html))
  session$setInputs(end = "2024-01-01")
  session$setInputs(mode = "yoy", start = "2024-01-01")
  stopifnot(isTRUE(all.equal(displayed()$Verdi, c(20, 25))))
  stopifnot(isTRUE(all.equal(kpi_data()$Verdi[kpi_data()$Visning == "yoy"], c(20, 25))),
    grepl("+20,0 %", output$kpis$html, fixed = TRUE))
  before <- kpi_data()
  session$setInputs(mode = "change12")
  stopifnot(identical(kpi_data(), before))
  for (mode in c("count", "sum12", "yoy")) {
    session$setInputs(mode = mode)
    stopifnot(identical(kpi_data(), before))
  }
  session$setInputs(airports = "ENGM")
  stopifnot(grepl("Valgene er endret", output$status))
})

# Flere flyplasser, lange navn og manglende sluttmåned for bare én flyplass.
shiny::testServer(server, {
  airports <- c("Oslo", "Bergen", "Trondheim", "Stavanger", "Tromsø",
    "Sandefjord lufthavn, Torp – et langt flyplassnavn", "Bodø", "Ålesund")
  months <- seq(as.Date("2024-10-01"), by = "month", length.out = 24)
  data <- data.frame(Flyplass = rep(airports, each = 24), Dato = rep(months, 8),
    Passasjerer = rep(c(rep(100, 12), rep(120, 12)), 8))
  data <- data[!(data$Flyplass == "Bergen" & data$Dato == max(months)), ]
  loaded(list(data = data, description = "Testutvalg"))
  session$setInputs(end = "2026-09-01")
  d <- kpi_data()
  stopifnot(length(unique(d$Flyplass)) == 8, nrow(d) == 32,
    all(is.na(d$Verdi[d$Flyplass == "Bergen"])),
    identical(d$Verdi[d$Flyplass == "Oslo" & d$Visning == "sum12"], 1440))
  html <- output$kpis$html
  stopifnot(all(vapply(airports, grepl, logical(1), x = html, fixed = TRUE)),
    length(gregexpr('scope="row"', html, fixed = TRUE)[[1]]) == 8,
    length(gregexpr('scope="col"', html, fixed = TRUE)[[1]]) == 5,
    grepl("Testutvalg", html), grepl("Nøkkeltall for september 2026", html),
    grepl("kpi-missing", html))
})
cat("Shiny-server OK\n")
