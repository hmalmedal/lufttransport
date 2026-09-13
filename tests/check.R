source("R/ssb.R", encoding = "UTF-8")
d <- data.frame(Flyplass = c("A", "A", "A", "B", "B"),
  Dato = as.Date(c("2023-01-01", "2024-01-01", "2024-03-01", "2023-01-01", "2024-01-01")),
  Passasjerer = c(100, 125, NA, 0, 50))
yoy <- traffic_series(d, as.Date("2024-01-01"), as.Date("2024-03-01"), "yoy")
stopifnot(identical(yoy$Verdi, c(25, NA_real_, NA_real_)))
idx <- traffic_series(d, as.Date("2023-01-01"), as.Date("2024-03-01"), "index")
stopifnot(identical(idx$Verdi, c(100, 125, NA_real_, NA_real_, NA_real_)))
missing_base <- traffic_series(d, as.Date("2023-02-01"), as.Date("2024-03-01"), "index")
stopifnot(all(is.na(missing_base$Verdi)))
stopifnot(identical(month_date("2024M02"), as.Date("2024-02-01")))
if (identical(Sys.getenv("TEST_SSB_LIVE"), "true")) {
  m <- ssb_metadata()
  live <- ssb_fetch(m, c("ENGM", "ENBR"), "000", "IU", "AAT")
  stopifnot(nrow(live) == 2 * length(ssb_variable(m, "Tid")$values),
    length(unique(live$Flyplass)) == 2, any(live$Passasjerer > 0, na.rm = TRUE),
    !anyDuplicated(live[c("Flyplass", "Dato")]))
  cat("SSB-integrasjon OK:", nrow(live), "observasjoner\n")
}
cat("Beregningstester OK\n")
