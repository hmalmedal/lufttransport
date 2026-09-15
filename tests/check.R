source("R/ssb.R", encoding = "UTF-8")
# JSON-objektnøkler kan komme i annen rekkefølge enn kategoriindeksen.
metadata_fixture <- list(label = "Test", id = list("Lufthavn"), dimension = list(
  Lufthavn = list(label = "Flyplass", category = list(
    index = list(ENBR = 1L, ENGM = 0L),
    label = list(ENBR = "Bergen", ENGM = "Oslo")))))
stopifnot(identical(ssb_choices(ssb_normalize_metadata(metadata_fixture), "Lufthavn"),
  c(Oslo = "ENGM", Bergen = "ENBR")))
metadata_fixture$dimension$Lufthavn$category$index <- list("ENGM", "ENBR")
stopifnot(identical(ssb_choices(ssb_normalize_metadata(metadata_fixture), "Lufthavn"),
  c(Oslo = "ENGM", Bergen = "ENBR")))
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
rolling <- data.frame(Flyplass = rep(c("A", "B"), each = 14),
  Dato = rep(seq(as.Date("2023-01-01"), by = "month", length.out = 14), 2),
  Passasjerer = c(0:13, rep(100, 14)))
average <- function(x, start = "2023-01-01") {
  traffic_series(x, as.Date(start), as.Date("2024-02-01"), "ma12")
}
ma <- average(rolling)
stopifnot(all(is.na(ma$Verdi[1:11])),
  identical(ma$Verdi[12:14], c(5.5, 6.5, 7.5)),
  identical(ma$Verdi[26:28], rep(100, 3)),
  identical(average(rolling, "2024-01-01")$Verdi, c(6.5, 7.5, 100, 100)))
# Hull og eksplisitt NA skal begge hindre beregning, uten å blande flyplasser.
stopifnot(is.na(average(rolling[-6, ])$Verdi[11]))
rolling$Passasjerer[6] <- NA_real_
stopifnot(all(is.na(average(rolling)$Verdi[12:14])))
if (identical(Sys.getenv("TEST_SSB_LIVE"), "true")) {
  m <- ssb_metadata()
  live <- ssb_fetch(m, c("ENGM", "ENBR"), "000", "IU", "AAT")
  stopifnot(nrow(live) == 2 * length(ssb_variable(m, "Tid")$values),
    length(unique(live$Flyplass)) == 2, any(live$Passasjerer > 0, na.rm = TRUE),
    !anyDuplicated(live[c("Flyplass", "Dato")]))
  cat("SSB-integrasjon OK:", nrow(live), "observasjoner\n")
}
cat("Beregningstester OK\n")
