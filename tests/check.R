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
stopifnot(identical(month_date("2024M02"), as.Date("2024-02-01")))
rolling <- data.frame(Flyplass = rep(c("A", "B"), each = 14),
  Dato = rep(seq(as.Date("2023-01-01"), by = "month", length.out = 14), 2),
  Passasjerer = c(0:13, rep(100, 14)))
rolling_sum <- function(x, start = "2023-01-01") {
  traffic_series(x, as.Date(start), as.Date("2024-02-01"), "sum12")
}
ma <- rolling_sum(rolling)
stopifnot(all(is.na(ma$Verdi[1:11])),
  identical(ma$Verdi[12:14], c(66, 78, 90)),
  identical(ma$Verdi[26:28], rep(1200, 3)),
  identical(rolling_sum(rolling, "2024-01-01")$Verdi, c(78, 90, 1200, 1200)))
# Hull og eksplisitt NA skal begge hindre beregning, uten å blande flyplasser.
stopifnot(is.na(rolling_sum(rolling[-6, ])$Verdi[11]))
rolling$Passasjerer[6] <- NA_real_
stopifnot(all(is.na(rolling_sum(rolling)$Verdi[12:14])))
comparison <- data.frame(Flyplass = rep(c("A", "B"), each = 25),
  Dato = rep(seq(as.Date("2022-01-01"), by = "month", length.out = 25), 2),
  Passasjerer = c(rep(100, 12), rep(125, 13), rep(100, 12), rep(80, 13)))
change <- function(x, start = "2022-01-01") {
  traffic_series(x, as.Date(start), as.Date("2024-01-01"), "change12")
}
result <- change(comparison)
stopifnot(all(is.na(result$Verdi[1:23])),
  isTRUE(all.equal(result$Verdi[c(24, 49)], c(25, -20))),
  isTRUE(all.equal(change(comparison, "2023-12-01")$Verdi,
    result$Verdi[c(24, 25, 49, 50)])))
stopifnot(is.na(change(comparison[-6, ])$Verdi[23]))
comparison$Passasjerer[6] <- NA_real_
stopifnot(is.na(change(comparison)$Verdi[24]))
comparison$Passasjerer[1:12] <- 0
stopifnot(is.na(change(comparison)$Verdi[24]))
if (identical(Sys.getenv("TEST_SSB_LIVE"), "true")) {
  m <- ssb_metadata()
  live <- ssb_fetch(m, c("ENGM", "ENBR"), "000", "IU", "AAT")
  stopifnot(nrow(live) == 2 * length(ssb_variable(m, "Tid")$values),
    length(unique(live$Flyplass)) == 2, any(live$Passasjerer > 0, na.rm = TRUE),
    !anyDuplicated(live[c("Flyplass", "Dato")]))
  cat("SSB-integrasjon OK:", nrow(live), "observasjoner\n")
}
cat("Beregningstester OK\n")
