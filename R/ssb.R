ssb_url <- "https://data.ssb.no/api/v0/no/table/08507"

ssb_metadata <- function() {
  response <- httr::GET(ssb_url, httr::timeout(45))
  httr::stop_for_status(response, "hente metadata fra SSB")
  httr::content(response, as = "parsed", encoding = "UTF-8")
}

ssb_variable <- function(metadata, code) {
  metadata$variables[[match(code, vapply(metadata$variables, `[[`, "", "code"))]]
}

ssb_choices <- function(metadata, code) {
  variable <- ssb_variable(metadata, code)
  stats::setNames(unlist(variable$values), unlist(variable$valueTexts))
}

month_date <- function(x) as.Date(paste0(sub("M", "-", x), "-01"))

ssb_fetch <- function(metadata, airports, traffic, route, passengers) {
  selections <- list(
    Lufthavn = airports, TrafikkType = traffic, TrafikkFly = route,
    PassasjerType = passengers, ContentsCode = "Passasjerer",
    Tid = unlist(ssb_variable(metadata, "Tid")$values)
  )
  query <- lapply(names(selections), function(code) {
    list(code = code, selection = list(filter = "item", values = unname(as.list(selections[[code]]))))
  })
  response <- httr::POST(ssb_url, body = list(query = query,
    response = list(format = "json-stat2")), encode = "json", httr::timeout(90))
  httr::stop_for_status(response, "hente passasjertall fra SSB")
  # Behold dimensjonskodene slik at endrede etiketter ikke knekker appen.
  raw <- rjstat::fromJSONstat(httr::content(response, as = "text", encoding = "UTF-8"), naming = "id")
  stopifnot(all(c("Lufthavn", "Tid", "value") %in% names(raw)))
  choices <- ssb_choices(metadata, "Lufthavn")
  result <- data.frame(
    Flyplass = names(choices)[match(raw$Lufthavn, choices)],
    Dato = month_date(raw$Tid), Passasjerer = as.numeric(raw$value)
  )
  result[order(result$Flyplass, result$Dato), ]
}

traffic_series <- function(data, start, end, mode) {
  result <- data
  result$Verdi <- result$Passasjerer
  if (mode == "yoy") {
    # Match på kalendermåned, ikke radnummer: tåler hull i serien.
    key <- paste(result$Flyplass, format(result$Dato, "%Y-%m"))
    previous <- paste(result$Flyplass, sprintf("%04d-%s",
      as.integer(format(result$Dato, "%Y")) - 1L, format(result$Dato, "%m")))
    base <- result$Passasjerer[match(previous, key)]
    result$Verdi <- ifelse(!is.na(base) & base > 0,
      100 * (result$Passasjerer / base - 1), NA_real_)
  }
  result <- result[result$Dato >= start & result$Dato <= end, ]
  if (mode == "index") {
    base_rows <- result[result$Dato == start, ]
    base <- base_rows$Passasjerer[match(result$Flyplass, base_rows$Flyplass)]
    result$Verdi <- ifelse(!is.na(base) & base > 0,
      100 * result$Passasjerer / base, NA_real_)
  }
  result
}
