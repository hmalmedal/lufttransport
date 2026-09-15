ssb_url <- "https://data.ssb.no/api/pxwebapi/v2/tables/08507"

ssb_normalize_metadata <- function(raw) {
  # JSON-stat2 har koder og etiketter i dimension/category.
  # Sorter etter indeksverdiene, ikke rekkefølgen på JSON-objektnøklene.
  variables <- lapply(unlist(raw$id), function(code) {
    dimension <- raw$dimension[[code]]
    index <- unlist(dimension$category$index)
    values <- if (is.null(names(index))) index else names(index)[order(index)]
    labels <- unlist(dimension$category$label)[values]
    list(code = code, text = dimension$label, values = unname(values),
      valueTexts = unname(labels))
  })
  list(title = raw$label, variables = variables)
}

ssb_metadata <- function() {
  response <- httr::GET(paste0(ssb_url, "/metadata"),
    query = list(lang = "no"), httr::timeout(45))
  httr::stop_for_status(response, "hente metadata fra SSB")
  ssb_normalize_metadata(httr::content(response, as = "parsed", encoding = "UTF-8"))
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
    list(variableCode = code, valueCodes = unname(as.list(selections[[code]])))
  })
  response <- httr::POST(paste0(ssb_url, "/data"),
    query = list(lang = "no", outputFormat = "json-stat2"),
    body = list(selection = query), encode = "json", httr::timeout(90))
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
  if (mode == "sum12") {
    # Beregn før periodefilteret, med tolv sammenhengende kalendermåneder.
    month <- as.integer(format(result$Dato, "%Y")) * 12L +
      as.integer(format(result$Dato, "%m"))
    key <- paste(result$Flyplass, month)
    result$Verdi <- vapply(seq_len(nrow(result)), function(i) {
      window <- result$Passasjerer[match(
        paste(result$Flyplass[i], month[i] - 0:11), key)]
      if (anyNA(window)) NA_real_ else sum(window)
    }, numeric(1))
  }
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
