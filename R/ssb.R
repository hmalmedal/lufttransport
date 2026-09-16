ssb_url <- "https://data.ssb.no/api/pxwebapi/v2/tables/08507/"

ssb_normalize_metadata <- function(raw) {
  # JSON-stat2 har koder og etiketter i dimension/category.
  # Sorter etter indeksverdiene, ikke rekkefølgen på JSON-objektnøklene.
  variables <- purrr::map(raw$id, function(code) {
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
  httr2::url_modify_relative(ssb_url, "metadata") |>
    httr2::request() |>
    httr2::req_url_query(lang = "no") |>
    httr2::req_timeout(45) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE) |>
    ssb_normalize_metadata()
}

ssb_variable <- function(metadata, code) {
  purrr::detect(metadata$variables, ~ identical(.x$code, code))
}

ssb_choices <- function(metadata, code) {
  variable <- ssb_variable(metadata, code)
  purrr::set_names(unlist(variable$values), unlist(variable$valueTexts))
}

month_date <- function(x) as.Date(paste0(sub("M", "-", x), "-01"))

ssb_fetch <- function(metadata, airports, traffic, route, passengers) {
  selections <- list(
    Lufthavn = airports, TrafikkType = traffic, TrafikkFly = route,
    PassasjerType = passengers, ContentsCode = "Passasjerer",
    Tid = unlist(ssb_variable(metadata, "Tid")$values)
  )
  query <- selections |>
    purrr::imap(~ list(variableCode = .y, valueCodes = unname(as.list(.x)))) |>
    unname()
  response <- httr2::url_modify_relative(ssb_url, "data") |>
    httr2::request() |>
    httr2::req_url_query(lang = "no", outputFormat = "json-stat2") |>
    httr2::req_body_json(list(selection = query)) |>
    httr2::req_timeout(90) |>
    httr2::req_perform()
  # Behold dimensjonskodene slik at endrede etiketter ikke knekker appen.
  raw <- response |>
    httr2::resp_body_string(encoding = "UTF-8") |>
    rjstat::fromJSONstat(naming = "id")
  stopifnot(all(c("Lufthavn", "Tid", "value") %in% names(raw)))
  choices <- ssb_choices(metadata, "Lufthavn")
  airports <- tibble::enframe(choices, name = "Flyplass", value = "Lufthavn")
  raw |>
    tibble::as_tibble() |>
    dplyr::left_join(airports, by = "Lufthavn") |>
    dplyr::transmute(Flyplass, Dato = month_date(Tid), Passasjerer = as.numeric(value)) |>
    dplyr::arrange(Flyplass, Dato)
}

traffic_series <- function(data, start, end, mode) {
  # Fyll kalenderhull før lag(), slik at 12 rader alltid betyr 12 måneder.
  # Behold radnummeret for å returnere bare opprinnelige rader, i samme orden.
  result <- data |>
    tibble::as_tibble() |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::group_by(Flyplass) |>
    tidyr::complete(Dato = seq(min(Dato), max(Dato), by = "month")) |>
    dplyr::arrange(Dato, .by_group = TRUE) |>
    dplyr::mutate(Verdi = as.numeric(Passasjerer))

  if (mode %in% c("sum12", "change12")) {
    result <- result |>
      dplyr::mutate(Verdi = purrr::map(0:11, ~ dplyr::lag(Verdi, .x)) |>
        purrr::reduce(`+`))
  }
  if (mode %in% c("yoy", "change12")) {
    result <- result |>
      dplyr::mutate(.base = dplyr::lag(Verdi, 12),
        Verdi = dplyr::if_else(.base > 0, 100 * (Verdi / .base - 1), NA_real_))
  }
  result |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.row), dplyr::between(Dato, start, end)) |>
    dplyr::arrange(.row) |>
    dplyr::select(-dplyr::any_of(c(".row", ".base")))
}
