ssb_metadata <- function() {
  metadata <- httr::with_config(httr::timeout(45),
    PxWebApiData::meta_frames("08507"))
  if (is.null(metadata)) stop("SSB returnerte ingen metadata.", call. = FALSE)
  metadata
}

ssb_choices <- function(metadata, code) {
  variable <- metadata[[code]]
  stats::setNames(variable$code, variable$label)
}

month_date <- function(x) as.Date(paste0(sub("M", "-", x), "-01"))

ssb_fetch <- function(metadata, airports, traffic, route, passengers) {
  # Gjenbruk metadata og behold dimensjonskodene uavhengig av etikettene.
  raw <- httr::with_config(httr::timeout(90), PxWebApiData::api_data_2(metadata,
    Lufthavn = airports, TrafikkType = traffic, TrafikkFly = route,
    PassasjerType = passengers, ContentsCode = "Passasjerer", Tid = "*"))
  if (is.null(raw)) stop("SSB returnerte ingen passasjertall.", call. = FALSE)
  if (!all(c("Lufthavn", "Tid", "value") %in% names(raw))) {
    stop("SSB returnerte et uventet dataformat.", call. = FALSE)
  }
  comment(raw) <- NULL
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
