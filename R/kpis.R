kpi_month <- function(date) {
  months <- c("januar", "februar", "mars", "april", "mai", "juni",
    "juli", "august", "september", "oktober", "november", "desember")
  date <- as.Date(date)
  paste(months[as.integer(format(date, "%m"))], format(date, "%Y"))
}

# Avrund før fortegn og klasse velges, så små endringer vises som 0,0 %.
kpi_direction <- function(value) {
  if (!is.finite(value)) return("missing")
  value <- round(value, 1)
  if (value > 0) "positive" else if (value < 0) "negative" else "neutral"
}

kpi_format <- function(value, percentage = FALSE) {
  if (!is.finite(value)) return("–")
  if (!percentage) return(format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE))
  value <- round(value, 1)
  sign <- switch(kpi_direction(value), positive = "+", negative = "−", neutral = "")
  paste0(sign, format(abs(value), nsmall = 1, decimal.mark = ",",
    big.mark = " ", scientific = FALSE, trim = TRUE), " %")
}

kpi_cell <- function(value, percentage = FALSE, primary = FALSE) {
  state <- if (percentage) kpi_direction(value) else if (!is.finite(value)) "missing" else "count"
  shiny::tags$td(class = paste("kpi-number", paste0("kpi-", state),
    if (primary) "kpi-primary" else ""), kpi_format(value, percentage))
}

kpi_row <- function(airport, data) {
  values <- data$Verdi[match(c("count", "yoy", "sum12", "change12"), data$Visning)]
  shiny::tags$tr(
    shiny::tags$th(scope = "row", class = "kpi-airport", airport),
    kpi_cell(values[1], primary = TRUE), kpi_cell(values[2], percentage = TRUE),
    kpi_cell(values[3]), kpi_cell(values[4], percentage = TRUE)
  )
}

kpi_table <- function(data) {
  heading <- function(title, detail = NULL) {
    shiny::tags$th(scope = "col", title,
      if (!is.null(detail)) shiny::tags$span(class = "kpi-detail", detail))
  }
  rows <- lapply(unique(data$Flyplass), function(airport) {
    kpi_row(airport, data[data$Flyplass == airport, ])
  })
  shiny::tags$table(class = "kpi-table", `aria-labelledby` = "kpi-title",
    `aria-describedby` = "kpi-missing-note",
    shiny::tags$thead(shiny::tags$tr(
      heading("Flyplass"), heading("Passasjerer", "Valgt måned"),
      heading("Endring", "Mot samme måned i fjor"),
      heading("Passasjerer", "Siste 12 måneder"),
      heading("Endring", "Mot foregående 12 måneder")
    )),
    shiny::tags$tbody(rows)
  )
}
