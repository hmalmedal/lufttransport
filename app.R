library(shiny)
library(ggplot2)
source("R/ssb.R", encoding = "UTF-8")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f3f6fa; color: #183047; }
    .container-fluid { max-width: 1450px; padding: 24px; }
    h1 { font-weight: 750; letter-spacing: -1px; }
    .well { background: white; border: 1px solid #dce4ed; border-radius: 12px; }
    .tab-content { background: white; padding: 20px; border-radius: 0 0 12px 12px; }
    .btn-primary { background: #126678; border-color: #126678; }
    .intro { color: #53697d; margin-bottom: 24px; font-size: 17px; }
  "))),
  titlePanel("Flytrafikk i Norge"),
  p(class = "intro", "Sammenlign passasjerutviklingen ved norske flyplasser · SSB tabell 08507"),
  sidebarLayout(
    sidebarPanel(width = 3,
      actionButton("metadata_refresh", "Oppdater flyplasser og perioder"),
      uiOutput("filters"),
      actionButton("fetch", "Hent passasjertall", class = "btn-primary"),
      hr(),
      radioButtons("mode", "Sammenlign med", c("Antall passasjerer" = "count",
        "Indeks: første valgte måned = 100" = "index",
        "Endring fra samme måned året før (%)" = "yoy")),
      uiOutput("period"),
      downloadButton("download", "Last ned viste data (CSV)")
    ),
    mainPanel(width = 9,
      textOutput("status"),
      tabsetPanel(
        tabPanel("Utvikling", plotOutput("plot", height = "510px"), textOutput("explanation")),
        tabPanel("Datatabell", p("Manglende observasjoner vises som NA."), tableOutput("table")),
        tabPanel("Om tallene",
          h3("Hva sammenlignes?"),
          p("Tabellen måler passasjerer, ikke antall flybevegelser eller unike reisende. ",
            "Standardvalget er all kommersiell flyging, innenlands og utenlands, med passasjerer ved både avgang og ankomst."),
          p("En innenlandsreise kan telles ved begge flyplassene. Tallene bør derfor ikke summeres til antall unike reisende."),
          p("Indeksen bruker samme startmåned for alle flyplasser. Serier med null eller manglende verdi i startmåneden får ingen indeks. ",
            "Årsendringen sammenligner hver måned med samme måned året før; null eller manglende sammenligningsgrunnlag gir NA."),
          p("Tallene er ikke sesongjustert. Manglende verdier beholdes som NA, og kurvene brytes ved hull. ",
            "Historiske tall kan bli revidert. Klikk Hent passasjertall for å hente på nytt."),
          a("Åpne tabell 08507 hos SSB", href = "https://www.ssb.no/statbank/table/08507", target = "_blank")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  metadata <- reactiveVal(NULL)
  loaded <- reactiveVal(NULL)
  metadata_error <- reactiveVal(NULL)
  observeEvent(input$metadata_refresh, {
    tryCatch({
      metadata(ssb_metadata())
      metadata_error(NULL)
    }, error = function(e) {
      metadata_error(paste("Kunne ikke hente metadata fra SSB:", conditionMessage(e)))
      showNotification(metadata_error(), type = "error", duration = NULL)
    })
  }, ignoreNULL = FALSE)

  output$filters <- renderUI({
    validate(need(!is.null(metadata()), if (is.null(metadata_error())) "Henter valg fra SSB …" else metadata_error()))
    m <- metadata()
    tagList(
      selectizeInput("airports", "Flyplasser", ssb_choices(m, "Lufthavn"),
        selected = c("ENGM", "ENBR", "ENVA", "ENZV"), multiple = TRUE),
      selectInput("traffic", "Trafikktype", ssb_choices(m, "TrafikkType"), selected = "000"),
      selectInput("route", "Innenlands / utenlands", ssb_choices(m, "TrafikkFly"), selected = "IU"),
      selectInput("passengers", "Passasjergruppe", ssb_choices(m, "PassasjerType"), selected = "AAT")
    )
  })

  observeEvent(input$fetch, {
    req(metadata())
    if (!length(input$airports)) {
      showNotification("Velg minst én flyplass.", type = "warning")
      return()
    }
    tryCatch(withProgress(message = "Henter passasjertall fra SSB", value = 0.2, {
      data <- ssb_fetch(metadata(), input$airports, input$traffic, input$route, input$passengers)
      label <- function(code, value) names(ssb_choices(metadata(), code))[match(value, ssb_choices(metadata(), code))]
      loaded(list(data = data, time = Sys.time(), airports = input$airports,
        traffic = input$traffic, route = input$route, passengers = input$passengers,
        description = paste(label("TrafikkType", input$traffic), label("TrafikkFly", input$route),
          label("PassasjerType", input$passengers), sep = " · ")))
    }), error = function(e) {
      loaded(NULL)
      showNotification(paste("Kunne ikke hente tall fra SSB. Prøv igjen.", conditionMessage(e)),
        type = "error", duration = NULL)
    })
  })

  output$period <- renderUI({
    req(loaded())
    months <- sort(unique(loaded()$data$Dato))
    choices <- stats::setNames(as.character(months), format(months, "%Y-%m"))
    tagList(selectInput("start", "Fra måned", choices,
      selected = as.character(months[max(1, length(months) - 59)])),
      selectInput("end", "Til måned", choices, selected = as.character(max(months))))
  })

  output$status <- renderText({
    if (is.null(loaded())) return("Velg flyplasser og klikk Hent passasjertall for å starte.")
    x <- loaded()
    changed <- !setequal(input$airports, x$airports) || !identical(input$traffic, x$traffic) ||
      !identical(input$route, x$route) || !identical(input$passengers, x$passengers)
    paste(if (changed) "Valgene er endret – klikk Hent passasjertall for å bruke dem. Viser fortsatt:" else "Viser:",
      x$description, "· Hentet", format(x$time, "%d.%m.%Y %H:%M"))
  })

  displayed <- reactive({
    req(loaded(), input$start, input$end)
    start <- as.Date(input$start)
    end <- as.Date(input$end)
    validate(need(start <= end, "Fra måned må være før eller lik til måned."))
    traffic_series(loaded()$data, start, end, input$mode)
  })
  axis_title <- reactive(switch(input$mode, count = "Passasjerer", index = "Indeks (startmåned = 100)",
    yoy = "Endring fra året før (%)"))
  output$plot <- renderPlot({
    d <- displayed()
    validate(need(any(is.finite(d$Verdi)), "Ingen beregnbare verdier for valget. Prøv en annen periode eller visning."))
    ggplot(d, aes(Dato, Verdi, colour = Flyplass, group = Flyplass)) +
      geom_line(linewidth = 0.85, na.rm = TRUE) + geom_point(size = 1, na.rm = TRUE) +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
      scale_x_date(date_labels = "%Y-%m") +
      labs(x = NULL, y = axis_title(), colour = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), legend.text = element_text(size = 10)) +
      guides(colour = guide_legend(ncol = 2))
  })
  output$explanation <- renderText({
    d <- displayed()
    paste(switch(input$mode, count = "Månedlige passasjertall, uten sesongjustering.",
      index = paste("Felles basis:", format(as.Date(input$start), "%Y-%m"), "= 100."),
      yoy = "Prosentvis endring fra samme måned året før."),
      sum(is.na(d$Verdi)), "observasjoner mangler eller kan ikke beregnes.")
  })
  output$table <- renderTable({
    d <- displayed()
    d$Dato <- format(d$Dato, "%Y-%m")
    d
  }, striped = TRUE, digits = 1, na = "NA")
  output$download <- downloadHandler(
    filename = function() paste0("ssb-08507-", input$mode, "-", Sys.Date(), ".csv"),
    content = function(file) {
      d <- displayed()
      d$Visning <- axis_title()
      d$Utvalg <- loaded()$description
      d$Kilde <- "SSB tabell 08507"
      d$Hentet <- format(loaded()$time, "%Y-%m-%d %H:%M:%S")
      write.csv2(d, file, row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
    }
  )
}

shinyApp(ui, server)
