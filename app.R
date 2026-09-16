library(shiny)
library(ggplot2)
source("R/ssb.R", encoding = "UTF-8")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  title = "Flytrafikk i Norge",
  tags$header(class = "app-header",
    div(class = "eyebrow", "LUFTTRANSPORT / SSB 08507"),
    h1("Flytrafikk i Norge"),
    p(class = "intro", "Sammenlign passasjerutviklingen ved norske flyplasser over tid.")
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
      h2(class = "section-title", "Velg trafikk"),
      actionButton("metadata_refresh", "Oppdater flyplasser og perioder"),
      uiOutput("filters"),
      actionButton("fetch", "Hent passasjertall", class = "btn-primary"),
      hr(),
      radioButtons("mode", "Sammenlign med", c("Antall passasjerer" = "count",
        "Endring fra samme måned året før (%)" = "yoy",
        "12 måneders glidende sum" = "sum12",
        "Rullerende 12-måneders endring (%)" = "change12")),
      uiOutput("period"),
      downloadButton("download", "Last ned viste data (CSV)")
    ),
    mainPanel(width = 9,
      div(class = "status-panel", role = "status", textOutput("status")),
      tabsetPanel(
        tabPanel("Utvikling", plotOutput("plot", height = "510px"), textOutput("explanation")),
        tabPanel("Datatabell", p("Manglende observasjoner vises som NA."),
          div(class = "table-scroll", tableOutput("table"))),
        tabPanel("Om tallene",
          h3("Hva sammenlignes?"),
          p("Tabellen måler passasjerer, ikke antall flybevegelser eller unike reisende. ",
            "Standardvalget er all kommersiell flyging, innenlands og utenlands, med passasjerer ved både avgang og ankomst."),
          p("En innenlandsreise kan telles ved begge flyplassene. Tallene bør derfor ikke summeres til antall unike reisende."),
          p("Årsendringen sammenligner hver måned med samme måned året før; null eller manglende sammenligningsgrunnlag gir NA."),
          p("12 måneders glidende sum er samlet antall passasjerer i inneværende måned og de elleve foregående månedene. ",
            "Alle tolv måneder må ha tall; ellers vises NA. Beregningen bruker også måneder før valgt visningsperiode."),
          p("Rullerende 12-måneders endring sammenligner summen for de siste tolv månedene med summen for de foregående tolv månedene, i prosent. ",
            "Beregningen krever tall for alle 24 måneder og en positiv sum i sammenligningsperioden; ellers vises NA."),
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
  axis_title <- reactive(switch(input$mode, count = "Passasjerer",
    yoy = "Endring fra året før (%)", sum12 = "Passasjerer (12 måneders sum)",
    change12 = "Rullerende 12-måneders endring (%)"))
  output$plot <- renderPlot({
    d <- displayed()
    validate(need(any(is.finite(d$Verdi)), "Ingen beregnbare verdier for valget. Prøv en annen periode eller visning."))
    if (input$mode == "yoy") {
      plot <- ggplot(d, aes(Dato, Verdi, fill = Flyplass, group = Flyplass)) +
        geom_hline(yintercept = 0, colour = "#536878", linewidth = 0.4) +
        geom_col(width = 25, position = position_dodge(width = 25), na.rm = TRUE)
    } else {
      plot <- ggplot(d, aes(Dato, Verdi, colour = Flyplass, group = Flyplass)) +
        geom_line(linewidth = 0.85, na.rm = TRUE) + geom_point(size = 1, na.rm = TRUE)
    }
    plot +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
      scale_x_date(date_labels = "%Y-%m") +
      labs(x = NULL, y = axis_title(), colour = NULL, fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), legend.text = element_text(size = 10)) +
      guides(colour = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))
  })
  output$explanation <- renderText({
    d <- displayed()
    paste(switch(input$mode, count = "Månedlige passasjertall, uten sesongjustering.",
      yoy = "Prosentvis endring fra samme måned året før.",
      sum12 = "Sum av inneværende måned og de elleve foregående. Krever tall for alle tolv måneder.",
      change12 = "Prosentvis endring i summen for de siste 12 månedene mot de foregående 12 månedene. Krever 24 måneder med tall og positiv sammenligningssum."),
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
      readr::write_excel_csv2(d, file, na = "NA")
    }
  )
}

shinyApp(ui, server)
