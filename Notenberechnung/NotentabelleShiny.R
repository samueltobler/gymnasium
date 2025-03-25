# Lade das Shiny-Package
library(shiny)

# Deine Funktionen ----------------------------------------
notenrechner_split <- function(p, plim) {
  n <- numeric(length(p))
  for(i in seq_along(p)) {
    if(p[i] < 0.8) {
      n[i] <- (p[i] * 3 / plim) + 1
    } else {
      n[i] <- (p[i] * 2 / (1 - plim)) + 6 - (2 / (1 - plim))
    }
  }
  return(n)
}

# Funktion ohne Plot --------------------------------------
notentabelle <- function(plim, steps) {
  # Vektor der Länge 'steps' zwischen 0 und 1
  vals <- seq_len(steps) / steps
  
  # Noten berechnen
  grade <- notenrechner_split(vals, plim)
  
  # Data Frame
  df <- data.frame(
    "RelativePunktzahl"  = vals,
    "Note" = grade
  )
  
  # Gibt direkt den Data Frame zurück
  return(df)
}

# Shiny-App -----------------------------------------------
ui <- fluidPage(
  titlePanel("Notentabelle-Applikation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("plim", "Relativer Grenzwert für 4er:",
                   value = 0.8, min = 0, max = 1, step = 0.01),
      numericInput("steps", "Anzahl Teilwerte (Schritte):",
                   value = 10, min = 1, max = 10000, step = 1),
      downloadButton("downloadData", "Tabelle herunterladen")
    ),
    mainPanel(
      tableOutput("notentable")
    )
  )
)

server <- function(input, output, session) {
  
  # Reaktive Berechnung
  results <- reactive({
    notentabelle(input$plim, input$steps)
  })
  
  # Tabelle anzeigen
  output$notentable <- renderTable({
    results()
  })
  
  # Download-Handler für die Tabelle
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Notentabelle_plim-", input$plim, "_steps-", input$steps, ".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

# App starten
shinyApp(ui = ui, server = server)
