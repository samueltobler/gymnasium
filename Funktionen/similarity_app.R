# app.R
# Shiny-App: Upload PDF/DOCX -> Jaccard-Heatmap -> Downloads

# — Upload-Limite erhöhen (hier 200 MB)
options(shiny.maxRequestSize = 200*1024^2)

# — Pakete sicherstellen
pkgs <- c("shiny", "ggplot2", "scales", "textreuse", "readtext", "pdftools")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(shiny)
library(ggplot2)
library(scales)
library(textreuse)
library(readtext)

ui <- fluidPage(
  titlePanel("Ähnlichkeits-Heatmap (Jaccard, n-Gramme)"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "files", "Dateien wählen (PDF/DOCX)",
        accept = c(".pdf", ".docx"),
        multiple = TRUE
      ),
      numericInput("ngram", "n-Gramm-Grösse", value = 5, min = 1, max = 10, step = 1),
      sliderInput("thr", "Schwellwert für Paarliste", min = 0, max = 1, value = 0.20, step = 0.01),
      actionButton("run", "Analysieren"),
      tags$hr(),
      strong("Downloads"),
      br(),
      downloadButton("dl_heatmap", "Heatmap (PNG)"),
      downloadButton("dl_mtx", "Matrix (CSV)"),
      downloadButton("dl_pairs", "Paare ≥ Schwellwert (CSV)")
    ),
    mainPanel(
      uiOutput("info"),
      plotOutput("heatmap", height = "auto")
    )
  )
)

server <- function(input, output, session) {
  
  # — Helfer
  make_tokens <- reactive({
    function(x) tokenize_ngrams(x, n = req(input$ngram))
  })
  
  read_uploads <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # readtext liest gemischte Formate; Originalnamen übernehmen
    rt <- readtext(df$datapath)
    # Mapping auf Originalnamen; falls readtext doc_id anpasst, auf Basenamen matchen
    idx <- match(rt$doc_id, basename(df$datapath))
    # Fallback: wenn das nicht klappt, versuche auf Dateiendungen zu matchen
    if (anyNA(idx)) {
      rt_base <- tools::file_path_sans_ext(basename(rt$doc_id))
      up_base <- tools::file_path_sans_ext(basename(df$datapath))
      idx2 <- match(rt_base, up_base)
      idx[is.na(idx) & !is.na(idx2)] <- idx2[is.na(idx) & !is.na(idx2)]
    }
    rt$doc_id <- ifelse(!is.na(idx), basename(df$name[idx]), basename(rt$doc_id))
    rt
  }
  
  # — Analyse
  result <- eventReactive(input$run, {
    validate(
      need(!is.null(input$files) && nrow(input$files) >= 2, "Mindestens zwei Dateien hochladen.")
    )
    
    withProgress(message = "Lese Dateien…", value = 0.2, {
      rt <- read_uploads(input$files)
      validate(need(!is.null(rt) && nrow(rt) >= 2, "Keine verwertbaren Texte gefunden."))
      
      texts <- setNames(as.character(rt$text), basename(rt$doc_id))
      texts <- texts[nchar(texts) > 0]
      validate(need(length(texts) >= 2, "Weniger als zwei nicht-leere Dokumente."))
      
      incProgress(0.3, detail = "Tokenisiere…")
      corpus <- TextReuseCorpus(
        text = texts,
        tokenizer = make_tokens(),
        progress = FALSE
      )
      
      incProgress(0.4, detail = "Berechne Ähnlichkeiten…")
      sim <- pairwise_compare(corpus, jaccard_similarity)
      m <- as.matrix(sim)
      
      ord <- rownames(m)  # Reihenfolge wie von textreuse geliefert
      m <- m[ord, ord, drop = FALSE]
      
      list(
        m = m,
        docs = ord
      )
    })
  }, ignoreInit = TRUE)
  
  output$info <- renderUI({
    req(result())
    HTML(sprintf(
      "<p><b>Dateien:</b> %s<br><b>n-Gramm:</b> %d</p>",
      paste(result()$docs, collapse = ", "),
      input$ngram
    ))
  })
  
  # — Heatmap
  output$heatmap <- renderPlot({
    req(result())
    m <- result()$m
    
    dfm <- as.data.frame(as.table(m))
    colnames(dfm) <- c("doc1", "doc2", "similarity")
    
    ggplot(dfm, aes(x = doc2, y = doc1, fill = similarity)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = c("#2166AC", "#67A9CF", "#D1E5F0",
                    "#FFFFBF", "#FDAE61", "#D73027", "#762A83"),
        values = rescale(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1)),
        limits = c(0, 1),
        name = "Jaccard"
      ) +
      coord_fixed() +
      theme_minimal(base_size = 11) +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank()
      )
  }, height = function() {
    req(result())
    # dynamische Höhe: 25 px pro Dokument, min 400 px
    max(400, 25 * nrow(result()$m))
  })
  
  # — Downloads
  output$dl_heatmap <- downloadHandler(
    filename = function() sprintf("similarities_heatmap_ngram%d.png", input$ngram),
    content = function(file) {
      req(result())
      m <- result()$m
      dfm <- as.data.frame(as.table(m))
      colnames(dfm) <- c("doc1", "doc2", "similarity")
      
      p <- ggplot(dfm, aes(x = doc2, y = doc1, fill = similarity)) +
        geom_tile() +
        scale_fill_gradientn(
          colours = c("#2166AC", "#67A9CF", "#D1E5F0",
                      "#FFFFBF", "#FDAE61", "#D73027", "#762A83"),
          values = rescale(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1)),
          limits = c(0, 1),
          name = "Jaccard"
        ) +
        coord_fixed() +
        theme_minimal(base_size = 11) +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid = element_blank()
        )
      
      w <- max(6, 0.25 * ncol(m))
      h <- max(6, 0.25 * nrow(m))
      ggsave(file, p, width = w, height = h, dpi = 150)
    }
  )
  
  output$dl_mtx <- downloadHandler(
    filename = function() sprintf("similarities_matrix_ngram%d.csv", input$ngram),
    content = function(file) {
      req(result())
      write.csv(result()$m, file, row.names = TRUE)
    }
  )
  
  output$dl_pairs <- downloadHandler(
    filename = function() sprintf("similarities_pairs_ge%02d_ngram%d.csv",
                                  as.integer(input$thr * 100), input$ngram),
    content = function(file) {
      req(result())
      m <- result()$m
      pairs <- which(upper.tri(m), arr.ind = TRUE)
      long <- data.frame(
        doc1 = rownames(m)[pairs[, 1]],
        doc2 = colnames(m)[pairs[, 2]],
        similarity = m[pairs]
      )
      long <- long[order(-long$similarity), ]
      long <- subset(long, similarity >= input$thr)
      write.csv(long, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
