# Pakete sicherstellen
pkgs <- c("textreuse", "readtext", "pdftools")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(textreuse)
library(readtext)

folder <- "/Users/samuel/Downloads/HS25BerichteP"

read_safe <- function(pattern) {
  tryCatch(readtext::readtext(file.path(folder, pattern)),
           error = function(e) data.frame())
}

# Dateien einlesen (PDF/DOCX)
texts_pdf  <- read_safe("*.pdf")
texts_docx <- read_safe("*.docx")

dfs <- Filter(function(d) nrow(d) > 0, list(texts_pdf, texts_docx))
if (!length(dfs)) stop("Keine PDF/DOCX gefunden. Pfad/Endungen prüfen; für PDF 'pdftools' nötig.")

texts_df <- do.call(rbind, dfs)

# Benannter Character-Vektor statt Liste
texts <- setNames(as.character(texts_df$text), basename(texts_df$doc_id))

# Leere Texte entfernen
texts <- texts[nchar(texts) > 0]
if (length(texts) < 2) stop("Weniger als zwei verwertbare Dokumente gefunden.")

# Tokenizer
token_fun <- function(x) tokenize_ngrams(x, n = 5)

# Corpus – einfache Variante ohne Minhash (exakter Jaccard)
corpus <- TextReuseCorpus(
  text = texts,
  tokenizer = token_fun,
  progress = FALSE
)

# Ähnlichkeit berechnen
sim <- pairwise_compare(corpus, jaccard_similarity)
m <- as.matrix(sim)

# Ausgaben
out_matrix <- file.path(folder, "similarities_matrix.csv")
write.csv(m, out_matrix, row.names = TRUE)

# Paarliste über Schwellwert
threshold <- 0.20
pairs <- which(upper.tri(m), arr.ind = TRUE)
long <- data.frame(
  doc1 = rownames(m)[pairs[,1]],
  doc2 = colnames(m)[pairs[,2]],
  similarity = m[pairs]
)
long <- long[order(-long$similarity), ]
long <- subset(long, similarity >= threshold)

out_long <- file.path(folder, sprintf("similarities_pairs_ge%02d.csv", as.integer(threshold*100)))
write.csv(long, out_long, row.names = FALSE)

cat("Gespeichert:\n",
    "- Matrix: ", out_matrix, "\n",
    "- Paare >= ", threshold, ": ", out_long, "\n", sep = "")


# Heatmap mit mehr Farbabstufungen
pkgs <- c("ggplot2", "scales")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
library(ggplot2)
library(scales)

dfm <- as.data.frame(as.table(m))
colnames(dfm) <- c("doc1","doc2","similarity")

p <- ggplot(dfm, aes(x = doc2, y = doc1, fill = similarity)) +
  geom_tile() +
  # Mehr Farbstufen: Blau -> Grün -> Gelb -> Orange -> Rot -> Violett
  scale_fill_gradientn(
    colours = c("#2166AC", "#67A9CF", "#D1E5F0", 
                "#FFFFBF", "#FDAE61", "#D73027", "#762A83"),
    values  = rescale(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1)),
    limits  = c(0, 1),
    name    = "Jaccard"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

out_png <- file.path(folder, "similarities_heatmap.png")
w <- max(6, 0.25 * ncol(m))
h <- max(6, 0.25 * nrow(m))
ggsave(out_png, p, width = w, height = h, dpi = 150)

cat("Gespeichert:\n",
    "- Heatmap: ", out_png, "\n", sep = "")