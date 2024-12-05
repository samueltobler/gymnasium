# Notenrechner mit linearer Bewertungsfunktion

# Code erarbeitet mit GPT-4o Canvas

Notenrechner <- function(Punkte, MaxPunkte) {
  # Validierung der Eingabewerte
  if (!is.numeric(Punkte) || !is.numeric(MaxPunkte)) {
    stop("Beide Eingaben müssen numerisch sein.")
  }
  if (any(Punkte < 0)) {
    stop("Punkte müssen grösser oder gleich 0 sein.")
  }
  if (MaxPunkte <= 0) {
    stop("MaxPunkte muss grösser als 0 sein.")
  }
  
  # Punkte, die über MaxPunkte liegen, auf MaxPunkte setzen
  Punkte <- pmin(Punkte, MaxPunkte)
  
  # Berechnung der Note für jeden Wert in Punkte
  Note <- sapply(Punkte, function(p) {
    n <- round((p * 5 / MaxPunkte) + 1, 2)
    # Sicherstellen, dass Note zwischen 1 und 6 liegt
    n <- max(1, min(n, 6))
    return(n)
  })
  
  return(Note)
}