# Code erarbeitet mit GPT-4o Canvas

# Funktion zur Berechnung der Punkte basierend auf einer Note
Punkterechner <- function(Note, MaxPunkte) {
  # Validierung der Eingabewerte
  if (!is.numeric(Note) || !is.numeric(MaxPunkte)) {
    stop("Beide Eingaben müssen numerisch sein.")
  }
  if (any(Note < 1 | Note > 6)) {
    stop("Note muss zwischen 1 und 6 liegen.")
  }
  if (MaxPunkte <= 0) {
    stop("MaxPunkte muss grösser als 0 sein.")
  }
  
  # Berechnung der Punkte für jede Note
  Punkte <- sapply(Note, function(n) {
    p <- ((n - 1) * MaxPunkte) / 5
    # Punkte sicherstellen (>= 0 und <= MaxPunkte)
    p <- max(0, min(p, MaxPunkte))
    return(round(p, 2))
  })
  
  return(Punkte)
}
