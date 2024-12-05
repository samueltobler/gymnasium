# Notenberechnung (Laubblattbericht)

y <- function(x) {
  (x / 3)+1
}

y(17)

xplot <- seq(0,15, 0.5)
yplot <- y(xplot)

plot(xplot, yplot, type = "l", xlab = "Anzahl Punkte", ylab = "Note", yaxt = "n", lwd = 2)

axis(1, at = seq(0, 15, by = 1))
axis(2, at = seq(1, 6, by = 0.5), las=2)
abline(v = seq(0,15,3), lty = 2, col = "lightgrey")
abline(h = seq(1,6,1), lty = 2, col = "lightgrey")

lines(x = xplot, y = rep(4, length(xplot)), col = "red", lty = 1)
lines(y = yplot, x = rep(9, length(xplot)), col = "red", lty = 1)
