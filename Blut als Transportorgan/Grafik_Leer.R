# Grafik Blutzusammensetzung (leer)

pdf("nährstoffe_leer.pdf", width = 8, height = 5)
par(mar = c(5, 4, 2, 2) + 0.1)  
plot(3:7.5, 0:4, type = "s", pch = 20,
     lwd = 1.5, ylim = c(0, 15), col = "white",
     ylab = "Relative Menge [%]", xlab = "Blutfluss [L / min]")
legend("topleft", legend = c("Kohlenstoffdioxid [%]", "Sauerstoff [%]"),
       col = c("black","black"),lty = c(1,3),
       lwd = 1, bty = "n", cex = 0.8, y.intersp = 1.2)
dev.off()


