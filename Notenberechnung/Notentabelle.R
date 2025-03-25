notenrechner_split <- function(p, plim) {
  if(p < 0.8) {
    n = (p*3/plim) + 1
  } else {
    n = p*2/(1-plim) + 6 - 2/(1-plim)
  }
  return(n)
}

notenrechner_split <- function(p, plim) {
  n <- numeric(length(p))       
  
  for(i in seq_along(p)) {
    if(p[i] < plim) {
      n[i] <- (p[i] * 3 / plim) + 1
    } else {
      n[i] <- (p[i] * 2 / (1 - plim)) + 6 - (2 / (1 - plim))
    }
  }
  
  return(n)
}

notentabelle <- function(plim) {
  vals <- c(1:100) / 100
  grade <- notenrechner_split(vals, plim)
  df <- data.frame(vals = vals, grade = grade)
  plot(df$vals, df$grade, type = "l",
       xlab = "Relative Anzahl Punkte", 
       ylab = "Note", 
       main = "Notentabelle")
  plot_obj <- recordPlot()
  return(list(
    data = df,
    plot = plot_obj
  ))
}

notentabelle(0.9)

