# Grafik Blutzusammensetzung (leer)

# Code Writing Assisted by GPT-4o

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(behavdata)

# Set working directory
current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_file_path)

data <- readxl::read_xlsx(path = "Datensatz.xlsx")

# Rename columns for consistency
colnames(data) <- c('Blutfluss [l / min]', 'Temperatur [°C]', 'CO2 [%]', 'O2 [%]', 'Nährstoffe [%]', 'Antikörper [%]')

# Interpolate dataset with spline interpolation
num_points <- 30  # Number of interpolated points (reduced for better fit with small dataset)

# Function to add interpolation
interpolate_data <- function(x, y, num_points) {
  if (length(x) > 1 && length(y) > 1) {
    spline_result <- spline(x, y, n = num_points)
    return(data.frame(x = spline_result$x, y = spline_result$y))
  } else {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
}

# Interpolate all columns
blutfluss <- data$`Blutfluss [l / min]`

interpolated_temperature <- interpolate_data(blutfluss, data$`Temperatur [°C]`, num_points)
interpolated_co2 <- interpolate_data(blutfluss, data$`CO2 [%]`, num_points)
interpolated_o2 <- interpolate_data(blutfluss, data$`O2 [%]`, num_points)
interpolated_nutrients <- interpolate_data(blutfluss, data$`Nährstoffe [%]`, num_points)
interpolated_antibodies <- interpolate_data(blutfluss, data$`Antikörper [%]`, num_points)

# Combine interpolated data into one data frame
interpolated_data <- data.frame(
  `Blutfluss [l / min]` = interpolated_temperature$x,
  `Temperatur [°C]` = interpolated_temperature$y,
  `CO2 [%]` = interpolated_co2$y,
  `O2 [%]` = interpolated_o2$y,
  `Nährstoffe [%]` = interpolated_nutrients$y,
  `Antikörper [%]` = interpolated_antibodies$y
)


# Rename columns for consistency
colnames(interpolated_data) <- c('Blutfluss [l / min]', 'Temperatur [°C]', 'CO2 [%]', 'O2 [%]', 'Nährstoffe [%]', 'Antikörper [%]')


# Remove rows with NA or empty values
interpolated_data <- interpolated_data %>% na.omit()

# Add random noise to indicate uncertainty
set.seed(42)  # For reproducibility

add_uncertainty <- function(values, percentage = 0.05) {
  if (length(values) > 0) {
    noise <- runif(length(values), min = -percentage, max = percentage) * values
    return(values + noise)
  } else {
    return(values)
  }
}

# Apply uncertainty to interpolated data
interpolated_data$`Temperatur [°C]` <- add_uncertainty(interpolated_data$`Temperatur [°C]`, percentage =  0.005)
interpolated_data$`CO2 [%]` <- add_uncertainty(interpolated_data$`CO2 [%]`)
interpolated_data$`O2 [%]` <- add_uncertainty(interpolated_data$`O2 [%]`)
interpolated_data$`Nährstoffe [%]` <- add_uncertainty(interpolated_data$`Nährstoffe [%]`)
interpolated_data$`Antikörper [%]` <- add_uncertainty(interpolated_data$`Antikörper [%]`)

# Plot the interpolated data
pdf("nährstoffe_leer.pdf", width = 8, height = 5)
par(mar = c(5, 4, 2, 2) + 0.1)  
plot(interpolated_data$`Blutfluss [l / min]`, interpolated_data$`O2 [%]`, type = "s", pch = 20,
     lwd = 1.5, ylim = c(0, 15), col = "white",
     ylab = "Relative Menge [%]", xlab = "Blutfluss [L / min]")
legend("topleft", legend = c("Kohlenstoffdioxid [%]", "Sauerstoff [%]"),
       col = c("white","white"),
       lwd = 1.5, bty = "n", cex = 0.8, y.intersp = 1.2)
dev.off()


