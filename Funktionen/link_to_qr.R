# Load packages
library(qrcode)
library(ggplot2)
library(svglite)

# List of URLs
urls <- readxl::read_xlsx("Data/02_Photosynthese/PS_Miro.xlsx")$urls

# Generate and save QR codes in SVG format
for (i in seq_along(urls)) {
  # Create QR code
  qr <- qr_code(urls[i])
  
  # Define file name
  svg_filename <- sprintf("Data/Export/qr_codes/QR_PS_%02d.svg", i)
  
  # Save QR as SVG
  svglite::svglite(svg_filename)
  plot(qr)
  dev.off()
}

