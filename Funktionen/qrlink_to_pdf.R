# Install necessary packages if not installed
if (!require("readxl")) install.packages("readxl", repos = "http://cran.us.r-project.org")
if (!require("grid")) install.packages("grid", repos = "http://cran.us.r-project.org")
if (!require("rsvg")) install.packages("rsvg", repos = "http://cran.us.r-project.org")
if (!require("svglite")) install.packages("svglite", repos = "http://cran.us.r-project.org")

# Load libraries
library(readxl)
library(grid)
library(rsvg)
library(svglite)

# Define paths
excel_file <- "Data/01_Ökologie/Flaschengarten_Miro.xlsx"  # Excel file path
qr_folder <- "Data/Export/qr_codes"  # Folder with QR codes (SVG)
pdf_folder <- "Data/Export/pdf_outputs"  # Output folder for PDFs

# Create output folder if it doesn't exist
dir.create(pdf_folder, showWarnings = FALSE)

# Read URLs from Excel file
urls <- readxl::read_xlsx(excel_file)$urls

# Generate PDFs for each QR code and URL
for (i in seq_along(urls)) {
  
  # Define file paths
  qr_filename <- sprintf("%s/QR_%02d.svg", qr_folder, i)  # Matching QR file
  pdf_filename <- sprintf("%s/QR_%02d.pdf", pdf_folder, i)  # Output PDF file
  
  # Check if QR file exists
  if (!file.exists(qr_filename)) {
    cat(sprintf("Warning: QR code file %s not found. Skipping...\n", qr_filename))
    next
  }
  
  # Convert SVG QR code to raster format
  qr_raster <- rsvg::rsvg(qr_filename)  # Load SVG as raster
  
  # Create PDF
  pdf(pdf_filename, width = 8, height = 6)  # PDF dimensions
  
  # Set up layout
  grid.newpage()
  
  # Add QR code (Centered)
  grid.raster(qr_raster, x = 0.5, y = 0.6, width = 0.5, height = 0.5)
  
  # Add URL below the QR code
  grid.text(urls[i], x = 0.5, y = 0.2, just = "center", gp = gpar(fontsize = 12))
  
  # Add a unique number on top of the figure
  grid.text(sprintf("#%02d", i), x = 0.5, y = 0.9, just = "center", gp = gpar(fontsize = 20, fontface = "bold"))
  
  dev.off()  # Close PDF file
}

cat("PDFs saved in 'Data/Export/pdf_outputs' folder.\n")
