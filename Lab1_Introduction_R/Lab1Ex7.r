# --- Funcție care citește fișierul și face plot ---
plotFromFile <- function(filePath) {

  text_data <- readLines(filePath)
  
  cat("Conținutul fișierului:\n")
  cat(text_data, sep = "\n")
  
  # --- Grafic primele două coloane ---
  quartz()
  
  x <- data_numeric[,1]
  y <- data_numeric[,2]
  
  plot(x, y, type = "o",
       main = "Plot primele două coloane cu linii între puncte",
       xlab = "Coloana 1",
       ylab = "Coloana 2",
       pch = 19, col = "blue")

  return(data_numeric)
}

file_path <- "/Users/alexandramanea/data.txt"
mat_from_file <- plotFromFile(file_path)

