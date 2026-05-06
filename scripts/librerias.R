# Librerias varias que vamos a usar

librerias <- c("vcd", "cowplot",
               "zoo",
               "tidyverse",
               "GGally",
               "naniar",
               "corrplot",
               "nnet",
               "mice",
               "VIM",
               "caret",
               "FactoMineR",
               "factoextra",
               "gridExtra",
               "knitr"
               )

# Comprobar si las librerías están instaladas y si no, instalarlas
for (libreria in librerias) {
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
  }
  
  # Cargar la librería
  library(libreria, character.only = TRUE)
}