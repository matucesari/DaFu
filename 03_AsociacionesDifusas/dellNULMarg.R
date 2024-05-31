# Filtrar columnas marginales nulas de una tabla de contigencia

dellNULMarg <- function(tabla) {
  # Reemplazar valores faltantes con cero en una tabla llamada 'mi_tabla'
  tabla[is.na(tabla)] <- 0
  suma_columnas <- colSums(tabla)
    return(as.data.frame(tabla[, suma_columnas != 0]))
}