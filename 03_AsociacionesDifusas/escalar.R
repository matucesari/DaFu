# Función para escalar valores a un rango específico
escalar <- function(x, min_d, max_d) {
  max_x <- max(x)
  min_x <- min(x)
  M <- (max_d - min_d) / (max_x - min_x)
  B <- max_d - M * max_x
  scaled <- M * x + B
  return(scaled)
}