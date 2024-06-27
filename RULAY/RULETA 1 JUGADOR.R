tirada_ruleta <- function(saldo, apuesta, tipo_apuesta) {
  numeros_ruleta <- 0:36
  numero_ganador <- sample(numeros_ruleta, 1)
  resultado <- "pierde"
  ganancia <- 0
  
  if (tipo_apuesta == "par") {
    if (numero_ganador != 0 && numero_ganador %% 2 == 0) {
      ganancia <- apuesta
      saldo <- saldo + ganancia
      resultado <- "gana"
    } else {
      saldo <- saldo - apuesta
    }
  } else if (tipo_apuesta == "impar") {
    if (numero_ganador != 0 && numero_ganador %% 2 != 0) {
      ganancia <- apuesta
      saldo <- saldo + ganancia
      resultado <- "gana"
    } else {
      saldo <- saldo - apuesta
    }
  }
  
  return(list(saldo=saldo, resultado=resultado, numero_ganador=numero_ganador, ganancia=ganancia))
}

saldo_inicial <- 100
cantidad_apostada <- 10
num_tiradas <- 100
resultados <- data.frame(
  Numero_de_tirada = integer(num_tiradas),
  Saldo_inicial_de_la_jugada = numeric(num_tiradas),
  Cantidad_apostada = numeric(num_tiradas),
  Tipo_de_apuesta = character(num_tiradas),
  Numero_obtenido_en_ruleta = integer(num_tiradas),
  Resultado_de_la_apuesta = character(num_tiradas),
  Saldo_final_de_la_jugada = numeric(num_tiradas),
  Ganancia_perdida = numeric(num_tiradas),
  stringsAsFactors = FALSE
)

set.seed(NULL)
saldo <- saldo_inicial
for (i in 1:num_tiradas) {
  saldo_inicial_jugada <- saldo
  tipo_apuesta <- sample(c("par", "impar"), 1)
  
  resultado <- tirada_ruleta(saldo, cantidad_apostada, tipo_apuesta)
  saldo <- resultado$saldo
  ganancia_perdida <- ifelse(resultado$resultado == "gana", resultado$ganancia, -cantidad_apostada)
  saldo <- saldo_inicial_jugada + ganancia_perdida
  
  resultados[i, ] <- c(i, saldo_inicial_jugada, cantidad_apostada, tipo_apuesta, 
                       resultado$numero_ganador, resultado$resultado, saldo, ganancia_perdida)
}

resultados
cat("\nEl saldo final después de 100 tiradas es:", saldo, "soles\n")

ganancia_perdida_final <- saldo - saldo_inicial

if (ganancia_perdida_final > 0) {
  cat("Has ganado", ganancia_perdida_final, "soles en total.\n")
} else if (ganancia_perdida_final < 0) {
  cat("Has perdido", abs(ganancia_perdida_final), "soles en total.\n")
} else {
  cat("No has ganado ni perdido dinero en total.\n")
}
