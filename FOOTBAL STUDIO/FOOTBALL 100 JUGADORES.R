football_studio <- function(saldo, apuesta, eleccion) {
  carta_izquierda <- sample(1:13, 1)
  carta_derecha <- sample(1:13, 1)
  
  saldo_inicial_jugada <- saldo 
  
  if (eleccion == "izquierda") {
    if (carta_izquierda > carta_derecha) {
      ganancia <- apuesta * 2
      saldo <- saldo_inicial_jugada + ganancia - apuesta
      resultado <- "gana"
    } else if (carta_izquierda < carta_derecha) {
      saldo <- saldo_inicial_jugada - apuesta
      resultado <- "pierde"
    } else {
      saldo <- saldo_inicial_jugada - apuesta / 2
      resultado <- "empate"
    }
  } else if (eleccion == "derecha") {
    if (carta_derecha > carta_izquierda) {
      ganancia <- apuesta * 2
      saldo <- saldo_inicial_jugada + ganancia - apuesta
      resultado <- "gana"
    } else if (carta_derecha < carta_izquierda) {
      saldo <- saldo_inicial_jugada - apuesta
      resultado <- "pierde"
    } else {
      saldo <- saldo_inicial_jugada - apuesta / 2
      resultado <- "empate"
    }
  } else {
    cat("Opción invalida. Elige 'izquierda' o 'derecha'.\n")
    return(list(saldo = saldo, resultado = "invalido"))
  }
  
  return(list(saldo = saldo, resultado = resultado))
}



simular_jugador_football_studio <- function(saldo_inicial, num_jugadas) {
  resultados <- data.frame(
    Numero_de_jugada = 1:num_jugadas,
    Saldo_inicial_de_la_jugada = rep(NA, num_jugadas),
    Ganancia_o_Perdida = rep(0, num_jugadas)
  )
  
  saldo <- saldo_inicial
  for (i in 1:num_jugadas) {
    resultados[i, "Saldo_inicial_de_la_jugada"] <- saldo
    
    eleccion <- sample(c("izquierda", "derecha"), 1)
    resultado_jugada <- football_studio(saldo, 10, eleccion)
    
    saldo <- resultado_jugada$saldo
    resultados[i, "Ganancia_o_Perdida"] <- saldo - saldo_inicial
  }
  
  return(resultados)
}



num_jugadores <- 100
saldo_inicial <- 100
num_jugadas <- 100



set.seed(NULL)  
resultados_globales <- list()
for (j in 1:num_jugadores) {
  resultados_jugador <- simular_jugador_football_studio(saldo_inicial, num_jugadas)
  resultados_globales[[j]] <- resultados_jugador
}



tabla_ganancia_perdida <- data.frame(
  Numero_de_jugador = 1:num_jugadores,
  Ganancia_o_Perdida = sapply(resultados_globales, function(x) tail(x$Ganancia_o_Perdida, 1))
)

print("Tabla de ganancia o pérdida de cada jugador:")
print(tabla_ganancia_perdida)



ganadores <- sum(tabla_ganancia_perdida$Ganancia_o_Perdida > 0)
perdedores <- sum(tabla_ganancia_perdida$Ganancia_o_Perdida < 0)
porcentaje_ganadores <- (ganadores / num_jugadores) * 100
porcentaje_perdedores <- (perdedores / num_jugadores) * 100

cat("\nPorcentaje de jugadores que ganan dinero:", porcentaje_ganadores, "%\n")
cat("Porcentaje de jugadores que pierden dinero:", porcentaje_perdedores, "%\n")



ganancia_casa_apuestas <- sum(tabla_ganancia_perdida$Ganancia_o_Perdida)



if (ganancia_casa_apuestas < 0) {
  cat("La casa de apuestas ha ganado:", abs(ganancia_casa_apuestas), "soles.\n")
} else if (ganancia_casa_apuestas > 0) {
  cat("La casa de apuestas ha perdido:", ganancia_casa_apuestas, "soles.\n")
} else {
  cat("La casa de apuestas no ha ganado ni perdido dinero.\n")
}
