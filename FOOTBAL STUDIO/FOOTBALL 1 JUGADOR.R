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
    cat("Opción inválida. Elige 'izquierda' o 'derecha'.\n")
    return(list(saldo = saldo, resultado = "invalido"))
  }
  
  return(list(saldo = saldo, resultado = resultado, carta_izquierda = carta_izquierda, carta_derecha = carta_derecha))
}



simular_jugador_football_studio <- function(saldo_inicial, num_jugadas) {
  resultados <- data.frame(
    Numero_de_jugada = 1:num_jugadas,
    Saldo_inicial_de_la_jugada = rep(NA, num_jugadas),  
    Apuesta = rep(10, num_jugadas),
    Carta_elegida = character(num_jugadas),
    Carta_izquierda = integer(num_jugadas),
    Carta_derecha = integer(num_jugadas),
    Resultado = character(num_jugadas),
    Saldo_final_de_la_jugada = numeric(num_jugadas),
    stringsAsFactors = FALSE
  )
  
  saldo <- saldo_inicial
  for (i in 1:num_jugadas) {
    resultados[i, "Saldo_inicial_de_la_jugada"] <- saldo
    
    eleccion <- sample(c("izquierda", "derecha"), 1)
    resultado_jugada <- football_studio(saldo, 10, eleccion)
    
    resultados[i, c("Carta_izquierda", "Carta_derecha", "Carta_elegida", "Resultado", "Saldo_final_de_la_jugada")] <- 
      c(resultado_jugada$carta_izquierda, resultado_jugada$carta_derecha, eleccion, resultado_jugada$resultado, resultado_jugada$saldo)
    
    saldo <- resultado_jugada$saldo
  }
  
  return(resultados)
}


saldo_inicial <- 100  
num_jugadas <- 100  


resultados_jugador <- simular_jugador_football_studio(saldo_inicial, num_jugadas)


resultados_jugador$Saldo_final_de_la_jugada <- as.numeric(resultados_jugador$Saldo_final_de_la_jugada)


if (any(is.na(resultados_jugador$Saldo_final_de_la_jugada))) {
  cat("Error: Hay valores no numéricos en la columna Saldo_final_de_la_jugada.\n")
} else {
  ganancia_perdida_total <- sum(resultados_jugador$Saldo_final_de_la_jugada) - saldo_inicial
  cat("\nGanancia o pérdida total:", ganancia_perdida_total, "soles\n")
}



print(resultados_jugador)


saldo_final <- resultados_jugador$Saldo_final_de_la_jugada[100]


ganancia <- saldo_final - saldo_inicial

if (ganancia < 0) {
  cat("Has perdido", abs(ganancia), "soles.\n")
} else if (ganancia > 0) {
  cat("Has ganado", abs(ganancia), "soles.\n")
} else {
  cat("No has ganado ni perdido dinero.\n")
}

