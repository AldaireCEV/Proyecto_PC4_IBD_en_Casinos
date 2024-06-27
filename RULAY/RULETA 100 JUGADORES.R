tirada_ruleta <- function(saldo, apuesta, tipo_apuesta) {
  numeros_ruleta <- 0:36
  numero_ganador <- sample(numeros_ruleta, 1)
  resultado <- "pierde"
  
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
  
  return(list(saldo=saldo, resultado=resultado, numero_ganador=numero_ganador))
}

simular_jugador <- function(saldo_inicial, cantidad_apostada, num_tiradas) {
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
  
  saldo <- saldo_inicial
  for (i in 1:num_tiradas) {
    saldo_inicial_jugada <- saldo
    tipo_apuesta <- sample(c("par", "impar"), 1)
    
    resultado <- tirada_ruleta(saldo, cantidad_apostada, tipo_apuesta)
    
    if (resultado$resultado == "gana") {
      saldo <- saldo_inicial_jugada + cantidad_apostada
    } else {
      saldo <- saldo_inicial_jugada - cantidad_apostada
    }
    
    ganancia_perdida <- saldo - saldo_inicial_jugada
    
    resultados[i, ] <- c(i, saldo_inicial_jugada, cantidad_apostada, tipo_apuesta, 
                         resultado$numero_ganador, resultado$resultado, saldo, ganancia_perdida)
  }
  
  return(resultados)
}

num_jugadores <- 100
saldo_inicial <- rep(100, num_jugadores)
cantidad_apostada <- 10
num_tiradas <- 100

set.seed(NULL)  
resultados_globales <- list()
for (j in 1:num_jugadores) {
  resultados_jugador <- simular_jugador(saldo_inicial[j], cantidad_apostada, num_tiradas)
  resultados_globales[[j]] <- resultados_jugador
}

total_casa_apuestas <- 0
for (j in 1:num_jugadores) {
  saldo_final_jugador <- resultados_globales[[j]][num_tiradas, "Saldo_final_de_la_jugada"]
  saldo_inicial_jugador <- saldo_inicial[j]
  
  if (!is.numeric(saldo_final_jugador)) {
    saldo_final_jugador <- as.numeric(saldo_final_jugador)
  }
  
  if (!is.numeric(saldo_inicial_jugador)) {
    saldo_inicial_jugador <- as.numeric(saldo_inicial_jugador)
  }
  
  ganancia_perdida_jugador <- saldo_final_jugador - saldo_inicial_jugador
  total_casa_apuestas <- total_casa_apuestas - ganancia_perdida_jugador
}

tabla_ganancia_perdida <- data.frame(
  Numero_de_jugador = 1:num_jugadores,
  Ganancia_o_Perdida = rep(0, num_jugadores)
)

for (j in 1:num_jugadores) {
  saldo_final_jugador <- resultados_globales[[j]][num_tiradas, "Saldo_final_de_la_jugada"]
  saldo_inicial_jugador <- saldo_inicial[j]
  
  if (!is.numeric(saldo_final_jugador)) {
    saldo_final_jugador <- as.numeric(saldo_final_jugador)
  }
  
  if (!is.numeric(saldo_inicial_jugador)) {
    saldo_inicial_jugador <- as.numeric(saldo_inicial_jugador)
  }
  
  ganancia_perdida_jugador <- saldo_final_jugador - saldo_inicial_jugador
  tabla_ganancia_perdida$Ganancia_o_Perdida[j] <- ganancia_perdida_jugador
}

print(tabla_ganancia_perdida)

ganadores <- 0
perdedores <- 0
for (j in 1:num_jugadores) {
  saldo_final_jugador <- resultados_globales[[j]][num_tiradas, "Saldo_final_de_la_jugada"]
  saldo_inicial_jugador <- saldo_inicial[j]
  
  if (!is.numeric(saldo_final_jugador)) {
    saldo_final_jugador <- as.numeric(saldo_final_jugador)
  }
  
  if (!is.numeric(saldo_inicial_jugador)) {
    saldo_inicial_jugador <- as.numeric(saldo_inicial_jugador)
  }
  
  ganancia_perdida_jugador <- saldo_final_jugador - saldo_inicial_jugador
  
  if (ganancia_perdida_jugador > 0) {
    ganadores <- ganadores + 1
  } else if (ganancia_perdida_jugador < 0) {
    perdedores <- perdedores + 1
  }
}

porcentaje_ganadores <- (ganadores / num_jugadores) * 100
porcentaje_perdedores <- (perdedores / num_jugadores) * 100

cat("Porcentaje de jugadores que ganan dinero:", porcentaje_ganadores, "%\n")
cat("Porcentaje de jugadores que pierden dinero:", porcentaje_perdedores, "%\n")

perdida_total_jugadores <- sum(tabla_ganancia_perdida$Ganancia_o_Perdida)

if (perdida_total_jugadores < 0) {
  cat("La casa de apuestas gana:", perdida_total_jugadores * -1, "soles\n")
} else if (perdida_total_jugadores > 0) {
  cat("La casa de apuestas pierde:", perdida_total_jugadores, "soles\n")
} else {
  cat("La casa de apuestas no gana ni pierde dinero.\n")
}
