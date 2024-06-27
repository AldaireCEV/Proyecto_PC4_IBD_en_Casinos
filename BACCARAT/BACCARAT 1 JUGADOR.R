# Definimos la función para empezar la simulación del Baccarat
jugar_baccarat <- function(apuesta, saldo_inicial, tipo_apuesta) {
# En el Baccarat se usan 8 barajas, por ende repetimos más veces los números
# Cartas del 1 al 9 valen su número, 10 y figuras valen 0 o 10, no importa en el conteo
  baraja <- c(rep(1:9, times = 32), rep(10, times = 128))  # Cartas del 1 al 9 valen su número, 10 y figuras valen 10
  baraja <- sample(baraja)  # Barajar las cartas
  
# Repartirmos las cartas al jugador y al crupier
  jugador <- c(baraja[1], baraja[2])
  crupier <- c(baraja[3], baraja[4])
  
# Calculamos el valor de la mano que ha recibido cada uno
  valor_mano <- function(mano) {
    valor <- sum(mano)
    if (valor >= 10) {
      return(valor %% 10)
    } else {
      return(valor)
    }
  }
  
# Calculamos el valor inicial de las manos
  valor_jugador <- valor_mano(jugador)
  valor_crupier <- valor_mano(crupier)
  
# Determinarnamos si el jugador toma una tercera carta
  if (valor_jugador <= 5) {
    jugador <- c(jugador, baraja[5])
    valor_jugador <- valor_mano(jugador)
  }
  
# Determinamos si el crupier toma una tercera carta
  if (valor_crupier <= 2) {
    crupier <- c(crupier, baraja[6])
    valor_crupier <- valor_mano(crupier)
  } else if (valor_crupier == 3 && (length(jugador) < 3 || jugador[3] != 8)) {
    crupier <- c(crupier, baraja[6])
    valor_crupier <- valor_mano(crupier)
  } else if (valor_crupier == 4 && (length(jugador) < 3 || jugador[3] %in% 2:7)) {
    crupier <- c(crupier, baraja[6])
    valor_crupier <- valor_mano(crupier)
  } else if (valor_crupier == 5 && (length(jugador) < 3 || jugador[3] %in% 4:7)) {
    crupier <- c(crupier, baraja[6])
    valor_crupier <- valor_mano(crupier)
  } else if (valor_crupier == 6 && (length(jugador) < 3 || jugador[3] %in% c(6, 7))) {
    crupier <- c(crupier, baraja[6])
    valor_crupier <- valor_mano(crupier)
  }
  
  # Determinamos a qué se apostó y si se ganó o perdió
  if (valor_jugador > valor_crupier) {
    if (tipo_apuesta == "banca") {
      ganancia <- -apuesta
      resultado <- "Perdio"
    } else {
      ganancia <- apuesta
      resultado <- "Gano"
    }
  } else if (valor_crupier > valor_jugador) {
    if (tipo_apuesta == "banca") {
      ganancia <- apuesta * (1 - 0.05)  # Ganancia con la comisión del 5%
      resultado <- "Gano"
    } else {
      ganancia <- -apuesta
      resultado <- "Perdio"
    }
  } else {
    ganancia <- 0
    resultado <- "Empato"
  }
  
  # Devolvemos los datos necesarios
  return(list(resultado = resultado, ganancia = ganancia, balance_final = saldo_inicial + ganancia))
}

# Creamos una funcion para simular la cantidad de juegos pedida
simular_juegos <- function(num_juegos, saldo_inicial) {
  resultados <- data.frame(
    Jugada = 1:num_juegos,
    Apuesta = rep(10, num_juegos),  # Apuesta fija de 10 unidades
    Tipo_Apuesta = character(num_juegos),
    Resultado = character(num_juegos),
    Ganancia = numeric(num_juegos),
    Saldo = numeric(num_juegos),
    stringsAsFactors = FALSE
  )
  
# Realizar las apuestas y registrar resultados
  for (i in 1:num_juegos) {
    tipo_apuesta <- sample(c("banca", "jugador"), 1)  # Apuesta aleatoria a banca o jugador
    juego <- jugar_baccarat(resultados$Apuesta[i], saldo_inicial, tipo_apuesta)
    
    resultados$Tipo_Apuesta[i] <- tipo_apuesta
    resultados$Resultado[i] <- juego$resultado
    resultados$Ganancia[i] <- juego$ganancia
    resultados$Saldo[i] <- juego$balance_final
    
# Actualizamos el saldo para el siguiente juego
    saldo_inicial <- juego$balance_final
  }
  
  return(resultados)
}
# Simulamos 100 juegos
resultados <- simular_juegos(num_juegos = 100, saldo_inicial = 1000)
print(resultados)
