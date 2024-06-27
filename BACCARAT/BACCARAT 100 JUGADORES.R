# Definimos la función para empezar la simulación del Baccarat
jugar_baccarat <- function(apuesta, saldo_inicial, tipo_apuesta) {
  
  # En el Baccarat se usan 8 barajas, por ende repetimos más veces los números
  # Cartas del 1 al 9 valen su número, 10 y figuras valen 0 o 10, no importa en el conteo
  
  baraja <- c(rep(1:9, times = 32), rep(10, times = 128)) 
  baraja <- sample(baraja)  # Barajamos las cartas
  
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
      ganancia <- apuesta * (1 - 0.05)  # Comisión del 5% de la banca
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

#Creamos una funcion para simular la cantidad de juegos pedida
simular_jugador <- function(num_juegos, saldo_inicial) {
  saldo <- saldo_inicial
  for (i in 1:num_juegos) {
    tipo_apuesta <- sample(c("banca", "jugador"), 1)  # Apuesta aleatoria a banca o jugador
    juego <- jugar_baccarat(10, saldo, tipo_apuesta)  # Apuesta fija de 10 
    
    saldo <- juego$balance_final
  }
  
  # Obtenemos el saldo final
  return(saldo - saldo_inicial)
}

# Simularemos para 100 jugadores
num_jugadores <- 100
resultados_jugadores <- numeric(num_jugadores)

for (j in 1:num_jugadores) {
  resultados_jugadores[j] <- simular_jugador(num_juegos = 100, saldo_inicial = 1000)
}

# Calculamos la ganancia de la casa
ganancia_casa <- -sum(resultados_jugadores)

# Creamos la tabla de resultados
tabla_resultados <- data.frame(
  Jugador = 1:num_jugadores,
  Resultado = resultados_jugadores)

tabla_resultados

cat("\nLa casa de apuestas ganó", ganancia_casa)
