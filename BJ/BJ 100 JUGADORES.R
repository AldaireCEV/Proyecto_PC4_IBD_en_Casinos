# Creamos una funcion para obtener el valor de una carta
valor_carta <- function(carta) {
  if (carta %in% c("J", "Q", "K")) {
    return(10)
  } else if (carta == "A") {
    return(11) # El valor del As lo ajustaremos(1 o 11) más adelante
  } else {
    return(as.numeric(carta))
  }
}

# Creamos una funcion para ajustar el valor del As(1 o 11) como sea conveniente
ajustar_ases <- function(mano) {
  total <- sum(mano)
  ases <- sum(mano == 11)
  while (total > 21 && ases > 0) {
    total <- total - 10
    ases <- ases - 1
  }
  return(total)
}

# Creamos baraja (As, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K)
crear_baraja <- function() {
  baraja <- c(rep(2:10, 4), rep(c("J", "Q", "K"), 4), rep("A", 4))
  return(sample(baraja))
}

# Creamos una funcion para jugar al "Blackjack"
jugar_blackjack <- function(apuesta, saldo) {
  baraja <- crear_baraja()
  
  # Repartimos 2 cartas, tanto al jugador como al crupier
  mano_jugador <- c(baraja[1], baraja[3])
  mano_crupier <- c(baraja[2], baraja[4])
  baraja <- baraja[-c(1:4)]
  
  # Convertimos las 2 cartas a valores numericos
  mano_jugador_valores <- sapply(mano_jugador, valor_carta)
  mano_crupier_valores <- sapply(mano_crupier, valor_carta)
  
  # Ajustamos el valor inicial de las manos del jugador y crupier(en caso haya As)
  total_jugador <- ajustar_ases(mano_jugador_valores)
  total_crupier <- ajustar_ases(mano_crupier_valores)
  
  # Realizamos el turno del jugador
  while (total_jugador < 21 && total_jugador < 17) {
    nueva_carta <- baraja[1]
    baraja <- baraja[-1]
    mano_jugador <- c(mano_jugador, nueva_carta)
    mano_jugador_valores <- c(mano_jugador_valores, valor_carta(nueva_carta))
    total_jugador <- ajustar_ases(mano_jugador_valores)
  }
  
  # Verficamos si el jugador se paso de 21
  if (total_jugador > 21) {
    return(list(resultado = "pierde", saldo_final = saldo - apuesta))
  }
  
  # Realizamos el turno del crupier luego de saber el resultado del turno del jugador
  while (total_crupier < 17) {
    nueva_carta <- baraja[1]
    baraja <- baraja[-1]
    mano_crupier <- c(mano_crupier, nueva_carta)
    mano_crupier_valores <- c(mano_crupier_valores, valor_carta(nueva_carta))
    total_crupier <- ajustar_ases(mano_crupier_valores)
  }
  
  # Determinamos el resultado del juego (gana, empata o pierde el Jugador)
  if (total_crupier > 21 || total_jugador > total_crupier) {
    return(list(resultado = "gana", saldo_final = saldo + apuesta))
  } else if (total_jugador < total_crupier) {
    return(list(resultado = "pierde", saldo_final = saldo - apuesta))
  } else {
    return(list(resultado = "empate", saldo_final = saldo))
  }
}
# Simulamos 100 juegos para 100 Jugadores (100 juegos cada uno)
num_jugadores <- 100
num_juegos <- 100
saldo_inicial <- 100
apuesta <- 10

resultados <- data.frame(
  "Numero de jugador" = integer(num_jugadores),
  "Ganancia o Perdida" = numeric(num_jugadores),
  stringsAsFactors = FALSE
)

for (j in 1:num_jugadores) {
  saldo <- saldo_inicial
  for (i in 1:num_juegos) {
    resultado <- jugar_blackjack(apuesta, saldo)
    saldo <- resultado$saldo_final
  }
  resultados[j, ] <- c(j, saldo - saldo_inicial)
}

# Calculamos el porcentaje de jugadores que ganan y pierden (respecto a su ganancia)
jugadores_ganan <- sum(resultados$Ganancia.o.Perdida > 0)
jugadores_pierden <- sum(resultados$Ganancia.o.Perdida <= 0)
porcentaje_ganan <- jugadores_ganan / num_jugadores * 100
porcentaje_pierden <- jugadores_pierden / num_jugadores * 100

# Calculamos la ganancia de la casa de apuestas
ganancia_casa <- -sum(resultados$Ganancia.o.Perdida)

# Mostramos el porcentaje de jugadores que ganan o pierden y
# mostramos la ganancia(o perdida) de la casa de apuestas
print(resultados)
cat("Porcentaje de jugadores que ganan:", porcentaje_ganan, "%\n")
cat("Porcentaje de jugadores que pierden:", porcentaje_pierden, "%\n")
cat("Ganancia total de la casa de apuestas:", ganancia_casa, "\n")
