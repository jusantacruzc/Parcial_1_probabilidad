# Cargar datos
datos <- read.csv("Base_Estadistica_Descriptiva.csv", header = TRUE, sep = ",")

# Semilla
set.seed(890250)

muestra_datos <- datos[sample(nrow(datos), size = 100), 1:8]
View(muestra_datos)

library(ggplot2)

# 6. Construye un histograma de la variable Estatura. ¿Qué puedes decir sobre su distribución?

numero_barras <- 1 + (3.33 * log10(length(muestra_datos$Estatura)))

estatura_menor <- min(muestra_datos$Estatura)
estatura_mayor <- max(muestra_datos$Estatura)

ancho_barra <- (estatura_mayor - estatura_menor) / numero_barras

graf_hist <- ggplot(data = muestra_datos, aes(x = Estatura, fill = after_stat(count))) +
  geom_histogram(binwidth = ancho_barra, color = "black") +
  labs(title = "Histograma de estatura",
       x = "Estatura (m)",
       y = "Frecuencia") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal()

graf_hist

# 7. Calcula la media y el percentil 90 de la variable HorasDeTrabajo. Interpreta los resultados.

media_horas_trabajo <- mean(muestra_datos$HorasDeTrabajo)
media_horas_trabajo

horas_trabajo_ordenado <- sort(muestra_datos$HorasDeTrabajo)
horas_trabajo_ordenado

percentil_90 <- round((length(horas_trabajo_ordenado) + 1) * 90 / 100)
percentil_90

horas_trabajo_ordenado[percentil_90]

# 8. Compara las alturas promedio (Estatura) por género (Genero). ¿Cuál es la diferencia más destacada?

promedio_estatura_masculino <- mean(muestra_datos[muestra_datos$Genero == "Masculino", 6])
promedio_estatura_masculino

promedio_estatura_femenino <- mean(muestra_datos[muestra_datos$Genero == "Femenino", 6])
promedio_estatura_femenino

df_promedio_estatura <- data.frame(
  Genero = c("Masculino", "Femenino"),
  Promedio = c(promedio_estatura_masculino, promedio_estatura_femenino))

df_promedio_estatura

diag_barra <- ggplot(df_promedio_estatura, aes(x = Genero, y = Promedio)) +
  geom_bar(stat = "identity", fill = 4) +
  geom_text(aes(label = round(Promedio, 2)), vjust = -1, colour = "black") +
  coord_cartesian(ylim = c(1.6, 1.85)) +
  labs(title = "Promedio de Estatura por Género",
       x = "Género",
       y = "Promedio (m)")

diag_barra
