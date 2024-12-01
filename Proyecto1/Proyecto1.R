# Cargar datos
datos <- read.csv("C:/Users/do0cu/OneDrive/Documentos/Universidad/Probabilidad/Proyecto1/Base_Estadistica_Descriptiva.csv", header = TRUE, sep = ",")

# Semilla
set.seed(890250)
datos

## 1. Construir tablas de frecuencia y grafico de barras
Nivel_Educativo <- datos$NivelEducativo; 

frecuencias_nivel_educativo <- table(Nivel_Educativo)

# Grafico de barras
barplot(frecuencias_nivel_educativo,
        main = "Gráfico de Barras",
        xlab = "Categorías",
        ylab = "Frecuencia",
        col = "skyblue",
        border = "black")


## 2. Porcentaje de generos y grafico pastel 
# Contar las frecuencias de cada categoría en la columna Genero
frecuencia_genero <- table(datos$Genero)

# Crear el gráfico de pastel usando la tabla_genero
pie(frecuencia_genero, 
    labels = paste(names(frecuencia_genero), "\n", round(100 * frecuencia_genero / sum(frecuencia_genero), 2), "%"), 
    main = "Gráfico de Pastel (Base R)", 
    col = rainbow(length(frecuencia_genero)))


# Asegúrate de que la variable Edad está correctamente cargada
edad <- datos$Edad

# Calcular la media
media_edad <- mean(edad) # na.rm = TRUE para ignorar valores NA

# Calcular la mediana
mediana_edad <- median(edad)

# Calcular la desviación estándar
desviacion_edad <- sd(edad)

# Imprimir resultados
cat("Media de Edad:", media_edad, "\n")
cat("Mediana de Edad:", mediana_edad, "\n")
cat("Desviación estándar de Edad:", desviacion_edad, "\n")

# Crear una nueva variable categórica según el número de hermanos
Grupo_Hermanos <- ifelse(datos$Hermanos > 3, "Más de 3 hermanos", "3 o menos hermanos")

# Contar las frecuencias de cada grupo
tabla_hermanos <- table(Grupo_Hermanos)

# Encontrar el valor máximo para ajustar el eje Y
valor_max <- max(tabla_hermanos)

# Crear el gráfico de barras
barplot(tabla_hermanos,
        main = "Distribución por número de hermanos",
        xlab = "Grupo",
        ylab = "Cantidad de personas",
        col = c("skyblue", "orange"),
        border = "black",
        ylim = c(0, valor_max + 100))  # Agregar un margen al eje Y

# Rango
rango <- range(datos$IngresoMensual)
rango_num <- diff(rango)

# Varianza
varianza <- var(datos$IngresoMensual)

# Coeficiente de Variación
desviacion_estandar <- sd(datos$IngresoMensual)
media <- mean(datos$IngresoMensual)
coef_variacion <- (desviacion_estandar / media) * 100

# Resultados
rango
rango_num
varianza
coef_variacion


