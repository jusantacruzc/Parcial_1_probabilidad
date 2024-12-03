# Cargar librerías necesarias
library(ggplot2)  # Para hacer gráficos más bonitos
library(dplyr)    # Para manipulación de datos

# Establecer la semilla para garantizar reproducibilidad
set.seed(0289)

# 1. Cargar la base de datos
# Cambia la ruta si la base de datos está en otra ubicación
ruta <- "C:/Users/Nicol/OneDrive/Escritorio/Documentos/University/UNAL/Semestre5/PyE/Base_Estadistica_Descriptiva.csv"
datos <- read.csv(ruta, header = TRUE, sep = ",")
# Visualizar las primeras filas para verificar que los datos se cargaron correctamente
head(datos)
# Revisar los nombres de las columnas
print(names(datos))  # Mostrar nombres exactos de las columnas


# 11. Construir un diagrama de caja y bigotes para IngresoMensual por NivelEducativo
boxplot_ingreso <- ggplot(datos, aes(x = NivelEducativo, y = IngresoMensual)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Diagrama de Caja y Bigotes: Ingreso Mensual por Nivel Educativo",
       x = "Educativo", y = "Ingreso Mensual") +
  theme_minimal()
print(boxplot_ingreso)

# 12. Graficar la relación entre Edad y HorasDeTrabajo con un gráfico de dispersión
scatter_plot <- ggplot(datos, aes(x = Edad, y = HorasDeTrabajo)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Edad y Horas de Trabajo",
       x = "Edad", y = "Horas de Trabajo") +
  theme_minimal()
print(scatter_plot)

# Reflexión sobre tendencia aparente
cat("Reflexión sobre tendencia aparente: \n")
cat("(Recordar insertar imagen antes xD) Como podemos ver en la imagen, la mayoria de puntos del grafico de dispersion se encuentran al rededor del numero 40 lo que indica que la mayoria de trabajadores trabajan al rededor de 40 horas independientemente de su edad\n")

# 13. Construir una tabla de doble entrada para la distribución conjunta de Género y NivelEducativo
tabla_conjunta <- table(datos$Genero, datos$NivelEducativo)
print(tabla_conjunta)

# 14. Calcular la proporción de personas con más de 2 hermanos y un ingreso mensual superior a $3000
# Filtrar personas con más de 2 hermanos e ingreso mensual > 3000
filtro <- filter(datos, Hermanos > 2 & IngresoMensual > 3000)

# Proporción sobre el total de personas con más de 2 hermanos
proporcion <- nrow(filtro) / nrow(datos %>% filter(Hermanos > 2))
cat("Proporción de personas con más de 2 hermanos e ingreso mensual > $3000: ", proporcion, "\n")

# 15. Reflexión sobre variables relacionadas con el ingreso mensual
correlaciones <- datos %>% 
  select(Edad, Hermanos, Estatura, HorasDeTrabajo, IngresoMensual) %>%
  cor()  # Matriz de correlaciones
print(correlaciones)

# 15. ¿Qué variables parecen tener mayor relación con el ingreso mensual?

# Según los resultados obtenidos, el Nivel Educativo parece ser la variable
# que más influye en el ingreso mensual. En general, las personas con niveles
# educativos más altos (como universitarios) tienden a tener mejores oportunidades
# laborales y, por ende, mayores ingresos. 
# También analizamos la proporción de personas con más de 2 hermanos e ingresos
# mayores a $3000 (aproximadamente 9.33%). Esto sugiere que el número de hermanos
# no está muy relacionado con altos ingresos. Sin embargo, podrían influir 
# otras variables, como Edad o Horas de Trabajo, que deberíamos revisar más a fondo.

cat("El Nivel Educativo parece tener una relación significativa con el ingreso mensual, \n")
cat("ya que las personas con niveles educativos más altos suelen acceder a mejores empleos.\n")

# 16. ¿Qué variables elegiría para predecir las Horas de Trabajo?

# Para predecir las Horas de Trabajo, elegiría como variables principales:
# - Edad: Es probable que influya porque las personas jóvenes o mayores
#   pueden trabajar menos horas, mientras que las personas de edad media
#   trabajan más.
# - Nivel Educativo: Dependiendo del nivel de formación, puede haber diferencias
#   en la cantidad de horas trabajadas. Por ejemplo, empleos técnicos pueden requerir
#   más horas que empleos administrativos.
# - Ingreso Mensual: Aunque menos directo, podría ser útil porque refleja
#   el tipo de empleo, y algunos empleos bien remunerados requieren más horas.

cat("Si quisiera predecir las Horas de Trabajo, elegiría Edad, Nivel Educativo e Ingreso Mensual. \n")
cat("Esto se debe a que estas variables pueden estar relacionadas con la cantidad de horas \n")
cat("que una persona trabaja, según su etapa de vida, formación y tipo de empleo.\n")

