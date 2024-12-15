###############################################################
# Cargar y estudiar el csv
###############################################################

# Establezco mi ruta de trabajo
#setwd("C:/Users/jorge/Documents/Máster BIG DATA/1er cuatrimestre/Análisis e Interpretación de Datos/Actividades/Actividad 1")

# Almaceno mi ruta de trabajo en la variable wd
wd <- getwd()
print(getwd())

# Almaceno la ruta para el archivo csv
ruta_datos <- file.path(wd, "Madrid_Airbnb.csv")

# Cargar csv
datos_csv <- read.csv(ruta_datos)

###############################################################
# Exploración inicial del dataset
###############################################################

# Filas y columnas del dataset original
dim(datos_csv)

# Ver los nombres de las columnas
colnames(datos_csv)

# Ver los datos desde la 1 hasta la 49
View(datos_csv[, 1:49])

# Ver los datos desde la 50 hasta el final
View(datos_csv[, 50:length(datos_csv)])

###############################################################
# Limpieza del dataset
###############################################################

# Limpieza inicial del dataset eliminado columnas no necesarias
datos_csv_limpio <- datos_csv[ , -c(2, 3, 4, 6, 7, 8, 9, 11, 12, 15, 20, 21, 
                                    22, 25, 29, 31, 32, 40, 42, 43, 44, 45, 46,
                                    47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 69)]

# Ver los nombres de las columnas tras la limpieza
colnames(datos_csv_limpio)

# Eliminar registros nulos para aquellos host que  no tengan puntuacion
datos_csv_limpio <- datos_csv_limpio[!is.na(datos_csv_limpio$review_scores_rating), ]

# Comprobamos número de filas y columnas del dataset limpio
dim(datos_csv_limpio)

# Exploración inicial de datos (variables y descripción)

# review_scores_rating: puntuación media del host
# neighbourhood_group_cleansed: zona de Madrid
# host_is_superhost: 't' es superhost, 'f' no lo es. Un superhost su anuncio está mejor posicionado
# instant_bookable: 't' reserva automatica activa, 'f' no activa

# Muestra datos
head(datos_csv_limpio)
str(datos_csv_limpio)

# Resumen estadistico
summary(datos_csv_limpio)

# Contar las frecuencias y ordenar de mayor a menor
tabla_frecuencias <- table(datos_csv_limpio$host_id)
tabla_frecuencias_ordenada <- sort(tabla_frecuencias, decreasing = TRUE)
print(tabla_frecuencias_ordenada)


# Filtro por un valor determinado y muestro las columnas que quiero ver del registro
subset(datos_csv_limpio, host_id == 377605855, select = c("id", "host_name", "neighbourhood_group_cleansed"))

###############################################################
# Preguntas de investigación
###############################################################
# ¿Existe algún patrón?


# Datos para el gráfico barras apiladas
nombre_tabla <- table(datos_csv_limpio$flightNumber, datos_csv_limpio$plane)
print(nombre_tabla)
transposed_table <- t(nombre_tabla)
print(transposed_table)

# Gráfico barras apiladas
barplot(transposed_table,
        beside = TRUE,
        legend.text = c(""),
        main = "Titulo grafico",
        xlab = "x",
        ylab = "y",
        col = c("lightblue", "lightgreen"),
        ylin = c(0, max(nombre_tabla) + 10))

# Gráfico boxplot
boxplot(flighNumber  ~ plane, data = datos_csv,
        main = "titulo",
        xlab = "x",
        ylab = "y",
        col = c("lightblue", "lightgreen"))

# Segregar por categorias, eligiendo y explicando por qúe uso ese umbral
umbral <- 2

# Calculos a partir del umbral elegido
comparativa_si_2_columnas_alto <- sum(datos_csv$flightNumber[datos_csv$plane == 1] >= umbral)
comparativa_si_2_columnas_bajo <- sum(datos_csv$flightNumber[datos_csv$plane == 1] < umbral)
comparativa_no_2_columnas_alto <- sum(datos_csv$flightNumber[datos_csv$plane == 0] >= umbral)
comparativa_no_2_columnas_bajo <- sum(datos_csv$flightNumber[datos_csv$plane == 0] < umbral)


# Tabla de contingencia
tabla_contingencia <- matrix(comparativa_si_2_columnas_alto,
                             comparativa_si_2_columnas_bajo,
                             comparativa_no_2_columnas_alto,
                             comparativa_no_2_columnas_bajo,
                             nrow = 2,
                             byrow = TRUE,
                             dimnames = list(c("1", "2"),
                                             c("a", "b")))

###############################################################
# Probabilidad de que suceda un evento
###############################################################
# Calcular el Odds Ratio
# Mide la fuerza de asociación entre 2 variables, es decir la posibilidad
# de que suceda un evento en una segunda variable en función de la primera
odds_primera <- comparativa_si_2_columnas_alto/comparativa_no_2_columnas_alto
odds_segunda <- comparativa_si_2_columnas_bajo/comparativa_no_2_columnas_bajo
odds_ratio   <- odds_primera / odds_segunda


print("La probabilidad de que suceda según el Odds Ratio es:",odds_ratio)

