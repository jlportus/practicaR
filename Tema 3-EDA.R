library(ggplot2)

################################################################################
# 1 - Definición del problema: 
# El objetivo del análisis exploratorio de datos radica en investigar 
# patrones en la aparición de diabetes, enfocándose en dos aspectos específicos:
# la cantidad de embarazos, y el impacto del nivel glucémico. Tiempo y espacio.
################################################################################
# 2 - Justificación: Un párrafo en el que demostremos la importancia de estudiar
# la diabetes, citando uno o dos artículos científicos que expliquen el porqué.
# 
# La diabetes es una de las enfermedades crónicas de mayor prevalencia a nivel mundial, afectando a 
# más de 530 millones de personas en 2021, según el Atlas de la Federación Internacional de Diabetes (Sun et al., 2021). 
# Esta enfermedad no solo impacta significativamente la calidad de vida de quienes la padecen, sino que también 
# representa un desafío económico para los sistemas de salud debido a sus complicaciones, como enfermedades cardiovasculares, 
# insuficiencia renal y amputaciones. Estudios como el de Saeedi et al. (2019) destacan que un diagnóstico temprano y una 
# adecuada gestión de los factores de riesgo pueden reducir la incidencia y la carga asociada a esta condición. 
# Por lo tanto, el análisis de factores relacionados, como el número de embarazos y los niveles de glucosa, 
# puede proporcionar información crucial para la prevención y el manejo de la diabetes.
################################################################################
# 3 - Descripción de la base de datos
################################################################################
# Cargar el conjunto de datos: https://www.kaggle.com/datasets/akshaydattatraykhare/diabetes-dataset/data
wd <- getwd()
ruta_datos <- file.path(wd, "R", "TestsClase", "data", "diabetes.csv")
diabetes <- read.csv(ruta_datos)


# Exploración inicial de los datos 

# Pregnancies: To express the Number of pregnancies
# Glucose: To express the Glucose level in blood
# BloodPressure: To express the Blood pressure measurement
# SkinThickness: To express the thickness of the skin
# Insulin: To express the Insulin level in blood
# BMI: To express the Body mass index
# DiabetesPedigreeFunction: To express the Diabetes percentage
# Age: To express the age
# Outcome: To express the final result 1 is Yes and 0 is No

# Verificar datos faltantes o valores atípicos antes del análisis.
head(diabetes)
str(diabetes)
summary(diabetes)
sum(is.na(diabetes))  # ¿Hay valores NA?
print(table(diabetes$Outcome)) # Conteo de casos de diabetes (1) y no diabetes (0)

################################################################################
# 4 - En este momento nos hacemos las preguntas de investigación.
################################################################################
# 4.1 - Estudio sobre cómo influye estar o no embarazada en la diabetes
# ¿Existe una relación entre el número de embarazos y la probabilidad de desarrollar diabetes? 
# ¿Se puede identificar un patrón en los datos que indique una asociación entre estas dos variables?

# Gráfico de barras apiladas
contingency_table <- table(diabetes$Pregnancies, diabetes$Outcome)
print(contingency_table)
transposed_table <- t(contingency_table)
print(transposed_table)

# Crear el gráfico de barras apiladas
barplot(transposed_table, 
        beside = TRUE, 
        legend.text = c("No diabético", "Diabético"),
        main = "Número de Embarazos por Resultado de Diabetes",
        xlab = "Número embarazos",
        ylab = "Frecuencia",
        col = c("lightblue", "lightgreen"),
        ylim = c(0, max(contingency_table) + 10))


# Crear el boxplot embarazos
boxplot(Pregnancies ~ Outcome, data = diabetes,
        main = "Número de Embarazos por Resultado de Diabetes",
        xlab = "Diabetes",
        ylab = "Número de Embarazos",
        col = c("lightblue", "lightgreen"))


# Calcular el Odds Ratio (OR) para embarazos >= 3 como umbral mostrado en los gráficos
umbral_embarazos <- 3

# Calcular el número de personas con embarazos >= umbral y < umbral en ambos grupos
diabetes_embarazos_altos <- sum(diabetes$Pregnancies[diabetes$Outcome == 1] >= umbral_embarazos)
diabetes_embarazos_bajos <- sum(diabetes$Pregnancies[diabetes$Outcome == 1] < umbral_embarazos)
no_diabetes_embarazos_altos <- sum(diabetes$Pregnancies[diabetes$Outcome == 0] >= umbral_embarazos)
no_diabetes_embarazos_bajos <- sum(diabetes$Pregnancies[diabetes$Outcome == 0] < umbral_embarazos)

# Crear la tabla de contingencia para los cálculos
tabla_contingencia_embarazos <- matrix(c(diabetes_embarazos_altos, no_diabetes_embarazos_altos, 
                                         diabetes_embarazos_bajos, no_diabetes_embarazos_bajos), 
                                       nrow = 2, byrow = TRUE,
                                       dimnames = list(c("Embarazos Altos", "Embarazos Bajos"),
                                                       c("Diabetes", "No Diabetes")))
# Calcular el Odds Ratio
# El Odds Ratio (OR) es una medida de la fuerza de asociación entre dos variables categóricas. En este caso, estamos evaluando la relación entre el número de embarazos (con un umbral de 3 embarazos) y la probabilidad de desarrollar diabetes.
# El OR se calcula comparando las probabilidades (odds) de que ocurra el evento (diabetes) en dos grupos: aquellos con embarazos altos (>= 3) y aquellos con embarazos bajos (< 3).
odds_embarazos_alto <- diabetes_embarazos_altos/no_diabetes_embarazos_altos
odds_embarazos_bajo <- diabetes_embarazos_bajos / no_diabetes_embarazos_bajos
odds_ratio_embarazos <- odds_embarazos_alto / odds_embarazos_bajo

# El cálculo anterior divide la "odds" de diabetes en el grupo de embarazos altos entre las "odds" de diabetes en el grupo de embarazos bajos.
# Las odds son simplemente la probabilidad de que ocurra un evento (en este caso, desarrollar diabetes) dividida entre la probabilidad de que no ocurra.

# Por ejemplo, si en el grupo con embarazos altos (>= 3), de 100 personas, 60 tienen diabetes, las "odds" de diabetes serían 60/40 = 1.5.
# Esto significa que por cada mujer con muchos embarazos que no tiene diabetes hay 1.5 mujeres que sí.
# Si en el grupo con embarazos bajos (< 3), de 100 personas, 30 tienen diabetes, las "odds" serían 30/70 = 0.43.
# Esto significa que por cada mujer con pocos embarazos que no tiene diabetes hay 0,43 que sí la tienen.
# El Odds Ratio sería 1.5 / 0.43 = 3.49, lo que indica que las personas con más embarazos tienen 3.49 veces más probabilidades de desarrollar diabetes.


# Calcular el incremento porcentual
# El incremento porcentual en el riesgo es una forma de interpretar el Odds Ratio de manera más intuitiva. Se calcula como:
# (OR - 1) * 100. Si el OR es mayor que 1, significa que hay un aumento en las probabilidades de desarrollar diabetes.
# Un OR de 3.49, por ejemplo, implica que el riesgo de desarrollar diabetes se ha incrementado en un 249% (3.49 - 1 = 2.49; 2.49 * 100 = 249%).
# Este porcentaje ayuda a visualizar el aumento en el riesgo de manera más tangible, ya que un incremento del 249% es más fácil de entender que un OR de 3.49.

incremento_porcentual <- (odds_ratio_embarazos - 1) * 100

# El cálculo del incremento porcentual nos da una idea de cuánto aumenta el riesgo en términos absolutos.
# Si el Odds Ratio es 1, no hay cambio en el riesgo. Si es mayor que 1, el riesgo aumenta, y si es menor que 1, el riesgo disminuye.
# Este incremento porcentual es muy útil en la interpretación de los resultados de estudios epidemiológicos o de salud.

# Mostrar los resultados
# La función 'cat()' se usa para imprimir texto y valores calculados de manera más limpia y comprensible.
# La tabla de contingencia muestra cuántos casos de diabetes y no diabetes hay para cada categoría de número de embarazos.
cat("Tabla de Contingencia para Embarazos:\n")
print(tabla_contingencia_embarazos)

# Imprimir el Odds Ratio calculado y el incremento porcentual
cat("\nOdds Ratio (OR) para Embarazos:", odds_ratio_embarazos, "\n")
cat("\nIncremento porcentual en el riesgo de diabetes por cada embarazo adicional:", incremento_porcentual, "%\n")


################################################################################
# 4.2 - Estudio sobre el impacto del nivel de glucosa en sangre en el diagnóstico de diabetes:
# ¿Cómo varía el nivel de glucosa en sangre entre las personas diagnosticadas con diabetes y aquellas que no lo están? 
# ¿Existen umbrales específicos de glucosa que puedan predecir el riesgo de diabetes?

# Crear boxplot Nivel de glucosa en sangre
boxplot(Glucose ~ Outcome, 
        data = diabetes, 
        main = "Distribución del Nivel de Glucosa en Sangre por Diagnóstico de Diabetes",
        xlab = "Diagnóstico de Diabetes",
        ylab = "Nivel de Glucosa en Sangre",
        col = c("lightblue", "lightgreen"),
        notch = TRUE)  # Agregar muescas para comparar medianas


# Gráfico de densidad
ggplot(diabetes, aes(x = Glucose, color = factor(Outcome), fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +  # Añadir transparencia para visualizar superposiciones
  labs(title = "Distribución del Nivel de Glucosa en Sangre por Diagnóstico de Diabetes",
       x = "Nivel de Glucosa en Sangre",
       y = "Densidad") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("No diabético", "Diabético")) +
  scale_color_manual(values = c("lightblue", "lightgreen"), 
                     labels = c("No diabético", "Diabético"))



# Subset de datos para personas diabéticas y no diabéticas
glucosa_diabetes <- diabetes$Glucose[diabetes$Outcome == 1]
glucosa_no_diabetes <- diabetes$Glucose[diabetes$Outcome == 0]
summary(glucosa_diabetes)
summary(glucosa_no_diabetes)
# Crear histogramas comparativos
par(mfrow=c(1,2))  # Organizar los histogramas en una fila

hist(glucosa_diabetes, 
     main = "Personas Diabéticas",
     xlab = "Nivel de Glucosa en Sangre",
     ylab = "Frecuencia",
     col = "lightgreen",
     ylim = c(0, 100),  # Ajustar el límite y para comparación
     breaks = seq(0, 200, by = 10))  # Ajustar los intervalos para comparación

hist(glucosa_no_diabetes, 
     main = "Personas No Diabéticas",
     xlab = "Nivel de Glucosa en Sangre",
     ylab = "Frecuencia",
     col = "lightblue",
     ylim = c(0, 100),  # Ajustar el límite y para comparación
     breaks = seq(0, 200, by = 10))  # Ajustar los intervalos para comparación


# Umbral para glucosa alta (por ejemplo, 126 mg/dL)
umbral_glucosa <- 126

# Calcular el número de personas con glucosa alta y baja en ambos grupos
diabetes_glucosa_alta <- sum(glucosa_diabetes >= umbral_glucosa)
diabetes_glucosa_baja <- sum(glucosa_diabetes < umbral_glucosa)
no_diabetes_glucosa_alta <- sum(glucosa_no_diabetes >= umbral_glucosa)
no_diabetes_glucosa_baja <- sum(glucosa_no_diabetes < umbral_glucosa)

# Crear la tabla de contingencia
tabla_contingencia_glucosa <- matrix(c(diabetes_glucosa_alta, diabetes_glucosa_baja, 
                                       no_diabetes_glucosa_alta, no_diabetes_glucosa_baja), 
                                     nrow = 2, byrow = TRUE,
                                     dimnames = list(c("Glucosa Alta", "Glucosa Baja"),
                                                     c("Diabetes", "No Diabetes")))

# Calcular las odds de diabetes para el grupo con glucosa alta
odds_glucosa_alta <- diabetes_glucosa_alta / no_diabetes_glucosa_alta

# Calcular las odds de diabetes para el grupo con glucosa baja
odds_glucosa_baja <- diabetes_glucosa_baja / no_diabetes_glucosa_baja

# Calcular el Odds Ratio (OR) para glucosa alta vs baja
odds_ratio_glucosa <- odds_glucosa_alta / odds_glucosa_baja

# Calcular el incremento porcentual
incremento_porcentual_glucosa <- (odds_ratio_glucosa - 1) * 100

# Mostrar los resultados
cat("Tabla de Contingencia para Glucosa en Sangre:\n")
print(tabla_contingencia_glucosa)

cat("\nOdds Ratio (OR) para diabetes y glucosa alta vs baja:", odds_ratio_glucosa, "\n")
cat("\nIncremento porcentual en el riesgo de diabetes por cada aumento en los niveles de glucosa:", incremento_porcentual_glucosa, "%\n")


# Calcular la mediana de la muestra de personas sanas:
# Si un nivel por encima de la mediana de los no diabéticos muestra un aumento 
# en el riesgo, el mensaje es más intuitivo: valores superiores a los "normales" 
# implican mayor riesgo.
mediana_glucosa_no_diabetes <- median(glucosa_no_diabetes)
cat("Mediana de glucosa en personas sin diabetes:", mediana_glucosa_no_diabetes, "\n")

# Categorizar según la mediana del grupo sin diabetes
diabetes$Glucose_Category <- ifelse(diabetes$Glucose >= mediana_glucosa_no_diabetes, "Alta", "Baja")

# Crear tabla de contingencia
tabla_contingencia_medianas <- table(diabetes$Glucose_Category, diabetes$Outcome)
print(tabla_contingencia_medianas)

# Calcular Odds Ratio
odds_glucosa_alta <- tabla_contingencia_medianas["Alta", "1"] / tabla_contingencia_medianas["Alta", "0"]
odds_glucosa_baja <- tabla_contingencia_medianas["Baja", "1"] / tabla_contingencia_medianas["Baja", "0"]
odds_ratio_glucosa_medianas <- odds_glucosa_alta / odds_glucosa_baja

cat("\nOdds Ratio (OR) basado en la mediana de glucosa:", odds_ratio_glucosa_medianas, "\n")

################################################################################
# 5 - Conclusiones:
# 5.1 - Nuestros análisis indican una relación entre el número de 
# embarazos y la incidencia de diabetes en mujeres. El Odds Ratio de 2.35 sugiere 
# que las mujeres que han tenido tres o más embarazos tienen un 135% más 
# probabilidades de contraer diabetes, lo que significa más del doble de 
# probabilidades de desarrollar diabetes en comparación con aquellas con menos 
# de tres embarazos. Este hallazgo resalta que el número de embarazos es un factor 
# de riesgo importante a considerar en la prevención y manejo de la diabetes en 
# mujeres. El estudio muestra que, con cada embarazo adicional a partir del tercero, 
# el riesgo de desarrollar diabetes aumenta. Por lo tanto, las mujeres con 
# múltiples embarazos deben ser monitoreadas más de cerca para la detección 
# temprana y la prevención de la diabetes. 

# 5.2 - Los datos sugieren que existe una fuerte relación entre los niveles de 
# glucosa en sangre y la presencia de diabetes. La mediana de los niveles de 
# glucosa en personas con diabetes es de 140 mg/dL, mientras que en personas sin 
# diabetes es de 107 mg/dL. Esto da a entender que las personas con diabetes 
# tienden a tener niveles de glucosa más altos en comparación con aquellas 
# personas sin diabetes. El Odds Ratio de 5.99 indica que las personas con 
# niveles de glucosa en ayunas superiores a 126 mg/dL tienen casi seis veces más 
# probabilidades de desarrollar diabetes en comparación con aquellas con niveles 
# de glucosa normales. Un incremento del 499% respecto al grupo con niveles normales.
# Este resultado subraya la importancia crítica de mantener los niveles de glucosa 
# bajo control para reducir el riesgo de diabetes.

################################################################################
# 6 - Referencias:
# Saeedi, P., Petersohn, I., Salpea, P., Malanda, B., Karuranga, S., Unwin, N., ... 
# & IDF Diabetes Atlas Committee. (2019). Global and regional diabetes prevalence 
# estimates for 2019 and projections for 2030 and 2045: Results from the International 
# Diabetes Federation Diabetes Atlas. Diabetes research and clinical practice, 157, 107843.

# Sun, H., Saeedi, P., Karuranga, S., Pinkepank, M., Ogurtsova, K., Duncan, B. B., ... 
# & Magliano, D. J. (2022). IDF Diabetes Atlas: Global, regional and country-level 
# diabetes prevalence estimates for 2021 and projections for 2045. Diabetes research 
# and clinical practice, 183, 109119.