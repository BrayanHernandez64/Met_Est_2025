# Asignacion 3
# Brayan Daniel Hernandez Quezada
# IF
# 4/sept/2025

# ANÁLISIS DE Petal.Length EN IRIS
# Comparando Versicolor y Virginica

# Cargo la base de datos que ya tiene R
data(iris)

# 1. Analisis en la variable Petal.Length
# ver las primeras filas
head(iris$Petal.Length)
# resumen rapido 
summary(iris$Petal.Length)

# Esta funcion resume por especie (versicolor y virginica)
aggregate(Petal.Length ~ Species, data = iris[iris$Species %in% c("versicolor","virginica"), ], summary)

# Preparación de los datos

iris_sub <- subset(iris, Species %in% c("versicolor", "virginica"))
versicolor <- iris_sub$Petal.Length[iris_sub$Species == "versicolor"]
virginica  <- iris_sub$Petal.Length[iris_sub$Species == "virginica"]


# Normalidad (calculada con la funcion shapiro)
shapiro.test(versicolor)
shapiro.test(virginica)

# Prueba t de Student
t.test(versicolor, virginica,
       alternative = "two.sided",
       var.equal = FALSE)

# Tamaño del efecto (Cohen's d)

if(!require(effsize)) install.packages("effsize")
library(effsize)


# Visualización de grafica
# Boxplot comparativo
boxplot(Petal.Length ~ Species, data = iris_sub,
        main = "Comparación de Petal.Length",
        xlab = "Especie", ylab = "Petal.Length",
        col = c("lightblue", "lightgreen"))






