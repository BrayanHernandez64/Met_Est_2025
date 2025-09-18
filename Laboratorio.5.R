# Brayan Daniel Hernandez Quezada
# COSTALES
# PROBLEMA 80kg
# 10/sep/2025

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

mean(costal)
var(costal)
sd(costal)
boxplot(costal)
# falta de control de calidaad

# Prueba T
t.test(costal, mu=80)

#H0 = es igual a 80
#H1 = no es igual a 80

#Produccion de semillas
sem <- read.csv("Datos_arboles.csv", header = T)
sem$Tiempo <- as.factor(sem$Tiempo)

tapply(sem$Kgsem, sem$Tiempo, mean)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col = "blue",
        xlab = "Año",
        ylab = "Semilla (kg)")

t2012 <- subset(sem, sem$Tiempo =="T2012")
t2013 <- subset(sem, sem$Tiempo !="T2012")
t.test(t2012$Kgsem, t2013$Kgsem, paored = T, alternative = "less")

