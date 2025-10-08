# ===============================
# ANALISIS DE CORRELACION - R SCRIPT
# ===============================

# -------------------------------
# EJERCICIO 1: Efímeras y velocidad del arroyo
# -------------------------------

# Datos del cuadro 1
speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)

# Scatter plot
plot(speed, abundance, 
     main="Relación entre velocidad del arroyo y abundancia de efímeras",
     xlab="Velocidad del arroyo", ylab="Abundancia de efímeras",
     pch=19, col="blue")
abline(lm(abundance ~ speed), col = "red", lwd = 2)


# Correlación de Pearson
cor_test1 <- cor.test(speed, abundance, method="pearson")
print(cor_test1)

# Resultados
cat("\nEjercicio 1 - Correlación de Pearson\n")
cat("r =", cor_test1$estimate, "\n")
cat("p-valor =", cor_test1$p.value, "\n")
cat("Grados de libertad =", cor_test1$parameter, "\n")


# -------------------------------
# EJERCICIO 2: Suelo (pH vs otras variables)
# -------------------------------

# Datos del cuadro 2
datos <- data.frame(
  Gp = c("T0","T0","T0","T0","T1","T1","T1","T1"),
  pH = c(5.40, 5.65, 5.14, 5.14, 5.14, 5.10, 5.10, 4.70),
  N = c(0.188,0.165,0.260,0.169,0.164,0.164,0.091,0.100),
  Dens = c(0.92,1.04,0.95,1.10,1.12,1.22,1.22,1.52),
  P = c(215,125,300,225,174,129,162,117),
  Ca = c(16.35,12.55,13.02,15.22,14.17,8.55,8.55,8.74),
  Mg = c(7.65,5.15,5.68,7.88,8.12,6.92,6.92,8.16),
  K = c(0.72,0.71,0.68,1.01,0.70,0.81,2.67,0.39),
  Na = c(1.14,0.94,0.60,1.27,0.90,2.17,3.18,3.32),
  Condu = c(1.09,1.35,1.41,1.64,1.85,1.64,3.18,4.16)
)

# Seleccionamos solo variables numéricas
datos_num <- datos[, c("pH","N","Dens","P","Ca","Mg","K","Na","Condu")]

# Correlación de pH con otras variables
resultados <- data.frame(
  Conjunto = character(),
  r = numeric(),
  p_valor = numeric()
)

for (var in names(datos_num)[-1]) {
  test <- cor.test(datos_num$pH, datos_num[[var]], method="pearson")
  resultados <- rbind(resultados, 
                      data.frame(Conjunto = paste("pH -", var),
                                 r = test$estimate,
                                 p_valor = test$p.value))
}

print(resultados)

# -------------------------------
# Opcional: Mapa de correlaciones entre todas las variables
# -------------------------------
# install.packages("corrplot") # ejecutar si no lo tienes
library(corrplot)

M <- cor(datos_num)
corrplot(M, method="circle", type="upper", 
         tl.col="black", tl.cex=0.8,
         title="Matriz de correlaciones del suelo")


# ====================================================
# EJERCICIO 3: El cuarteto de Anscombe
# ====================================================

# Usamos dataset oficial incluido en R
data(anscombe)

# ====================================================
# Gráficas de los cuatro conjuntos
# ====================================================
par(mfrow=c(2,2))  # Ventana gráfica en 2 filas y 2 columnas

for (i in 1:4) {
  plot(anscombe[,i], anscombe[,i+4],
       main=paste("Conjunto", i),
       xlab="x", ylab="y",
       pch=19, col="orange")
  abline(lm(anscombe[,i+4] ~ anscombe[,i]), col="blue", lwd=2)
}
par(mfrow=c(1,1)) # Restablecer formato original de gráficos

# ====================================================
# Estadísticas descriptivas del Cuarteto
# ====================================================
cat("\nResumen del Cuarteto de Anscombe:\n")

for (i in 1:4) {
  x <- anscombe[,i]
  y <- anscombe[,i+4]
  cat(paste("\nConjunto", i, ":\n"))
  cat("Media de x:", mean(x), "\n")
  cat("Media de y:", mean(y), "\n")
  cat("Varianza de x:", var(x), "\n")
  cat("Varianza de y:", var(y), "\n")
  cat("Correlación:", cor(x, y), "\n")
  modelo <- lm(y ~ x)
  cat("Regresión lineal: y =", round(coef(modelo)[1],3), "+", round(coef(modelo)[2],3), "* x\n")
  cat("R²:", round(summary(modelo)$r.squared,3), "\n")
}

