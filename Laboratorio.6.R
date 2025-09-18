# Datos de produccion de semillas para los años 20112 y 2013
# ae expresa en kg semillas por arbol

# Importe datos

sem <- read.csv("kilo_semi.csv", header = T)
sem$Tiempo <- as.factor(sem$Tiempo)

tapply(sem$Kgsem, sem$Tiempo,mean)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col = "yellow",
        xlab = "año",
        ylab = "semilla (kg)")

t2012 <- subset(sem, sem$Tiempo=="t2012")
t2013 <- subset(sem, sem$Tiempo !="t2012")
t.test(t2012$Kgsem, t2013$Kgsem, )
