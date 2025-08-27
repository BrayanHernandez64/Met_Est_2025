# Pruerba de t
# caso de Muestra independiente
# BDHQ
# 27/08/20245

# Importar datos de indice de calidad

calidad <- read.csv("calidad_planta.csv" header = T)

calidad$Tratamiento <- as.factor(calidad$Tratamiento)

colores <- c("blue","yellow")
boxplot(calidad$IE - calidad$Tratamiento,
        col= colores,
        xlab = "Tratamiento",
        ylab = "indice calidadd",
        ylim = c(0.4,1,2),
        main = "vivero Iturbide")

# Estadsistica dscriptivas
# tapply sirve para obtener un valor cuando contamos
# con varios grupos

tapply(calidad$IE, calidad$Tratamiento, mean)
tapply(calidad$IE, calidad$Tratamiento,  var)

# Observamos que la varianza del grupo  fert es 3 veces 
# mas grande que el grupo Control (ctrl)

# revisar el comportamiento de los datos
library("ggplot2")

ggplot(calidad, aes(x = IE, color = Tratamiento))+ geom_density()
ggplot(calidad, aes(x = IE, color = Tratamiento))+ geom_histogram()



