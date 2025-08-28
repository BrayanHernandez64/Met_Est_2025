# Pruerba de t
# caso de Muestra independiente
# BDHQ
# 27/08/20245

# Importar datos de indice de calidad

calidad <- read.csv("CalidadPlantas1.csv", header= T)

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
tapply(calidad$IE, calidad$Tratamiento,  sd)


# Observamos que la varianza del grupo  fert es 3 veces 
# mas grande que el grupo Control (ctrl)

# revisar el comportamiento de los datos
library("ggplot2")

ggplot(calidad, aes(x = IE, color = Tratamiento))+ geom_density()
ggplot(calidad, aes(x = IE, color = Tratamiento))+ geom_histogram()

# Separar los datos por tratamiento

df_ctrl <- subset(calidad, Tratamiento == "Ctrl")
df_ctrl <- subset(calidad, Tratamiento != "Ctrl")

#qqnorm revisar normalidad
par(afrow = c(2,2))
qqnorm(df_ctlr$IE); qqline(df_ctlr$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)
par(afrow = c(1,1))

# revisar homogenidad de varianzas
var.test(df_ctlr$IE,df_fest$IE)
var.test(calidad$IE - calidad$Tratamiento)

# Aplicar la prueba de t, variables iguales 
# dos colores =two.sided

t.test(calidad$IE, calidad$Tratamiento,
       var.equal = T,
       alternative = "two.sided")

cohens_efecto <- function(x, y){
  n1 <- length(x); n2 <- sd(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt((n1 - 1) * s1´2 + (n1 - 1) * s´2) / (n1 + n2 -2)) 
  (mean(x) - mean(y)) / sp}

d_cal_efect
