# Examen parcial 2
# Brayan Daniel Hernandez Quezada
# 22/oct/2025


download.file("https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1",
              destfile = "obs.csv")

suelo <- read.csv("obs.csv")

suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)

#P1 agregar summary a las 3 variables de Arcilla 
summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)

#Conclusión (P1):
# A mayor profundidad, aumenta el contenido promedio de arcilla.



# boxplot de clay1
boxplot(suelo$Clay1)
#P2, P3       
#(Si aparecen puntos fuera de los “bigotes”, sí existen outliers.)
boxplot.stats(suelo$Clay1)$out


# calcula la media 
mean(suelo$Clay1)
#P4
#Si el valor p (p-value) es menor que 0.05, hay diferencia significativa con 30%. Si es mayor que 0.05, no hay diferencia significativa.


# caulcula la correlacion
cor(suelo$Clay1, suelo$Clay5)
# prueba significativa 
cor.test(suelo$Clay1, suelo$Clay5)
#P5: Existe una relacion positiva

boxplot(Clay5 ~ zone, data = suelo,
        main = "Contenido de Arcilla (30–50 cm) por Zona",
        xlab = "Zona Agroecológica",
        ylab = "Arcilla (%)",
        col = "lightblue")
#P6,P7
# (Si los boxplots muestran diferentes medianas o amplitudes -> hay variación entre zonas.Si los boxplots son parecidos -> no hay gran diferencia.)
# muestran diferentes medidas 




