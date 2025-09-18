temperatura <- read.csv("C:/repositorio Brayan/Met_Est_2025/temperatura.csv")
View(temperatura)

head(temperatura) #primeras 6
dim(temperatura) #numero de filas y columnas
names(temperatura) #nombres de las colunas 
str(temperatura) #estructura del objeto

#Resumen estadistico
summary(temperatura)

#Modificar nombre de columna
names(temperatura) <-c("anio","Ene","Feb","Mar","Abr","May",
                       "Jun","Jul","Ago","Sep","Oct","Nov","Dic")


names(temperatura)

#Crear columna Media_anual con temperatura media anual
temperatura$Ene
temperatura$Media_anual <- rowMeans(temperatura[,2:13])
head(temperatura)

#Crear objeto con medias mensuales de temperatura
medias_mensuales <- colMeans(temperatura[,2:13])
medias_mensuales

boxplot(temperatura$Ene,main="Temperatura de enero",ylab="c",col="lightblue")

datos_meses <- temperatura[,2:13]
boxplot(datos_meses,main="temperatura",ylab="c",col="lightgreen",names=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

