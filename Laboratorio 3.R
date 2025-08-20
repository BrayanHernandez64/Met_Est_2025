# Laboratorio Semana 3
20/08/2025
Brayan Daniel Hernandez Quezada


# Importar datos ----------------------------------------------------------

Temp <- read.csv("temperatura.csv",header= T)
Temp <- read.csv("Data/Medias_temp.csv",header = T)

  
# Ingresar datos de manera manual

edad <- c(18,19,18,18,25,19,18,18,18,17,19,19, 18,17,19,18,19,19)

Alumno <- seq(1,18,1)

info <- data.frame(Alumno, edad)

info$Altura <- c(174,174,170,160,158,155,188,170,175,170,172,170,174,180,158,161,188,164)


# Grafica descriptivas ----------------------------------------------------

boxplot(info$Altura,
        # col sirve para colorear la grafica
        col = "peru",
        # Main sirve para poner un titulo
        main = "clase 3 semestre")

colores = c("navajowhite","yellow","skyblue")
boxplot(datos_meses,col = colores)

# Estadistcas descriptivas ------------------------------------------------


