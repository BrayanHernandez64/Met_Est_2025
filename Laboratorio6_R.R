#
# correlacion de Pearson
# Datos de geyser Old Faithful
# 24/09/2025
#

data("faithful")

plot(faithful$waiting, faithful$eruptions,
     xlab = "Tiempo de espera (min)",
     ylab = "Erupcion (min)",
     col = "indianred",
     pch = 20)

# correlacionar las dos variables

shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)

# Pearson solo se utiliza cuando tenemos datos normales 
#
cor.test(faithful$waiting, faithful$eruptions,
         method = "pearson")
# corr 0.9008112 muy alta, significativa


#
# Spearman se utiliza como contraparte para datos no nornales 
#

cor.test(faithful$waiting, faithful$eruptions,
         method = "spearman")

