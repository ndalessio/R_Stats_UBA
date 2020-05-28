library(readxl)
library(xlsx)

# 5 y 6
cuestionario <- read_excel("~/R/Curso_R_estadística/cuestionario.xlsx")

View(cuestionario)

# 7 ¿Cuántos datos tiene el cuestionario?
nrow(cuestionario) # Tiene 717 datos.

# 8 ¿Cuántas variables tiene el cuestionario?
ncol(cuestionario) #Tiene 21 variables

#9 ¿Qué variable ocupa la posición 10 en la tabla de datos?
cuestionario[10, ]

#10 Obtener una lista de las variables del cuestionario
names(cuestionario)

#11 Hacer un resumen de las variables del cuestionario
summary(cuestionario)
#Si quisieramos de una sola variable:
summary(cuestionario$amor)

#12 Utilizando la variable ocio de cuestionario, crear una variable que se llame horas.ocio
horas.ocio <- cuestionario$ocio

#13 Crear la variable 2*ocio dentro del data frame cuesitonario
cuestionario$ocio2 <- cuestionario$ocio*2
###

#14 Crear un objeto data.frame que solo contenga las variables clave ocio y horas.
objeto14<- data.frame(cuestionario$horas, cuestionario$ocio, cuestionario$horas)
objeto14bis<- cuestionario[,c("Clave", "horas", "ocio")]

#15 Crear un objeto data.frame solo con los valores de la muestra
varones_cuestionario <- cuestionario[cuestionario$genero == 1,]

#16 Calcular la media de la variable ocio y la variable horas
median(cuestionario$ocio, na.rm = TRUE)
median(cuestionario$horas, na.rm = TRUE)

#17 Calcular utilizando la función summary() un resumen estadístico de la variable ocio y la variable horas
summary(cuestionario$ocio, na.rm = TRUE)
summary(cuestionario$horas, na.rm = TRUE)

