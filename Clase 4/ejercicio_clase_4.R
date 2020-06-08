library(tidyverse)
library(expss)
library(psych)
library(ggplot2)

#EJERCICIO 1
#Determinar la cantidad de hombres y mujeres que integran la muestra. Realizar un gráfico adecuado

addmargins(prop.table
          (table(cuestionario$genero))*100) %>% 
          round(2)

#Utilizo expss para agregar nombres a las variables
val_lab(cuestionario$genero) <- make_labels("1 masculino 2 femenino ")

#Grafico con barplot:
barplot((prop.table(table(cuestionario$genero))*100),
          main = "Genero",
          ylab = "Frecuencia",
          xlab = 'Genero',
         
          col = c("Darkgreen", "Violet"),
          border = FALSE) %>% round(2)

#Pregunta. Conviene que el eje y esté al 100%?

#Gráfico con ggplot2
genero <- cuestionario$genero

genero = ifelse(genero == 1, "Masculino", "Femenino")

ggplot(cuestionario, mapping = aes(genero, fill = genero)) +
    geom_bar() 
