library(tidyverse)
library(expss)

# 1. Utilizando table y sus funciones anexas, calcular:
# a.TABLAS FRECUENCIAS ABSOLUTAS para las variables ocio, genero, colegio, amor, religion

addmargins(table(cuestionario$ocio,
                 dnn = "Distribucion por actividad de ocio preferida",
                 useNA = "always"))
                
addmargins(table(cuestionario$genero,
                 dnn = "Distribucion por genero",
                 useNA = "always"))

addmargins(table(cuestionario$colegio, 
                 dnn = "Distribucion de acuerdo a tipo de colegio",
                 useNA = "always"))

addmargins(table(cuestionario$amor,
                 dnn = "Distribucion de acuerdo a estado afectivo",
                 useNA = "always"))
addmargins(table(cuestionario$religion, useNA = "always"))


# b. TABLAS FRECUENCIAS RELATIVAS para las variables ocio, genero, colegio, amor, religion

addmargins(prop.table(table(cuestionario$ocio,
                            dnn = "Distribucion por actividad de ocio preferida", 
                            useNA = "always")))

addmargins(prop.table(table(cuestionario$genero,
                            dnn = "Distribucion por genero",
                            useNA = "always")))

addmargins(prop.table(table(cuestionario$colegio,
                            dnn = "Distribucion por tipo de colegio",
                            useNA = "always")))

addmargins(prop.table(table(cuestionario$amor,
                            dnn = "Distribucion por estado afectivo",
                            useNA = "always")))

addmargins(prop.table(table(cuestionario$religion,
                            dnn = "Distribucion de acuerdo a tipo de religiÃ³n",
                            useNA = "always")))


#2.	Realizar graficos de barras para cada una de las variables anteriores

# GRAFICO BARRAS OCIO
barplot(table(cuestionario$ocio), beside = TRUE,
        main = "Observaciones de acuerdo actividad preferida de ocio",
        xlab = "Actividades",
        ylab = "Frecuencia",
        border = "red",
        col = "blue",
        density = 10,
        cex.names = 0.9)
        legend("topright", 
               c("1.Beber", "2.Deportes", "3.Viajes", "4.Cine", "5.Museos", "6.Leer", "7.Otras"),
               cex=0.6)    
        
# GRAFICO BARRAS OCIO GENERO
barplot(table(cuestionario$genero), beside = TRUE,
        main = "Distribucion de hombres y mujeres",
        xlab = "Sexo",
        ylab = "Frecuencia",
        col = c("green", "violet"),
        border = FALSE,
        names.arg = c("1 Hombre", "2 Mujer"))

# GRAFICO BARRAS COLEGIO
barplot(table(cuestionario$colegio), beside = TRUE,
        main = "Distribucion de acuerdo tipo de colegio",
        xlab = "Tipo de colegio",
        ylab = "Frecuencia",
        names.arg = 
          c("Centro estatal publico", "Centro privado no religioso", "Centro privado religioso"),
        cex.names = 0.6)

# GRAFICO BARRAS AMOR
barplot(table(cuestionario$amor), beside = TRUE,
        main = "Distribucion de acuerdo a estado afectivo",
        xlab = "Estado afectivo",
        ylab = "Frecuencia",
        col = c("Darkgreen", "Yellow", "Pink", "Orange"),
        border = FALSE)
        
        legend("topright", 
           c("Tiene novie formal", 
             "Ahora no tiene novio formal", 
             "Hasta ahora solo relaciones afectivas pasajeras", 
             "Nunca ha tenido una relación especial"),
           fill = c("Darkgreen", "Yellow", "Pink", "Orange"),
           cex=0.6) 

# GRAFICO BARRAS RELIGION
cuestionario$religion[cuestionario$religion == 99999] <- NA

barplot(table(cuestionario$religion), beside = TRUE,
        main = "Distrtibucion de acuerdo a tipo de colegio",
        xlab = "Tipo de colegio",
        ylab = "Frecuencia")


# 3.TABLAS DE CONTINGENCIAS

#a. TABLA DE CONTINGENCIA OCIO Y GENERO ABSOLUTA

addmargins(table(cuestionario$ocio, 
                 cuestionario$genero, 
                 useNA = "always",
                 dnn = c("Actividad de ocio preferida", "GÃ©nero")))%>% 
                round(2)

#a.	TABLA DE CONTINGENCIA OCIO Y GENERO RELATIVA

addmargins(prop.table(table(cuestionario$ocio, 
                            cuestionario$genero, 
                            useNA = "always",
                            dnn = c("Actividad de ocio preferida", "GÃ©nero"))))%>% 
                            round(2)


#b.	TABLA DE CONTINGENCIA OCIO Y COLEGIO ABSOLUTA

addmargins(table(cuestionario$ocio, 
                 cuestionario$colegio, 
                 useNA = "always",
                 dnn = c("Actividad de ocio preferida", "Tipo de colegio")))%>% 
                  round(2)

#b.	TABLA DE CONTINGENCIA OCIO Y COLEGIO RELATIVA

addmargins(prop.table(table(cuestionario$ocio, 
                            cuestionario$colegio, 
                            useNA = "always",
                            dnn = c("Actividad de ocio preferida", "Tipo de colegio"
                            ))))%>% 
                            round(2)

#c.	TABLA DE CONTINGENCIA AMOR Y RELIGION ABSOLUTA
addmargins(table(cuestionario$amor, 
                cuestionario$religion, 
                useNA = "always",
                dnn = c("Estado afectivo", "Religion")))%>% 
                round(2)

#c.	TABLA DE CONTINGENCIA AMOR Y RELIGION RELATIVA

addmargins(prop.table(table(cuestionario$amor, 
                            cuestionario$religion, 
                            useNA = "always",
                            dnn = c("Estado afectivo", "Religion"))))%>% 
                            round(2)

#d.TABLA DE CONTINGENCIA COLEGIO Y RELIGION ABSOLUTA

addmargins(table(cuestionario$colegio, 
                  cuestionario$religion, 
                  useNA = "always",
                  dnn = c("Tipo de colegio", "Religion")))%>% 
                  {. * 100} %>% 
                  round(2)

#d.TABLA DE CONTINGENCIA COLEGIO Y RELIGION RELATIVA

addmargins(prop.table(table(cuestionario$amor, 
                            cuestionario$religion, 
                            useNA = "always",
                            dnn = c("Estado afectivo", "Religion"))))%>% 
                            {. * 100} %>% 
                            round(2)

# 5 y 6 instalación y carga expss package
# 7. Incorporarle etiqueta a cada una de las variables utilizadas en esta actividad

var_lab(cuestionario$ocio) = "Actividades de ocio"
var_lab(cuestionario$genero) = "Genero"
var_lab(cuestionario$colegio) = "Colegio"
var_lab(cuestionario$amor) = "Amor"
var_lab(cuestionario$religion) = "Religion"

# 8.	Incorporarles etiquetas a todas las modalidades de las variables utilizadas en esta actividad

val_lab(cuestionario$ocio) <- make_labels("
                                     1 beber
                                     2 deportes
                                     3 viajar
                                     4 cine
                                     5 museos
                                     6 leer
                                     7 otros ")

val_lab(cuestionario$genero) <- make_labels("
                                     1 masculino
                                     2 femenino ")

val_lab(cuestionario$colegio) <- make_labels("
                                             1 Estatal y publico
                                             2 Privado no religioso
                                             3 Privado religioso")

val_lab(cuestionario$amor) <- make_labels("1 Tiene novie formal, 
                                           2 Ahora no tiene novio formal 
                                           3 Hasta ahora solo relaciones afectivas pasajeras 
                                           4 Nunca ha tenido una relación especial")

val_lab(cuestionario$religion) <- make_labels ("1 Catolico practicante
                                                2 Catolico no practicante
                                                3 Otra religion
                                                4 No creyente
                                                5 Indiferente")
                                              
  
#9.	Repetir el ejercicio 1 utilizando la funcion fre

#Primero, para poder ver tabla en la consola:
expss_output_default()

fre(cuestionario$ocio)
fre(cuestionario$ocio)
fre(cuestionario$colegio)
fre(cuestionario$amor)
fre(cuestionario$religion)

#10. Rehacer los graficos del ejercicio 2

# GRAFICO BARRAS OCIO FRECUENCIAS ABSOLUTAS 

barplot(table(cuestionario$ocio),
        main ="Actividad de ocio preferida",
        xlab ="Actividad",
        ylab ="frecuencia",
        col = "darkblue")

# GRAFICO DE BARRAS OCIO FRECUENCIAS RELATIVAS

barplot((prop.table(table(cuestionario$ocio))*100),
                   main = "Actividad de ocio preferida",
                   xlab = "Actividad",
                   ylab = "Frecuencia",
                   col = "darkblue")

#GRAFICO DE BARRAS GENERO FRECUENCIAS ABSOLUTAS

barplot(table(cuestionario$genero),
        main = "Genero",
        ylab = "Frecuencia",
        xlab = "Genero",
        col = "orange",
        border = FALSE)

#GRAFICO DE BARRAS GENERO FRECUENCIAS RELATIVAS

barplot(prop.table(table(cuestionario$genero)),
        main = "Genero",
        ylab = "Frecuencia",
        xlab = "Genero",
        col = "orange",
        border = FALSE)

#GRAFICO DE BARRAS COLEGIO FRECUENCIAS ABSOLUTAS

barplot(table(cuestionario$colegio),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")

#GRAFICO DE BARRAS COLEGIO FRECUENCIAS RELATIVAS

barplot(prop.table(table(cuestionario$colegio)),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")

#GRAFICO DE BARRAS AMOR FRECUENCIAS ABSOLUTAS

barplot(table(cuestionario$amor),
        main = "Amor",
        ylab = "Frecuencia",
        xlab = "Tipo de relacion",
        col = "darkblue", 
        density = 50,
        cex.names = 0.6)

#GRAFICO DE BARRAS AMOR FRECUENCIAS RELATIVAS

barplot(prop.table(table(cuestionario$amor)),
        main = "Amor",
        ylab = "Frecuencia",
        xlab = "Tipo de relacion",
        col = "darkblue", 
        density = 50,
        cex.names = 0.6)

#GRAFICO DE BARRAS RELIGION FRECUENCIAS ABSOLUTAS

barplot(table(cuestionario$religion),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")

#GRAFICO DE BARRAS RELIGION FRECUENCIAS RELATIVAS

barplot(prop.table(table(cuestionario$religion)),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")

# 11.	Repetir el ejercicio 3 utilizando la función cro y sus funciones afines

cro(cuestionario$ocio, cuestionario$genero)
cro(cuestionario$ocio, cuestionario$colegio)
cro(cuestionario$amor, cuestionario$religion)
cro(cuestionario$colegio, cuestionario$religion)

# 12.	Rehacer los gráficos del ejercicio 4 
# (4.	Realizar los gráficos de las tablas de contingencia anteriores para frecuencias relativas)

barplot((prop.table(table(cuestionario$ocio))*100),
        main = "Actividad de ocio preferida",
        xlab = "Actividad",
        ylab = "Frecuencia",
        col = "darkblue")

barplot((prop.table(table(cuestionario$genero))*100),
        main = "Genero",
        ylab = "Frecuencia",
        xlab = "Genero",
        col = c("orange", "red"),
        border = FALSE)

barplot((prop.table(table(cuestionario$colegio))*100),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")

barplot((prop.table(table(cuestionario$amor))*100),
        main = "Amor",
        ylab = "Frecuencia",
        xlab = "Tipo de relacion",
        col = "red", 
        border = "red",
        density = 50,
        cex.names = 0.6)

barplot((prop.table(table(cuestionario$religion))*100),
        main = "Tipo de colegio",
        ylab = "Frecuencia",
        xlab = "Colegio",
        col = "blue")
