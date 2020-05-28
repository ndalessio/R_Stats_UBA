###########################################################
# SCRIPT DE LA CLASE 3 - TABLAS DE FRECUENCIAS Y GRAFICOS #
###########################################################


##############################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE #
#################################################
table(hombres$ocio)

##############################################
#TABLA DE FRECUENCIA RELATIVA PARA UNA VARIABLE #
#################################################

prop.table(table(hombres$ocio))

##############################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE CON TOTAL#
#################################################

addmargins(table(hombres$ocio))

#####################################################################
#TABLA DE FRECUENCIA ABSOLUTA PARA UNA VARIABLE INCLUYENDO MISSINGS #
#####################################################################

addmargins(table(hombres$ocio,useNA = "always"))

##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS#
##############################################
table(hombres$ocio, hombres$colegio)

##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS#
##############################################

prop.table(table(hombres$ocio, hombres$colegio))

##############################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS CON TOTALES#
##############################################
addmargins(table(hombres$ocio, hombres$colegio))


#####################################################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS CON TOTALES Y MISSINGS#
#####################################################################
addmargins(table(hombres$ocio, hombres$colegio,useNA = "always"))


###################################
# FRECUENCIAS CON EL PAQUETE expss#
###################################

############################
# SE CARGA EL PAQUETE expss#
############################
library(expss)

##############################
# ETIQUETA PARA UNA VARIABLE #
##############################

var_lab(hombres$ocio) = "Actividades de ocio"
var_lab(hombres$colegio) = "Colegio"

#########################################
# ETIQUETA PARA VALORES DE UNA VARIABLE #
#########################################
val_lab(hombres$ocio) <- make_labels("
                                     1 beber
                                     2 deportes
                                     3 viajar
                                     4 cine
                                     5 museos
                                     6 leer
                                     7 otros ")

val_lab(hombres$colegio) <- make_labels("
                                     1 estatal
                                     2 no religioso
                                     3 religioso")
#######################
# TABLA DE FRECUENCIA #
#######################
fre(hombres$ocio)



###############################################
# TABLA DE CONTINGENCIA FRECUENCIAS ABSOLUTAS #
###############################################
cro(hombres$ocio,hombres$colegio)

#######################################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS COLUMNA #
#######################################################

cro_cpct(hombres$ocio,hombres$colegio)

#######################################################
# TABLA DE CONTINGENCIA FRECUENCIAS RELATIVAS TOTALES #
#######################################################

cro_tpct(hombres$ocio,hombres$colegio)


############
# GRAFICOS #
############

#####################################################
#GRAFICO DE BARRAS DE LA FRECUENCIA ABSOLUTA DE OCIO#
#####################################################
barplot(table(hombres$ocio),
        main="Actividades de tiempo libre",
        xlab="Actividad",
        ylab="Casos",
        border="red",
        col="blue",
        density=10)

#######################################################
# GRAFICO DE BARRAS DE LA FRECUENCIA RELATIVA DE OCIO #
#######################################################
barplot(prop.table(table(hombres$ocio)),
        main="Actividades de tiempo libre",
        xlab="Actividad",
        ylab="frecuencia relativa",
        border="red",
        col="blue",
        density=10)

################################
# GRAFICO DE TORTAS BASICO 
################################
pie(table(hombres$ocio),
    main="Actividades en tiempo de ocio",
    col=c("red","orange","yellow","blue","green","black"),
    border="brown",
    clockwise=TRUE)

######################################################
# GRAFICO DE BARRAS PARA DOS VARIABLES FREC ABSOLUTAS#
######################################################

barplot(table(hombres$colegio,hombres$ocio),beside=T,
        main = "Actividades de ocio según tipo de escuela",
        xlab = "Actividad",
        ylab = "Frecuencia",
        col = c("red","green","blue"))
legend("topright",
       c("Pública","Privada no religiosa", "Privada religiosa"),
       fill = c("red","green","blue"))
######################################################################

##################################################################
# GRAFICO DE BARRAS PARA DOS VARIABLES FREC RELATIVAS POR COLUMNA#
##################################################################

barplot(prop.table(table(hombres$colegio,hombres$ocio),1),beside=T,
        main = "Actividades de ocio según tipo de escuela",
        xlab = "Actividad",
        ylab = "Frecuencia",
        col = c("red","green","blue"))
legend("topright",
       c("Pública","Privada no religiosa", "Privada religiosa"),
       fill = c("red","green","blue"))
######################################################################

