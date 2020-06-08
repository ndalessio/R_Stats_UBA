############################
# SE CARGA EL PAQUETE expss#
############################
library(expss)

##########################################
# PARA QUE LAS TABLAS SALGAN EN EL VISOR #
##########################################
expss_output_rnotebook()

#######################
# TABLA DE FRECUENCIA #
#######################
fre(hombres$libros,
    stat_lab = getOption("expss.fre_stat_lab", c("Count", "Valid percent", "Percent",
                                                 "Responses, %", "Cumulative responses, %")))

#######################################################
# GRAFICO DE BARRAS DE LA FRECUENCIA RELATIVA DE LIBROS #
#######################################################
barplot(prop.table(table(cuestionario$libros)),
        main="Cantidad de libros leidos",
        xlab="Libros",
        ylab="frecuencia relativa",
        border="red",
        col="blue",
        density=10)

############################
# PARA HACER EL HISTOGRAMA #
############################
hist(cuestionario$libros, 
     main="Histograma de cantidad de libros leidos", 
     xlab="Libros",
     freq=F,
     border="blue", 
     col="green",
     xlim=c(0,50),
     las=1, 
     breaks=20)

lines(density(cuestionario$libros, na.rm=T))

###########################################
# PARA DESCRIPTIVOS 
###############################
library(psych)

describe(cuestionario$libros)

describe(cuestionario$libros, na.rm=TRUE, skew = TRUE, ranges = TRUE,
         trim=.1, quant = NULL, IQR = TRUE)

describeBy(cuestionario$libros,cuestionario$genero)
