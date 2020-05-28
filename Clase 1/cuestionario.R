#CARGA LA LIBRERIA QUE PERMITE IMPORTAR UN ARCHIVO EXCEL#
library(readxl)

#IMPORTA EL ARCHIVO EXCEL#
cuestionario <- read_excel("cuestionario.xlsx")

#VISUALIZA EL DATA.FRAME "CUESTIONARIO" #
View(cuestionario)

# PRESENTA LOS NOMBRES DE LAS VARIABLES DE CUESTIONARIO #
names(cuestionario)

#PRESENTA LOS PRIMEROS REGISTROS DE CUESTIONARIO #
head(cuestionario)

# PRESENTA UN PRIMER RESUMEN ESTADISTICO DE LAS VARIABLES DE CUESTIONARIO #
summary(cuestionario)

#SUMAR LOS INGRESOS#
cuestionario$suma <- cuestionario$ingreso1+cuestionario$ingreso2+cuestionario$ingreso3

# CORTO POR OTRA VARIABLE Y RESUELVO EL PROBLEMA MISSING#
tapply(cuestionario$horas,cuestionario$genero,mean,na.rm=T)
mean(cuestionario$horas,na.rm=T)


#CALCULA LA MEDIA DE LA VARIABLE EDAD #
mean(cuestionario$edad)

# CALCULA EL DESVIO STANDARD DE LA VARIABLE EDAD #
sd(cuestionario$edad)

# CALCULA LA VARIANZA DE LA VARIABLE EDAD #
var(cuestionario$edad)

# CALCULA EL MAXIMO DE LA VARIABLE EDAD #
max(cuestionario$edad)

# CALCULA EL MINIMO DE LA VARIABLE EDAD #
min(cuestionario$edad)

# CALCULA UN RESUMEN ESTADISTICO DE LA VARIABLE EDAD #
summary(cuestionario$edad)

# GENERA UN DATA.FRAME A PARTIR DE CUESTIONARIO SEGUN UN CRITERIO DE 
#SELECCION #
hombres <- cuestionario[cuestionario$genero==1,]

# GENERA UN DATA.FRAME SELECCIONANDO ALGUNAS VARIABLES #
# LA SELECCION SE HACE DE DOS MANERAS DIFERENTES #
maschico <- cuestionario[,c("Clave","ocio","horas")]
maschico2 <- cuestionario[,c(1,2,3)]

# GENERO UNA VARIABLE FUERA DEL DATA.FRAME #
edad <- cuestionario$edad

# CALCULO UNA NUEVA VARIABLE EN EL DATA.FRAME #
cuestionario$edad2 <- cuestionario$edad*2
