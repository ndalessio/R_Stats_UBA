####################################
# EJERCICIO 9
###############################

Insomnio_2$terapia <- 1*Insomnio_2$Terapia1+3*Insomnio_2$Terapia2+5*Insomnio_2$Terapia3
table(Insomnio_2$terapia)
fre(Insomnio_2$terapia)


library(expss)
expss_output_viewer()

val_lab(Insomnio_2$terapia) <- make_labels("
                                     0 sin terapia
                                     1 terapia insomnio
                                     3 terapia ansiedad
                                     4 insomnio - ansiedad
                                     5 terapia fobia
                                     6 insomnio - fobia
                                     8 ansiedad - fobia
                                     9 todas las terapias
                                     ")

#######################################
# EJERCICIO 10
###############################


barplot(prop.table(table(Insomnio_2$terapia))*100,
        main="Tipos de terapia",
        xlab="Terapia",
        ylab="frecuencia porcentual",
        border="red",
        col="blue",
        density=10)

####################################################
# EJERCICIO 11
##############################################

boxplot(PrimerNoche~terapia, data = Insomnio_2, na.action = NULL,
        xlab = "horas dormidas",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

library(psych)
#########################################
# Q-Q PLOT CON EJEMPLOS
###################################

############################
# GENERACION DE NORMALES
###############################
y <- rnorm(500)

########################
# qqplot
#######################
qqnorm(y)
qqline(y)

qqnorm(Insomnio_2$PrimerNoche)
qqline(Insomnio_2$PrimerNoche)

##########################
# otros graficos de la normal
##############################
hist(Insomnio_2$PrimerNoche)
hist(y)
boxplot(y, horizontal = T, main="boxplot de una normal standard")
boxplot(Insomnio_2$PrimerNoche, horizontal = T, main="boxplot de primera noche")
##################################
# descriptivos
##############################
describe(y, na.rm=TRUE, skew = TRUE, ranges = TRUE,
         trim=.1, IQR = T)

quantile(y,c(.25,.75))

###############################################
# PROBAR CON DISTINTOS ENES
#############################################
###############
# estimaciones con distintos enes
##########################################
y10 <- rnorm(10)
mean(y10)
hist(y10)
y100 <- rnorm(100)
mean(y100)
hist(y100)
y1000 <- rnorm(1000)
mean(y1000)
hist(y1000)
y10000 <- rnorm(10000)
hist(y10000)
y100000 <- rnorm(100000)
mean(y100000)

summary(y10)

#######################################
# graficar una distribución normal 
#######################################
xx <- seq(-4, 4, by = .01)
yy <- dnorm(xx, mean = 0, sd = 1.0)

plot(xx,yy, main = "Normal Distribution", col = "blue")

#####################################################
# CALCULAR PROBABILIDADES CON LA NORMAL
#################################################
# acumuladas a izquierda, como siempre
#pnorm(0) es la prob de que una normal(0,1) sea menor o igual a 0

#########################################
pnorm(0)

#############################################
# percentiles normales
###################################3
qnorm(.75)

# armar cosas con todo esto, ejemplos y ejercicios
####################################
# deteccion de outliers
##################################
limiteinferior <- qnorm(.25) - 1.5*(qnorm(.75)-qnorm(.25))
limitesuperior <- qnorm(.75) + 1.5*(qnorm(.75)-qnorm(.25))
limiteinferior
limitesuperior
pnorm(limitesuperior)-pnorm(limiteinferior)

###################################
# normales con otras medias y desvíos
##################################
library(dplyr)

num_vec <- c(1:4, NA)
recode(num_vec, `2` = 20L, `4` = 40L)

vec <- c("no","si")
vec1 <- recode(vec, "no" = 0, "si" = 1)
