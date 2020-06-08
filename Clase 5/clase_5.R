library(expss)
expss_output_viewer()

table(cuestionario$genero)

val_lab(cuestionario$genero) <- make_labels("
                                     1 masculino
                                     2 femenino
                                     ")

pie(table(cuestionario$genero),
    main="Género",
    col=c("yellow","blue"),
    border="brown",
    clockwise=TRUE)



val_lab(cuestionario$colegio) <- make_labels("
1 estatal
2 privado no religioso
3 privado religioso")

fre(cuestionario$colegio)

barplot(prop.table(table(cuestionario$libros))*100,
        main="Colegio",
        xlab="COlegio",
        ylab="frecuencia porcentual",
        border="red",
        col="blue",
        density=10)

fre(cuestionario$libros)
fre(cuestionario$horas)

library(psych)

describe(cuestionario$libros, na.rm=TRUE, skew = TRUE, ranges = TRUE,
         trim=.1, quant = c(.1,.2), IQR = T)

describe(cuestionario$tv, na.rm=TRUE, skew = F, ranges = F,
         trim=.1, quant = c(.7, .8, .82) , IQR = F)

describe(cuestionario$horas, na.rm=TRUE, skew = F, ranges = F,
         trim=.1, quant = c(.7) , IQR = F)


hist(cuestionario$ingresot, 
     main="Histograma de Ingreso total", 
     xlab="Ingreso",
     freq=F,
     border="blue", 
     col="green",
     xlim=c(0,40),
     las=1, 
     breaks=10)


###################################
# GRAFICO TALLO HOJA
##########################

stem(cuestionario$libros, scale = 2, width = 80)

stem(cuestionario$ingresot, scale = 1, width = 80)

#######################################
# BOXPLOTS
#####################################


boxplot(cuestionario$libros~colegio, data = cuestionario, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

#######################################################
# como programar una variable que indique los missings
#######################################################

lsup <- 1.5*IQR(cuestionario$horas,na.rm=T)+quantile(cuestionario$horas,0.75,na.rm=T)
linf <- -1.5*IQR(cuestionario$horas,na.rm=T)+quantile(cuestionario$horas,0.25,na.rm=T)

condicion <- cuestionario$horas > lsup | cuestionario$horas < linf 
ncondicion <- as.numeric(condicion)

#########################################
# BOXPLOT QUE CORTA POR MAS DE UNA VARIABLE
##############################################
boxplot(PrimerNoche~Sexo+Edad, data = Insomnio_2, na.action = NULL,
        xlab = "x",
        ylab = "y",main="titulo", ann = T, horizontal = T,
        varwidth = F, outline=T, col="yellow")

###############################################
# BOXPLOT PARA DISTINTAS VARIABLES JUNTAS
###############################################
boxplot(Insomnio_2[,c(2,3,4)])
