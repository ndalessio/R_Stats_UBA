# ACTIVIDAD CLASE 6

# 1. Generar 500 valores de una variable con distribución normal de media 0 y desvío 1
normal_d <- rnorm(500, 0, 1)

# 2. Calcular los cuartiles inferior, superior y el rango intercuartil
quantile(normal_d, c(0.25, 0.75))
rango_intercuartil <- IQR(normal_d)

# 3. Calcular las cerca interiores y la probabilidad de que la variable tome valores entre ellos
limite_inferior <- quantile(normal_d, 0.25) - (1.5 * rango_intercuartil)
limite_superior <- quantile(normal_d, 0.75) + (1.5 * rango_intercuartil)

probabilidad <- pnorm(limite_superior) - pnorm(limite_inferior)

# 4. Hacer un histograma y un boxplot de la variable
hist(normal_d,
     main = 'Histograma',
     xlab = 'X',
     ylab = 'Frecuencia',
     col = 'orange')

boxplot(normal_d,
        main = 'Boxplot',
        xlab = 'X',
        ylab = 'Frecuencia',
        col = 'orange',
        horizontal = TRUE)

# 5. Evaluar la normalidad mediante el q-q plot
qqnorm(normal_d,
       main = 'Qqplot normalidad')

qqline(normal_d,
       lwd = 2)

# 6. Generando variables normales 10, 20, 50, 100, 1000 y 10000 valores, calcular y comparar media y desvío observando cómo tienden al valor del parámetro respectivo.
normales_10 <- rnorm(10)
normales_20 <- rnorm(20)
normales_50 <- rnorm(50)
normales_100 <- rnorm(100)
normales_1000 <- rnorm(1000)
normales_10000 <- rnorm(10000)

mean(normales_10)
sd(normales_10)

mean(normales_20)
sd(normales_20)

mean(normales_50)
sd(normales_50)

mean(normales_100)
sd(normales_100)

mean(normales_1000)
sd(normales_1000)

mean(normales_10000)
sd(normales_10000)

# 7. Con las variables generadas en 6, realizar histogramas y boxplots, analizando la evolución de los mismos a medida que el tamaño muestral crece
df <- data.frame(normales_10, normales_20, normales_50, normales_100, normales_1000, normales_10000)

hist(df$normales_10, main = "10", xlab = "x", ylab = "Frecuencia")
hist(df$normales_20, main = "20", xlab = "x", ylab = "Frecuencia")
hist(df$normales_50, main = "10", xlab = "x", ylab = "Frecuencia")
hist(df$normales_100, main = "100", xlab = "x", ylab = "Frecuencia")
hist(df$normales_1000, main = "1000", xlab = "x", ylab = "Frecuencia")
hist(df$normales_10000, main = "10000", xlab = "x", ylab = "Frecuencia")

boxplot(df$normales_10, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales_20, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales_50, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales_100, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales_1000, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales_10000, main = "10", xlab = "10", horizontal = TRUE)

# 8. Repetir los puntos 1 a 7 pero generando una variable normal de media 10 y desvío 5.
# 1
normal2_d <- rnorm(500, 10, 5)

# 2
quantile(normal2_d, c(0.25, 0.75))
rango_intercuartil <- IQR(normal2_d)

# 3
limite_inferior <- quantile(normal2_d, 0.25) - (1.5 * rango2_intercuartil)
limite_superior <- quantile(normal2_d, 0.75) + (1.5 * rango2_intercuartil)
probabilidad2 <- pnorm(limite_superior) - pnorm(limite_inferior)

# 4
hist(normal2_d,
     main = 'Histograma',
     xlab = 'X',
     ylab = 'Frecuencia',
     col = 'orange')

boxplot(normal2_d,
        main = 'Boxplot',
        xlab = 'X',
        ylab = 'Frecuencia',
        col = 'orange',
        horizontal = TRUE)

# 5 
qqnorm(normal2_d,
       main = 'Qqplot normalidad')

qqline(normal2_d,
       lwd = 2)

# 6
normales2_10 <- rnorm(10, 10, 5)
normales2_20 <- rnorm(20, 10, 5)
normales2_50 <- rnorm(50, 10, 5)
normales2_100 <- rnorm(100, 10, 5)
normales2_1000 <- rnorm(1000, 10, 5)
normales2_10000 <- rnorm(10000, 10, 5)

mean(normales2_10)
sd(normales2_10)

mean(normales2_20)
sd(normales2_20)

mean(normales2_50)
sd(normales2_50)

mean(normales2_100)
sd(normales2_100)

mean(normales2_1000)
sd(normales2_1000)

mean(normales2_10000)
sd(normales_10000) 

# 7
df <- data.frame(normales2_10, normales2_20, normales2_50, normales2_100, normales2_1000, normales2_10000)

hist(df$normales2_10, main = "10", xlab = "x", ylab = "Frecuencia")
hist(df$normales2_20, main = "20", xlab = "x", ylab = "Frecuencia")
hist(df$normales2_50, main = "10", xlab = "x", ylab = "Frecuencia")
hist(df$normales2_100, main = "100", xlab = "x", ylab = "Frecuencia")
hist(df$normales2_1000, main = "1000", xlab = "x", ylab = "Frecuencia")
hist(df$normales2_10000, main = "10000", xlab = "x", ylab = "Frecuencia")

boxplot(df$normales2_10, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales2_20, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales2_50, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales2_100, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales2_1000, main = "10", xlab = "10", horizontal = TRUE)
boxplot(df$normales2_10000, main = "10", xlab = "10", horizontal = TRUE)

# 9. Evaluar normalidad en las variables cuantitativas del archivo cuestionario
library(readxl)
cuestionario <- read_excel("~/R/R_Stats_UBA/cuestionario.xlsx")
View(cuestionario)

qqnorm(cuestionario$ocio, main = 'qqplot para normalidad de horas ocio')
qqline(cuestionario$ocio,lwd = 2)

qqnorm(cuestionario$tv, main = 'qqplot para normalidad horas tv')
qqline(cuestionario$tv,lwd = 2)


qqnorm(cuestionario$ingreso1, main = 'qqplot para normalidad ingresos personales')
qqline(cuestionario$ingreso1, lwd = 2)

qqnorm(cuestionario$ingreso2,main = 'qqplot para normalidad ingresos pareja')
qqline(cuestionario$ingreso2, lwd = 2)

qqnorm(cuestionario$ingreso3,main = 'qqplot para normalidad aportes familiares')
qqline(cuestionario$ingreso3, lwd = 2)


# 10. Evaluar normalidad de las variables cuantitativas del archivo insomnio
library(haven)
Insomnio_2 <- read_sav("~/R/R_Stats_UBA/Clase 5/Insomnio_2.sav")
View(Insomnio_2)

qqnorm(Insomnio_2$PrimerNoche, main = 'qqplot para normalidad horas sueno primer noche')
qqline(Insomnio_2$PrimerNoche, lwd = 2)

qqnorm(Insomnio_2$SegundaNoche, main = 'qqplot para normalidad horas sueno segunda noche')
qqline(Insomnio_2$SegundaNoche, lwd = 2)

qqnorm(Insomnio_2$TerceraNoche, main = 'qqplot para normalidad horas sueno tercera noche')
qqline(Insomnio_2$TerceraNoche, lwd = 2)
