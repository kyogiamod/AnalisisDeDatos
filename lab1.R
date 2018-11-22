data <- read.csv("wine.data", header=TRUE, sep=",")

#data
#write.csv(t(summary(data)), "summary.txt")

#summary(data)

#varianza, covarianza, 
varianza <- c()
coefvar <- c()
promedio <- c()
for (i in 1:14)
{
    varianza <- c(varianza,var(data[,i]))
    promedio <- c(promedio, mean(data[,i]))
}

coefVar <- function(des, med) { return(des/abs(med)) }
coefvar <- coefVar(sqrt(varianza), promedio)

#write.csv(round(coefvar,3), "asd.txt")
#write.csv(round(varianza,3), "asd2.txt")

#Se crean los graficos de los histogramas
jpeg("img/a1.jpeg")
hist(data[,1], breaks=9, xlab="Clase", ylab="Cantidad", main="Histograma para la variable clase")
dev.off()

jpeg("img/a2.jpeg")
hist(data[,2], breaks=2, xlab="Intervalos de alcohol", ylab="Cantidad", main="Histograma para la variable alcohol")
dev.off()

jpeg("img/a3.jpeg")
hist(data[,3], breaks=9, xlab="Intervalos de acido malico", ylab="Cantidad", main="Histograma para la variable Malic acid")
dev.off()

jpeg("img/a4.jpeg")
hist(data[,4], breaks=9, xlab="Intervalos de ceniza", ylab="Cantidad", main="Histograma para la variable Ash")
dev.off()

jpeg("img/a5.jpeg")
hist(data[,5], breaks=9, xlab="Intervalos de alcanilidad", ylab="Cantidad", main="Histograma para la variable Alkalinity")
dev.off()

jpeg("img/a6.jpeg")
hist(data[,6], breaks=9, xlab="Intervalos de magnesio", ylab="Cantidad", main="Histograma para la variable Magnesium")
dev.off()

jpeg("img/a7.jpeg")
hist(data[,7], breaks=9, xlab="Intervalos de fenoles totales", ylab="Cantidad", main="Histograma para la variable Total phenols")
dev.off()

jpeg("img/a8.jpeg")
hist(data[,8], breaks=9, xlab="Intervalos de flavanoides", ylab="Cantidad", main="Histograma para la variable Flavanoids")
dev.off()

jpeg("img/a9.jpeg")
hist(data[,9], breaks=9, xlab="Intervalos de fenoles no flavanoides", ylab="Cantidad", main="Histograma para la variable Nonflavanoid phenols")
dev.off()

jpeg("img/a10.jpeg")
hist(data[,10], breaks=9, xlab="Intervalos de proanthocyanins", ylab="Cantidad", main="Histograma para la variable Proanthocyanidins")
dev.off()

jpeg("img/a11.jpeg")
hist(data[,11], breaks=9, xlab="Intervalos de intensidad de color", ylab="Cantidad", main="Histograma para la variable Color intensity")
dev.off()

jpeg("img/a12.jpeg")
hist(data[,12], breaks=9, xlab="Intervalos de matiz", ylab="Cantidad", main="Histograma para la variable Hue")
dev.off()

jpeg("img/a13.jpeg")
hist(data[,13], breaks=9, xlab="Intervalos de OD280/OD315", ylab="Cantidad", main="Histograma para la variable OD280/OD315 of diluted wines")
dev.off()

jpeg("img/a14.jpeg")
hist(data[,14], breaks=9, xlab="Intervalos de prolina", ylab="Cantidad", main="Histograma para la variable Proline")
dev.off()