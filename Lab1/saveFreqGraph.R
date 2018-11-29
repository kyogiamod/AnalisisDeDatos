saveFreqGraph <- function(data)
{
    jpeg("img/frequency/a1.jpeg")
    plot(table(data[,1]), xlab="Clase", ylab="Cantidad", main="Frecuencia para la variable clase")
    dev.off()

    jpeg("img/frequency/a2.jpeg")
    plot(table(data[,2]), xlab="alcohol", ylab="Cantidad", main="Frecuencia para la variable alcohol")
    dev.off()

    jpeg("img/frequency/a3.jpeg")
    plot(table(data[,3]), xlab="acido malico", ylab="Cantidad", main="Frecuencia para la variable Malic acid")
    dev.off()

    jpeg("img/frequency/a4.jpeg")
    plot(table(data[,4]), xlab="ceniza", ylab="Cantidad", main="Frecuencia para la variable Ash")
    dev.off()

    jpeg("img/frequency/a5.jpeg")
    plot(table(data[,5]), xlab="alcanilidad", ylab="Cantidad", main="Frecuencia para la variable Alkalinity")
    dev.off()

    jpeg("img/frequency/a6.jpeg")
    plot(table(data[,6]), xlab="magnesio", ylab="Cantidad", main="Frecuencia para la variable Magnesium")
    dev.off()

    jpeg("img/frequency/a7.jpeg")
    plot(table(data[,7]), xlab="fenoles totales", ylab="Cantidad", main="Frecuencia para la variable Total phenols")
    dev.off()

    jpeg("img/frequency/a8.jpeg")
    plot(table(data[,8]), xlab="flavanoides", ylab="Cantidad", main="Frecuencia para la variable Flavanoids")
    dev.off()

    jpeg("img/frequency/a9.jpeg")
    plot(table(data[,9]), xlab="fenoles no flavanoides", ylab="Cantidad", main="Frecuencia para la variable Nonflavanoid phenols")
    dev.off()

    jpeg("img/frequency/a10.jpeg")
    plot(table(data[,10]), xlab="proanthocyanins", ylab="Cantidad", main="Frecuencia para la variable Proanthocyanidins")
    dev.off()

    jpeg("img/frequency/a11.jpeg")
    plot(table(data[,11]), xlab="color", ylab="Cantidad", main="Frecuencia para la variable Color intensity")
    dev.off()

    jpeg("img/frequency/a12.jpeg")
    plot(table(data[,12]), xlab="matiz", ylab="Cantidad", main="Frecuencia para la variable Hue")
    dev.off()

    jpeg("img/frequency/a13.jpeg")
    plot(table(data[,13]), xlab="OD280/OD315", ylab="Cantidad", main="Frecuencia para la variable OD280/OD315 of diluted wines")
    dev.off()

    jpeg("img/frequency/a14.jpeg")
    plot(table(data[,14]), xlab="prolina", ylab="Cantidad", main="Frecuencia para la variable Proline")
    dev.off()
}