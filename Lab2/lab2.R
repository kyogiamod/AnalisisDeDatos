data <- read.csv("/Users/kyogia/Desktop/analisis/Lab2/wine.data", header=TRUE, sep=",")
cleanData <- data
remove <- c()

for(j in 2:14)
{
    rows <- data[,j]
    IQR <- summary(data[,j])[5] - summary(data[,j])[2]
    rangeSup <- summary(data[,j])[5] + 1.5*IQR
    rangeInf <- summary(data[,j])[2] - 1.5*IQR
    min <- summary(data[,j])[1]
    max <- summary(data[,j])[6]

    jpeg(paste("Lab2/img/outliers/", names(data)[j], "-before", ".jpeg", sep=""))
    plot(data[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(data)[j])
    abline(h = rangeSup, col="red")
    abline(h = rangeInf, col="red")
    dev.off()
       
    for(i in 1:length(data[,j]))
    {
        if (data[i,j] < rangeInf | data[i,j] > rangeSup)
        {
            remove <- c(remove, i)
        }
    }
}

cleanData <- data[-remove,]

for(j in 2:14)
{
    rows <- cleanData[,j]
    IQR <- summary(data[,j])[5] - summary(data[,j])[2]
    rangeSup <- summary(data[,j])[5] + 1.5*IQR
    rangeInf <- summary(data[,j])[2] - 1.5*IQR
    min <- summary(data[,j])[1]
    max <- summary(data[,j])[6]
    
    jpeg(paste("Lab2/img/outliers/", names(data)[j], "-after", ".jpeg", sep=""))
    plot(cleanData[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(data)[j])
    abline(h = rangeSup, col="red")
    abline(h = rangeInf, col="red")
    dev.off()
}
