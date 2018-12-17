library(cluster)
library(factoextra)

whole_data <- read.csv("wine.data", header=TRUE, sep=",")
remove <- c()

#Para cada columna
for(j in 2:14)
{
    rows <- whole_data[,j]
    IQR <- summary(whole_data[,j])[5] - summary(whole_data[,j])[2]
    rangeSup <- summary(whole_data[,j])[5] + 1.5*IQR
    rangeInf <- summary(whole_data[,j])[2] - 1.5*IQR
    min <- summary(whole_data[,j])[1]
    max <- summary(whole_data[,j])[6]

    #se guarda una imagen de todos los row de la columna i
    jpeg(paste("img/outliers/", names(whole_data)[j], "-before", ".jpeg", sep=""))
    plot(whole_data[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(whole_data)[j])
    #Se establecen los limites considerados normales. Fuera de esto es outlier
    abline(h = rangeSup, col="red")
    abline(h = rangeInf, col="red")
    dev.off()
    
    #Los que estan fuera se guardan para ser eliminados posteriormente
    for(i in 1:length(whole_data[,j]))
    {
        if (whole_data[i,j] < rangeInf | whole_data[i,j] > rangeSup)
        {
            remove <- c(remove, i)
        }
    }
}

#Se eliminan los outliers
data <- whole_data[-remove,]

for(j in 2:14)
{
    rows <- data[,j]
    IQR <- summary(whole_data[,j])[5] - summary(whole_data[,j])[2]
    rangeSup <- summary(whole_data[,j])[5] + 1.5*IQR
    rangeInf <- summary(whole_data[,j])[2] - 1.5*IQR
    min <- summary(whole_data[,j])[1]
    max <- summary(whole_data[,j])[6]
    
    jpeg(paste("img/outliers/", names(whole_data)[j], "-after", ".jpeg", sep=""))
    plot(data[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(whole_data)[j])
    abline(h = rangeSup, col="red")
    abline(h = rangeInf, col="red")
    dev.off()
}

#Se usa el metodo de la silueta para ver el mejor k
sil <- c()
for (i in 2:length(data$Alcohol)-1)
{
  fit <- pam(data, diss = FALSE, k = i)
  sil[i] <- fit$silinfo$avg.width
}

#Se guardan los puntos que da el metodo
jpeg("img/K-Graph.jpeg")
plot(sil)
dev.off()

#Se divide en k grupos
km_clusters <- pam(data, diss = FALSE, k = which.max(sil))

jpeg("img/kmeans.jpeg")
plot(fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Resultados clustering K-means") + 
        theme_bw()
)
dev.off()

#Se añade la columna cluster para identificar el tipo de cluster al que pertenece
data$cluster <- km_clusters$clustering

#Se procede a hacer un analisis
datacluster1 <- data[data$cluster == 1,]
datacluster2 <- data[data$cluster == 2,]
for(i in 1:14)
{
    x1 <- c(1:length(datacluster1[,i]))
    x2 <- c(1:length(datacluster2[,i]))
    y1 <- datacluster1[,i]
    y2 <- datacluster2[,i]

    jpeg(paste("img/groupDiff/", i, ".jpeg", sep=""))

    if(length(x1) > length(x2))
    {
        plot(x1, y1, type="o", ylim=c( min(y1,y2), max(y1,y2) ), col="blue", pch=18, main=names(datacluster1)[i], xlab="Index", ylab="Value")
        lines(y2, col="red", pch=19)
        legend(1, max(y1,y2), legend=c("Cluster 1", "Cluster 2"), col=c("blue", "red"), lty=1, cex=0.8)
    }
    else
    {
        plot(x2, y2, type="o", ylim=c( min(y1,y2), max(y1,y2) ), col="red", pch=19, main=names(datacluster1)[i], xlab="Index", ylab="Value")
        lines(y1, col="blue", pch=18)
        legend(1, max(y1,y2), legend=c("Cluster 2", "Cluster 1"), col=c("red", "blue"))
    }
    dev.off()
}

for(i in 1:14)
{
    x1 <- c(1:length(datacluster1[,i]))
    x2 <- c(1:length(datacluster2[,i]))
    y1 <- sort(datacluster1[,i])
    y2 <- sort(datacluster2[,i])

    jpeg(paste("img/groupDiffSorted/", i, ".jpeg", sep=""))
    if(length(x1) > length(x2))
    {
        plot(x1, y1, type="b", ylim=c( min(y1,y2), max(y1,y2) ), col="blue", main=names(datacluster1)[i], xlab="Index", ylab="Value")
        lines(y2, col="red")
        legend(1, max(y1,y2), legend=c("Cluster 1", "Cluster 2"), col=c("blue", "red"), lty=1, cex=0.8)
    }
    else
    {
        plot(x2, y2, type="b", ylim=c( min(y1,y2), max(y1,y2) ), col="red", main=names(datacluster1)[i], xlab="Index", ylab="Value")
        lines(y1, col="blue")
        legend(1, max(y1,y2), legend=c("Cluster 2", "Cluster 1"), col=c("red", "blue"), lty=1, cex=0.8)
    }
    dev.off()
}


#Se guarda el summary para cada variable de ambos clusters
write.table(t(summary(datacluster1)), file="sum_datacluster1.csv")
write.table(t(summary(datacluster2)), file="sum_datacluster2.csv")
