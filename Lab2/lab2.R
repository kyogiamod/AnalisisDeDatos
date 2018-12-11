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
    #Se establecen los limites coonsiderados normales. Fuera de esto es outlier
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
#data$Class.identifier <- NULL

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
