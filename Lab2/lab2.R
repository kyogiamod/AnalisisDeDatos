library(cluster)
library(factoextra)

whole_data <- read.csv("/Users/kyogia/Desktop/analisis/Lab2/wine.data", header=TRUE, sep=",")
remove <- c()


for(j in 2:14)
{
    rows <- whole_data[,j]
    IQR <- summary(whole_data[,j])[5] - summary(whole_data[,j])[2]
    rangeSup <- summary(whole_data[,j])[5] + 1.5*IQR
    rangeInf <- summary(whole_data[,j])[2] - 1.5*IQR
    min <- summary(whole_data[,j])[1]
    max <- summary(whole_data[,j])[6]

    #jpeg(paste("Lab2/img/outliers/", names(whole_data)[j], "-before", ".jpeg", sep=""))
    #plot(whole_data[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(whole_data)[j])
    #abline(h = rangeSup, col="red")
    #abline(h = rangeInf, col="red")
    #dev.off()
       
    for(i in 1:length(whole_data[,j]))
    {
        if (whole_data[i,j] < rangeInf | whole_data[i,j] > rangeSup)
        {
            remove <- c(remove, i)
        }
    }
}

data <- whole_data[-remove,]
data$Class.identifier <- NULL

#for(j in 2:14)
#{
#    rows <- data[,j]
#    IQR <- summary(whole_data[,j])[5] - summary(whole_data[,j])[2]
#    rangeSup <- summary(whole_data[,j])[5] + 1.5*IQR
#    rangeInf <- summary(whole_data[,j])[2] - 1.5*IQR
#    min <- summary(whole_data[,j])[1]
#    max <- summary(whole_data[,j])[6]
#    
#    jpeg(paste("Lab2/img/outliers/", names(whole_data)[j], "-after", ".jpeg", sep=""))
#    plot(data[,j], ylim=c(min, max), xlab="Dato", ylab="Valor", main=names(whole_data)[j])
#    abline(h = rangeSup, col="red")
#    abline(h = rangeInf, col="red")
#    dev.off()
#}

#data_dist <- daisy(data, metric="manhattan")


sil <- c()
for (i in 2:length(data$Alcohol)-1)
{
  fit <- pam(data, diss = FALSE, k = i)
  sil[i] <- fit$silinfo$avg.width
}


jpeg("img/K-Graph.jpeg")
plot(sil)
dev.off()

km_clusters <- pam(data, diss = FALSE, k = 2)

jpeg("img/kmeans.jpeg")
plot(fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Resultados clustering K-means") + 
        theme_bw()
)
dev.off()

fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + labs(title = "Resultados clustering K-means") + theme_bw()