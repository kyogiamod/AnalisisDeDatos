source("saveHistGraph.R")
source("saveFreqGraph.R")
source("saveRelationsGraph.R")
data <- read.csv("wine.data", header=TRUE, sep=",")

summary(data)
saveHistGraph(data)
saveFreqGraph(data)
#saveRelationsGraph(data)

#varianza, covarianza
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


#Se crean los graficos de los histogramas

correlacion <- round(cor(data), digits=2)
covarianza <- round(cov(data), digits=2)
