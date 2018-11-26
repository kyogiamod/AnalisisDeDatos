source("saveHistGraph.R")
source("saveFreqGraph.R")
data <- read.csv("wine.data", header=TRUE, sep=",")

#summary(data)
saveHistGraph(data)
saveFreqGraph(data)

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


#Se crean los graficos de los histogramas


#names(data) <- NULL
#correlacion <- round(cor(data), digits=2)
#names(correlacion) <- NULL
#correlacion

#covarianza <- round(cov(data), digits=2)
#write.csv(correlacion, "asd.txt", sep="\t")
#test <- c()
#for(i in 1:14)
#{
#    print(shapiro.test(data[,i]))
#}

