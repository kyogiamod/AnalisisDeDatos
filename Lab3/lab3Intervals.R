library("arules")
library("arulesViz")

data <- read.csv("wine.data", header=TRUE, sep=",")

#OBTENCIÓN DE REGLAS

# Transformación de datos y inicialización en con ceros todas las variables
#   - Alcohol 
#       - Bajo: [11.03, 12.29]
#       - Medio: [12.30, 13.56]
#       - Alto: [13.57, 14.83]
alcoholBajo <- integer(length(data[[1]]))
alcoholMedio <- integer(length(data[[1]]))
alcoholAlto <- integer(length(data[[1]]))

#   - Malic acid
#       - Bajo: [0.740, 2.426]
#       - Medio: [2.427, 4.113]
#       - Alto: [4.114, 5.800]
malicBajo <- integer(length(data[[1]]))
malicMedio <- integer(length(data[[1]]))
malicAlto <- integer(length(data[[1]]))

#   - Ash
#       - Bajo: [1.360, 1.983]
#       - Medio: [1.984, 2.606]
#       - Alto: [2.607, 3.230]
ashBajo <- integer(length(data[[1]]))
ashMedio <- integer(length(data[[1]]))
ashAlto <- integer(length(data[[1]]))

#   - Alkanility of Ash
#       - Bajo: [10.600 - 17.066]
#       - Medio: [17.07 - 23.533]
#       - Alto: [23.54 - 30.000]
alkalinityBajo <- integer(length(data[[1]]))
alkalinityMedio <- integer(length(data[[1]]))
alkalinityAlto <- integer(length(data[[1]]))

#   - Magnesium
#       - Bajo: [70.00, 100.66]
#       - Medio: [100.67, 131.33]
#       - Alto: [131.34, 162.00]
magnesiumBajo <- integer(length(data[[1]]))
magnesiumMedio <- integer(length(data[[1]]))
magnesiumAlto <- integer(length(data[[1]]))

#   - Total phenols
#       - Bajo: [0.98 - 1.946]
#       - Medio: [1.947 - 2.913]
#       - Alto: [2.914 - 2.88]
total_phenolsBajo <- integer(length(data[[1]]))
total_phenolsMedio <- integer(length(data[[1]]))
total_phenolsAlto <- integer(length(data[[1]]))

#   - Flavanoids
#       - Bajo: [0.34 - 1.92]
#       - Medio: [1.93 - 3.50]
#       - Alto: [3.51 - 2.08]
flavanoidsBajo <- integer(length(data[[1]]))
flavanoidsMedio <- integer(length(data[[1]]))
flavanoidsAlto <- integer(length(data[[1]]))

#   - Nonflavanoid phenols
#       - Bajo: [0.13 - 0.306]
#       - Medio: [0.307 - 0.483]
#       - Alto: [0.484 - 0.66]
nonflavanoidsBajo <- integer(length(data[[1]]))
nonflavanoidsMedio <- integer(length(data[[1]]))
nonflavanoidsAlto <- integer(length(data[[1]]))

#   - Proanthocyanidins
#       - Bajo: [0.41 - 1.466]
#       - Medio: [1.467 - 2.523]
#       - Alto: [2.524 - 3.580]
proanBajo <- integer(length(data[[1]]))
proanMedio <- integer(length(data[[1]]))
proanAlto <- integer(length(data[[1]]))

#   - Color intensity
#       - Bajo: [1.28 - 5.186]
#       - Medio: [5.187 - 9.093]
#       - Alto: [9.094 - 13]
colorBajo <- integer(length(data[[1]]))
colorMedio <- integer(length(data[[1]]))
colorAlto <- integer(length(data[[1]]))

#   - Hue
#       - Bajo: [0.48 - 0.89]
#       - Medio: [0.90 - 1.30]
#       - Alto: [0.31 - 1.71]
hueBajo <- integer(length(data[[1]]))
hueMedio <- integer(length(data[[1]]))
hueAlto <- integer(length(data[[1]]))

#   - OD280/OD315 of diluted wines
#       - Bajo: [1.27 - 2.18]
#       - Medio: [2.19 - 3.09]
#       - Alto: [3.10 - 4.00]
OD280Bajo <- integer(length(data[[1]]))
OD280Medio <- integer(length(data[[1]]))
OD280Alto <- integer(length(data[[1]]))

#   - Proline
#       - Bajo: [278 - 745.333]
#       - Medio: [745.334 - 1212.666]
#       - Alto: [1212.667 - 1680]
prolineBajo <- integer(length(data[[1]]))
prolineMedio <- integer(length(data[[1]]))
prolineAlto <- integer(length(data[[1]]))

for(i in 1:length(data[[1]]))
{
    print(i)
    #Alcohol
    if(data[i,2] < 12.29) {
        alcoholBajo[i] <- 1
    } else if (data[i,2] < 13.56) {
        alcoholMedio[i] <- 1
    } else {
        alcoholAlto[i] <- 1
    }

    #Malic acid
    if(data[i,3] < 2.426) {
        malicBajo[i] <- 1
    } else if (data[i,3] < 4.113) {
        malicMedio[i] <- 1
    } else {
        malicAlto[i] <- 1
    }
    
    #Ash
    if(data[i,4] < 1.983) {
        ashBajo[i] <- 1
    } else if (data[i,4] < 2.606) {
        ashMedio[i] <- 1
    } else {
        ashAlto[i] <- 1
    }
    
    #Alkalinity of ash
    if(data[i,5] < 17.066) {
        alkalinityBajo[i] <- 1
    } else if (data[i,5] < 23.533) {
        alkalinityMedio[i] <- 1
    } else {
        alkalinityAlto[i] <- 1
    }
    
    #Magnesium
    if(data[i,6] < 100.66) {
        magnesiumBajo[i] <- 1
    } else if (data[i,6] < 131.33) {
        magnesiumMedio[i] <- 1
    } else {
        magnesiumAlto[i] <- 1
    }
    
    #Total phenols
    if(data[i,7] < 1.946) {
        total_phenolsBajo[i] <- 1
    } else if (data[i,7] < 2.913) {
        total_phenolsMedio[i] <- 1
    } else {
        total_phenolsAlto[i] <- 1
    }
    
    #Flavanoids
    if(data[i,8] < 1.92) {
        flavanoidsBajo[i] <- 1
    } else if (data[i,8] < 3.50) {
        flavanoidsMedio[i] <- 1
    } else {
        flavanoidsAlto[i] <- 1
    }
    
    #Nonflavanoid phenols
    if(data[i,9] < 0.306) {
        nonflavanoidsBajo[i] <- 1
    } else if (data[i,9] < 0.483) {
        nonflavanoidsMedio[i] <- 1
    } else {
        nonflavanoidsAlto[i] <- 1
    }
    
    #Proanthocyanidins
    if(data[i,10] < 1.466) {
        proanBajo[i] <- 1
    } else if (data[i,10] < 2.523) {
        proanMedio[i] <- 1
    } else {
        proanAlto[i] <- 1
    }
    
    #Color intensity
    if(data[i,11] < 5.186) {
        colorBajo[i] <- 1
    } else if (data[i,11] < 9.093) {
        colorMedio[i] <- 1
    } else {
        colorAlto[i] <- 1
    }
    
    #Hue
    if(data[i,12] < 0.89) {
        hueBajo[i] <- 1
    } else if (data[i,12] < 1.30) {
        hueMedio[i] <- 1
    } else {
        hueAlto[i] <- 1
    }
    
    #OD280/OD315 of diluted wines
    if(data[i,13] < 2.18) {
        OD280Bajo[i] <- 1
    } else if (data[i,13] < 3.09) {
        OD280Medio[i] <- 1
    } else {
        OD280Alto[i] <- 1
    }
    
    #Proline      
    if(data[i,14] < 745.333) {
        prolineBajo[i] <- 1
    } else if (data[i,14] < 1212.666) {
        prolineMedio[i] <- 1
    } else {
        prolineAlto[i] <- 1
    }
}

#stop()

data$Class.identifier <- as.factor(data$Class.identifier)

data$Alcohol <- NULL
data$Alcohol.bajo <- as.factor(alcoholBajo)
data$Alcohol.medio <- as.factor(alcoholMedio)
data$Alcohol.alto <- as.factor(alcoholAlto)

data$Malic.acid <- NULL
data$Malic.bajo <- as.factor(malicBajo)
data$Malic.medio <- as.factor(malicMedio)
data$Malic.alto <- as.factor(malicAlto)

data$Ash <- NULL
data$Ash.bajo <- as.factor(ashBajo)
data$Ash.medio <- as.factor(ashMedio)
data$Ash.alto <- as.factor(ashAlto)

data$Alkalinity.of.ash <- NULL
data$Alkalinity.bajo <- as.factor( alkalinityBajo)
data$Alkalinity.medio <- as.factor( alkalinityMedio)
data$Alkalinity.alto <- as.factor( alkalinityAlto)

data$Magnesium <- NULL
data$Magnesium.bajo <- as.factor(magnesiumBajo)
data$Magnesium.medio <- as.factor(magnesiumMedio)
data$Magnesium.alto <- as.factor(magnesiumAlto)

data$Total.phenols <- NULL
data$Total.phenols.bajo <- as.factor(total_phenolsBajo)
data$Total.phenols.medio <- as.factor(total_phenolsMedio)
data$Total.phenols.alto <- as.factor(total_phenolsAlto)

data$Flavanoids <- NULL
data$Flavanoids.bajo <- as.factor(flavanoidsBajo)
data$Flavanoids.medio <- as.factor(flavanoidsMedio)
data$Flavanoids.alto <- as.factor(flavanoidsAlto)

data$Nonflavanoid.phenols <- NULL
data$nonFlavanoids.bajo <- as.factor(nonflavanoidsBajo)
data$nonFlavanoids.medio <- as.factor(nonflavanoidsMedio)
data$nonFlavanoids.alto <- as.factor(nonflavanoidsAlto)

data$Proanthocyanidins <- NULL
data$Proanthocyanidins.bajo <- as.factor(proanBajo)
data$Proanthocyanidins.medio <- as.factor(proanMedio)
data$Proanthocyanidins.alto <- as.factor(proanAlto)

data$Color.intensity <- NULL
data$Color.bajo <- as.factor(colorBajo)
data$Color.medio <- as.factor(colorMedio)
data$Color.alto <- as.factor(colorAlto)

data$Hue <- NULL
data$Hue.bajo <- as.factor(hueBajo)
data$Hue.medio <- as.factor(hueMedio)
data$Hue.alto <- as.factor(hueAlto)

data$OD280.OD315.of.diluted.wines <- NULL
data$OD280.bajo <- as.factor(OD280Bajo)
data$OD280.medio <- as.factor(OD280Medio)
data$OD280.alto <- as.factor(OD280Alto)

data$Proline <- NULL
data$Proline.bajo <- as.factor(prolineBajo)
data$Proline.medio <- as.factor(prolineMedio)
data$Proline.alto <- as.factor(prolineAlto)

levels(data$Class.identifier) <- factor(c("1", "2", "3"))

rules <- apriori(as(data, "transactions"), parameter = list(minlen=2, support=0.01, confidence=0.5, maxlen=4))
#rules.sop <- sort(rules, by="support")
#rules.conf <- sort(rules, by="confidence")
#rules.lift <- sort(rules, by="lift")

#write(rules, file="AsotiationRulesIntervals.csv", sep=",", quote=TRUE, row.names=FALSE)

jpeg("img/matrixGroupedIntervals.jpeg")
plot(rules, method="grouped")
dev.off()
jpeg("img/headMatrixGroupedIntervals.jpeg")
plot(head(rules, 100), method="grouped")
dev.off()
jpeg("img/TransactionsIntervals.jpeg")
image(as(data, "transactions"), xlab="Items (columnas)", ylab="Transacciones (Filas)")
dev.off()
