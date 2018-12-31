library("arules")
library("arulesViz")

data <- read.csv("wine.data", header=TRUE, sep=",")


alcohol <-  logical(length(data[[1]]))
malic <- logical(length(data[[1]]))
ash <- logical(length(data[[1]]))
alkalinity <- logical(length(data[[1]]))
magnesium <- logical(length(data[[1]]))
total_phenols <- logical(length(data[[1]]))
flavanoids <- logical(length(data[[1]]))
nonflavanoids <-  logical(length(data[[1]]))
proan <- logical(length(data[[1]]))
color <- logical(length(data[[1]]))
hue <- logical(length(data[[1]]))
OD280 <- logical(length(data[[1]]))
proline <- logical(length(data[[1]]))

for(i in 1:length(data[[1]]))
{
    #Alcohol
    if(data[i,2] < ((summary(data[,2])[6] - summary(data[,2])[1] )/2) ) {
        alcohol[i] <- TRUE
    } else {
        alcohol[i] <- FALSE
    }

    #Malic acid
    if(data[i,3] < ((summary(data[,3])[6] - summary(data[,3])[1] )/2) ) {
        malic[i] <- TRUE
    } else {
        malic[i] <- FALSE
    }
    
    #Ash
    if(data[i,4] < ((summary(data[,4])[6] - summary(data[,4])[1] )/2) ) {
        ash[i] <- TRUE
    } else {
        ash[i] <- FALSE
    }
    
    #Alkalinity of ash
    if(data[i,5] < ((summary(data[,5])[6] - summary(data[,6])[1] )/2) ) {
        alkalinity[i] <- TRUE
    } else {
        alkalinity[i] <- FALSE
    }
    
    #Magnesium
    if(data[i,6] < ((summary(data[,6])[6] - summary(data[,6])[1] )/2) ) {
        magnesium[i] <- TRUE
    } else {
        magnesium[i] <- FALSE
    }
    
    #Total phenols
    if(data[i,7] < ((summary(data[,7])[6] - summary(data[,7])[1] )/2) ) {
        total_phenols[i] <- TRUE
    } else {
        total_phenols[i] <- FALSE
    }
    
    #Flavanoids
    if(data[i,8] < ((summary(data[,8])[6] - summary(data[,8])[1] )/2) ) {
        flavanoids[i] <- TRUE
    } else {
        flavanoids[i] <- FALSE
    }
    
    #Nonflavanoid phenols
    if(data[i,9] < ((summary(data[,9])[6] - summary(data[,9])[1] )/2) ) {
        nonflavanoids[i] <- TRUE
    } else {
        nonflavanoids[i] <- FALSE
    }
    
    #Proanthocyanidins
    if(data[i,10] < ((summary(data[,10])[6] - summary(data[,10])[1] )/2) ) {
        proan[i] <- TRUE
    } else {
        proan[i] <- FALSE
    }
    
    #Color intensity
    if(data[i,11] < ((summary(data[,11])[6] - summary(data[,11])[1] )/2) ) {
        color[i] <- TRUE
    } else {
        color[i] <- FALSE
    }
    
    #Hue
    if(data[i,12] < ((summary(data[,12])[6] - summary(data[,12])[1] )/2) ) {
        hue[i] <- TRUE
    } else {
        hue[i] <- FALSE
    }
    
    #OD280/OD315 of diluted wines
    if(data[i,13] < ((summary(data[,13])[6] - summary(data[,13])[1] )/2) ) {
        OD280[i] <- TRUE
    } else {
        OD280[i] <- FALSE
    }
    
    #Proline      
    if(data[i,14] < ((summary(data[,14])[6] - summary(data[,14])[1] )/2) ) {
        proline[i] <- TRUE
    } else {
        proline[i] <- FALSE
    }
}

#stop()

data$Class.identifier <- as.factor(data$Class.identifier)
data$Alcohol <- as.factor(alcohol)
data$Malic.acid <- as.factor(malic)
data$Ash <- as.factor(ash)
data$Alkalinity.of.ash <- as.factor(alkalinity)
data$Magnesium <- as.factor(magnesium)
data$Total.phenols <- as.factor(total_phenols)
data$Flavanoids <- as.factor(flavanoids)
data$Nonflavanoid.phenols <- as.factor(nonflavanoids)
data$Proanthocyanidins <- as.factor(proan)
data$Color.intensity <- as.factor(color)
data$Hue <- as.factor(hue)
data$OD280.OD315.of.diluted.wines <- as.factor(OD280)
data$Proline <- as.factor(proline)

levels(data$Class.identifier) <- factor(c("1", "2", "3"))

rules <- apriori(as(data, "transactions"), parameter = list(minlen=2, support=0.01, confidence=0.5, maxlen=5))
rules.sop <- sort(rules, by="support")
rules.conf <- sort(rules, by="confidence")
rules.lift <- sort(rules, by="lift")

write(rules, file="AsotiationRulesLogical.csv", sep=",", quote=TRUE, row.names=FALSE)

jpeg("img/matrixGroupedLogical.jpeg")
plot(rules, method="grouped")
dev.off()
jpeg("img/headMatrixGroupedLogical.jpeg")
plot(head(rules, 100), method="grouped")
dev.off()
jpeg("img/TransactionsLogical.jpeg")
image(as(data, "transactions"), xlab="Items (columnas)", ylab="Transacciones (Filas)")
dev.off()
jpeg("img/TwoKeyPlotLogical.jpeg")
plot(rules, method="two-key plot")
dev.off()

rules.sop <- sort(rules, by="support")
rules.conf <- sort(rules, by="confidence")
rules.lift <- sort(rules, by="lift")

write(head(rules.sop, 15), file="LogicalRulesSop.csv", sep=" ", row.names=FALSE)
write(head(rules.conf, 15), file="LogicalRulesConf.csv", sep=" ", row.names=FALSE)
write(head(rules.lift, 15), file="LogicalRulesLift.csv", sep=" ", row.names=FALSE)