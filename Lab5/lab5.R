library(C50)
#Se recogen los datos
data <- read.csv("wine.data", header=TRUE, sep=",")
# Se deja la clase como variable categorica
data$Class.identifier = as.factor(data$Class.identifier)

# OBTENCIÓN ARBOL DE DECISIÓN #

data_tree <- subset(data, select=-Class.identifier)

model <- C5.0(data_tree, data$Class.identifier)

jpeg("img/tree.jpeg")
plot(model)
dev.off()