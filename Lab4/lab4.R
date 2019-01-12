library(e1071)
library(dplyr)
library(caret)

#Se recogen los datos
data <- read.csv("wine.data", header=TRUE, sep=",")
# Se deja la clase como variable categorica
data$Class.identifier = as.factor(data$Class.identifier)
#Se le agrega la variable id para separar el conjunto de entrenamiento del testing
data$ID <- 1:length(data[[1]])

# Dado que se trabaja con probabilidades, se setea el seed para que siempre sea igual
set.seed(0)

# Se obtiene el conjunto de entrenamiento, aprox 70% de todos los datos
training <- data %>% sample_frac(0.7)

# Se obtiene el conjunto de prueba (es el conjunto de datos que no feron utilizados para entrenar).
testing <- data[-c(training$ID),]

# Se elimina la variable ID agregada artificialmente que separaba entre trainig y test
data$ID <- NULL
training$ID <- NULL
testing$ID <- NULL
#
# =========== Obtencion del Clasificador =========
#
# A continuacion, se obtendra el Clasificador Bayesiano Ingenuo a partir del conjunto de datos de entrenamiento, empleando
# la funcion 'naiveBayes()' de la biblioteca 'e1071'.
modelo_bayesiano <- naiveBayes(Class.identifier ~ ., training)

# Se pone a prueba el modelo obtenido con el conjunto de datos de prueba.
prediction <- predict(modelo_bayesiano, testing)


# ========= Resultados ==========
#
# Se obtiene una matriz de confusion respecto a los resultados obtenidos con el clasificador conseguido, para poder
# evaluar la precision del clasificador.
matriz_confusion <- table(testing$Class.identifier, prediction)

cat("=== Matriz de Confusion ===\n\n")
resultados <- confusionMatrix(matriz_confusion)
print(resultados)