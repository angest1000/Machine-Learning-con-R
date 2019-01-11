#Metodo de Support Vector Machine (SVM o en español Máquina de Soporte Vectorial) en R
#Coódigo extraido del siguiente tutorial:
# https://www.youtube.com/watch?v=ImhjKyc88x0


#La máquina de soporte vectorial es un algoritmo supervisado 
#que busca el hiperplano que mejor separe (o mas) tipos de datos.

#En este ejemplo separaremos gatos y gatas con base en algunas de
#sus características.

#Cargamos librería que contiene el algoritmo SVM
#   install.package("e1071")
library(e1071)

#Cargamos el dataset de cats

attach(data(cats,package = "MASS"))

#Contiene 3 variables: Sex(sexo), bwt(Peso),Hwt(Peso del corazón)

#Veamos los datos
plot(cats$Hwt,cats$Bwt,col=cats$Sex,xlab = "Peso del Corazón",ylab = "Peso")

#Crearemos el conjunto de datos de train y test

ind <- sample(2,nrow(cats),replace = TRUE, prob = c(0.7,0.3)) #70% para train y 30% para test

train <- cats[ind==1,] #Entrenamiento
test <- cats[ind==2,] #Prueba

View(train)
View(test)

#Entrenemos el modelo

modelo <- svm(Sex~.,data = train, kernel = "radial") 

summary(modelo)
#valores para kernel
#Kernel = "linear","polynomial", "radial", "sigmoid"

#Veamos como se ve el modelo con nuestros diferentes datasets
plot(modelo,train)
plot(modelo, test)
plot(modelo,cats)

#Creemos un pronóstico
pronostico <- predict(modelo,newdata = test[-1])
pronostico
View(pronostico)

#Creemos la matriz de confusión
MC <- table(test[,1],pronostico)
MC

#Dado que los valores que se encuentran en la diagonal de la 
#Matriz son los valores acertados por el modelo
#Veamos cual es el % de acierto

acierto <- (sum(diag(MC))/sum(MC))*100
acierto
# 78.57% de aciertos

