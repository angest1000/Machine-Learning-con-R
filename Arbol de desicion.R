
#Árbol de desicion con R




#En el siguiente ejemplo se clasificará a algunos clientes 
#de una compañia de telefonía móvil para 
#saber si son propensos a cancelar el cotrato o no


library(rpart)          #Algoritmo de arbol de desición
#install.packages(rpart.plot)
library(rpart.plot)     #Grafica el árbol de desicion
#install.packages(c50)
library(C50)            #Contiene el dataset
data(churn)             #Dataset para el ejemplo



#Uniremos los datasets que nos arrojó la linea anterior

churn <- rbind(churnTest,churnTrain)
rm(churnTest,churnTrain)    #Elimina los dataframes que ya no se necesitan
head(churn)

#Seleccionamos las columnas que nos serán de utilidad

churn <- churn[,c(4,7,8,16,19,17,20)]

#cambiamos los nombres
names(churn) <- c("Plan Internacional", "Minutos/Dia","Llamadas/dia",
                  "Minutos internacionales","Reclamaciones","Llamadas internacionales",
                  "Cancelacion")

head(churn)

#Creamos set de entrenamiento y test

#Creamos el vector que nos ayudará a particionar nuestros datos
ind <- sample(2,nrow(churn),replace=TRUE,prob=c(0.6,0.4)) #60% train, 40% test

#Particionamos el dataset para el train y test
train <- churn[ind==1,]
test <- churn[ind==2,]

head(train)
#Creamos el algoritmo del arbol de desición

Arbol <- rpart(Cancelacion~.,method = "class", data = train)

#Imprimimos el árbol

print(Arbol)
rpart.plot(Arbol,extra=4)

#Checamos esadisticas para podar el árbol, cuando se incremente el error

printcp(Arbol)             # estadísticas de resultados
plotcp(Arbol)              # evolución del error a medida que se incrementan los nodos


# Podamos el árbol

#Modo automático
#Se poda donde se encuentre el mínimo error
ArbolPodado<- prune(Arbol, 
                    cp= Arbol$cptable[which.min(Arbol$cptable[,"xerror"]),"CP"])

#Modo manual
#Podamos a partir del cp dónde encontramos el menor error, en este caso
#en el cp 0.016092 
ArbolPodado<- prune(Arbol, cp= 0.016092)  
printcp(ArbolPodado)


#Pronóstico de cancelaciones

testPred <- predict(Arbol, newdata = test, type = "class")

# Visualizamos una matriz de confusión
table(testPred, test$Cancelacion)

#Veamos el porcentaje de acierto del modelo
sum(testPred == test$Cancelacion) / length(test$Cancelacion)*100








