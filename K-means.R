#Metodo de K-means en R

#Que es outliers???

#COMO SE EVALUA EL MODELO:

#1. Inercia total (En R: $Totss)
#2. Inercia entre grupos (En R: $betweenss)
#3. Inercia dentro de los grupos (En R: $withinss, $tot.withinss)

#Inercia total = inercia entre grupos + inercia dentro de los grupos

#En nuestro ejemplo tenemos los datos de una aseguradora

#Queremos responer algunas preguntas sobre que tarifa se debe aplicar a cada
#grupo, o que tipo de marketing requiere cada cliente


#1. Preparamos el set de datos

library(readr)
insurance <- read_csv("Descargas/insurance.csv")
View(insurance)

#Escalando (Normalizando) los datos
#Tomamos solo las variables cuantitativas
insurance.scale <- as.data.frame(scale(insurance[,5:9]))
View(insurance.scale)


#2. Creando los clusters
#Fijamos la semilla
set.seed(80)

#Aplicamos Kmeans para crear los Clusters
insurance.km <- kmeans(insurance.scale, centers = 4)

#Veamos el contenido del objeto
names(insurance.km)

#Asignacion de observaciones a clusters
insurance.km$cluster
#Inercia total
insurance.km$totss
#Inercia inter grupos
insurance.km$betweenss
#Inercia Intra grupos
insurance.km$withinss
#Inercia intragrupos total
insurance.km$tot.withinss


#4. Determinar el número de Clusters óptimo

sumbt <- kmeans(insurance.scale, centers = 1)$betweenss

for ( i in 2:10) sumbt[i] <- kmeans(insurance.scale, centers = i)$betweenss

plot(1:10, sumbt, type = "b", xlab = "Número de Clusters", ylab = "Suma de cuadrados inter grupos")

#En la grafica notamos que conforme va aumentando el número de 
#Clusters, va aumentando la inercia inter grupos.
#Notemos que apartir del 4to se disminuye
#Y posteriormente sigue aumentando con menor intensidad,
#Por lo que sería buena idea hacer un corte en 4


#5. Inspeccionando los resultados

#Creemos una grafica con 2 variables pintando los puntos
#De acuerdo con el cluster asignado

plot(insurance$ant_comp, insurance$ant_perm, col=insurance.km$cluster, xlab = "Fidelidad a la compañia", ylab = "Experiencia")

medias <- aggregate(insurance[,5:9], by = list(insurance.km$cluster),mean)
#Notemos en la salida anterior que en el cluster 3 se tienen a los clientes 
#Con el indice mayor de siniestros
View(medias)








