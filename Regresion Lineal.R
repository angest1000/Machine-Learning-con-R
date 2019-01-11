#Regresion lineal simple

#Código extraido del siguiente enlace:
# https://www.youtube.com/watch?v=CLKPK2JN1XE&t=0s&index=28&list=WL

#importamos datos
library(readr)
kc_house_data <- read_csv("Documentos/Scripts R/kc_house_data.csv")
View(kc_house_data)

#attach Nos permite acceder a las variables simples de nuestro
#Dataset sin mencionar la base de datos previamente
attach(kc_house_data)

#Generemos un grafico de correlaciones entre las 
#Variables de la 3 a la 6 (precio, recamaras, baños, metros cuadrados)
pairs(kc_house_data[,3:6])

#Notemos del grafico anterior que en la esquina superior derecha
#Se aprecia que existe una correlación entre el precio como 
#Variable de respuesta y los metros cuadrados como variable independiente

cor(sqft_living,price)
# La correlación entre ellas es de 0.702

#Creamos nuestro modelo de regresión
#y=f(x):= precio
#x:= pies cuadrados (sqft)
modelo <- lm(price ~ sqft_living)

#Grafiquemos nuestro modelo
plot(sqft_living,price,col="blue",xlab = "Superficie (ft^2)",ylab = "Precio",main = "Regresion Lineal")
abline(modelo, col="red")

#Veamos la información de nuestro modelo
summary(modelo)
#La formula de la recta es y=mx+b
#En la salida (Intercept) sería b
# y sqft_living sería m

#Para que el modelo sea útil, el p-value debe ser menor a nuestro nivel de significancia
#es decir, en el caso de que se utilice un grado de 95% el p-value deberá ser
#menor a 0.05. En este caso el p-value = 2.2e-16 lo cual es mucho menor a 0.05

#La R² nos indica que el 49% de nuestros datos pueden ser explicados 
#Por nuestro modelo


#Hagamos un pronostico
predict(modelo,data.frame(sqft_living=2000))

#Creemos un intervalo de confianza para dicha prediccion
predict(modelo,data.frame(sqft_living=2000),level = 0.90, interval = "prediction")

#notemos que el intervalo de confianza es demasiado amplio

boxplot(sqft_living)
#En la grafica anterior, podemos notar que después del cuarto cuartil
#Existen demasiados datos, por ello tenemos un intervalo demasiado amplio


#Ahora creemos un dataframe con los valores reales, los valores de nuestro modelo
#y las diferencias 
resultados <- data.frame(price,sqft_living,modelo$fitted.values, round(price - modelo$fitted.values))
names(resultados) <- c("Precio","Pies^2","Estimado","Diferencia")
View(resultados)
