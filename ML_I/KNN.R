###############
###   KNN   ###
###############

library(dplyr)
library(MASS)
library(class) # paquete de la funcion knn
library(caret)
library(ggplot2)
# install.packages("distances")
library(distances)

# reproductivilidad
set.seed(123456)

### DATOS BOSTON
Boston
str(Boston)

### OTROS DATOS
library(mlbench)
data(BreastCancer)
data(Glass)
data(Ionosphere)
data(PimaIndiansDiabetes)


#### MUESTREO
numero_total = nrow(Boston)

# Porcentajes de train, test y validation
w_train = .5
w_test = .25
w_validation = 1 - (w_train + w_test)

# Todos los índices
indices = seq(1:numero_total) 

# Muestreo
indices_train = sample(1:numero_total, numero_total * w_train)
indices_test = sample(indices[-indices_train], numero_total * w_test)
indices_validation = indices[-c(indices_train,indices_test)]

# Agrupamos
Boston_train = Boston[indices_train,]
Boston_test = Boston[indices_test,]
Boston_validation = Boston[indices_validation,]


### TRANSFORMACION DE VARIABLES
# discretizamos la variable respuesta precio para tener clasificacion
Boston_train=
  Boston_train %>%
  mutate(sq_lstat = scale(sqrt(lstat)), log_crim = scale(log(crim)), precio = medv>=30)


Boston_test=
  Boston_test %>%
  mutate(sq_lstat = scale(sqrt(lstat)), log_crim = scale(log(crim)), precio = medv>=30)


### KNN CON K=1
prediccion_knn1 =knn(Boston_train[,c("sq_lstat","log_crim")], Boston_test[,c("sq_lstat","log_crim")], 
                     k=1, cl=Boston_train$precio)

table(prediccion_knn1,Boston_test$precio)

# Medidas de precisión
accuracy = sum(prediccion_knn1 == Boston_test$precio) /nrow(Boston_test)
error = 1-accuracy

# Acierto sobre el total de las casas CARAS, sensitivity o recall
sensitivity = sum(prediccion_knn1 == Boston_test$precio & Boston_test$precio == TRUE) / sum(Boston_test$precio == TRUE)
recall = sensitivity

# Acierto sobre el total de las casas BARATAS
specificity =  sum(prediccion_knn1 == Boston_test$precio & Boston_test$precio == FALSE) / sum(Boston_test$precio == FALSE)

# Acierto cuando el predicho es CARO
precision = sum(prediccion_knn1 == Boston_test$precio & prediccion_knn1 == TRUE) / sum(prediccion_knn1 == TRUE)

# Acierto cuando el predicho es BARATO
npv = sum(prediccion_knn1 == Boston_test$precio & prediccion_knn1 == FALSE) / sum(prediccion_knn1 == FALSE)

# F1_score
f1score = 2*precision*recall /(precision+recall)
c(accuracy = accuracy, error = error, sensitivity = sensitivity, specificity = specificity, precision = precision, npv = npv, f1=f1score)


## funcion para medidas de rendimiento
confusionMatrix(table(prediccion_knn1,Boston_test$precio), positive="TRUE",mode = "prec_recall")



### KNN CON K=3
prediccion_knn3 =knn(Boston_train[,c("sq_lstat","log_crim")], Boston_test[,c("sq_lstat","log_crim")],
                     k=3, cl=Boston_train$precio)

table(prediccion_knn3,Boston_test$precio)

# Medidas de rendimiento
confusionMatrix(table(prediccion_knn3,Boston_test$precio), positive="TRUE",mode = "prec_recall")



### KNN CON CROSS VALIDATION leave-one-out cross validation. 
prediccion_knn_cv =knn.cv(Boston_train[,c("sq_lstat","log_crim")], k=1, cl=Boston_train$precio)
table(prediccion_knn_cv,Boston_train$precio)
confusionMatrix(table(prediccion_knn_cv,Boston_train$precio), positive="TRUE",mode = "prec_recall")



### GRID SEARCH DE K PARA KNN CON CROSS VALIDATION leave-one-out cross validation. 
long = 15
accuracy = rep(0,long)
f1score = rep(0,long)
recall = rep(0,long)
precision = rep(0,long)
for (i in 1:long){
  prediccion_knn_cv =knn.cv(Boston_train[,c("sq_lstat","log_crim")], 
                            k=i, cl=Boston_train$precio)
  accuracy[i] = sum(prediccion_knn_cv == Boston_train$precio) /nrow(Boston_train)
  recall[i] = sum(prediccion_knn_cv == Boston_train$precio & Boston_train$precio == TRUE) / sum(Boston_train$precio == TRUE)
  precision[i] = sum(prediccion_knn_cv == Boston_train$precio & prediccion_knn_cv == TRUE) / sum(prediccion_knn_cv == TRUE)
  f1score[i] = 2*precision[i]*recall[i]/(precision[i]+recall[i])
}
resultados_knn = as.data.frame(cbind(accuracy,f1score,precision,recall))
resultados_knn = resultados_knn %>% mutate(index=as.factor(seq(1:long)))

max(resultados_knn$f1score)

which.max(resultados_knn$f1score)

ggplot(data=resultados_knn,aes(x=index,y=accuracy)) + 
  geom_col(colour="cyan4",fill="cyan3")+
  ggtitle("Accuracy")

ggplot(data=resultados_knn,aes(x=index,y=f1score)) + 
  geom_col(colour="orange2",fill="orange") +
  ggtitle("F1_score values")


## RESULTADOS CON NUMERO OPTIMO

# En train
prediccion_knn5_train =knn.cv(Boston_train[,c("sq_lstat","log_crim")], 
                              k=5, cl=Boston_train$precio)
confusionMatrix(table(prediccion_knn5_train,Boston_train$precio), positive="TRUE",mode = "prec_recall")

#En test
prediccion_knn5_test=knn(Boston_train[,c("sq_lstat","log_crim")], Boston_test[,c("sq_lstat","log_crim")],
                         k=5, cl=Boston_train$precio)
confusionMatrix(table(prediccion_knn5_test,Boston_test$precio), positive="TRUE",mode = "prec_recall")


### NUESTRA PROPIA MATRIZ DE DISTANCIA
peso = seq(0.05,0.95,.05)
f1score = rep(0,length(peso))
for (j in 1:length(peso)){
  distancia_train = as.matrix(distances(Boston_train,
                                        dist_variables=c("sq_lstat","log_crim"),
                                        weights=c(peso[j],1-peso[j])))
  n_train = length(indices_train)
  k = 3
  predicho = rep("FALSE",n_train)
  
  for (i in 1:n_train){
    predicho[i] = 
      sum(Boston_train$precio[sort(distancia_train[i,],index.return=TRUE)$ix[2:(k+1)]])  > floor(k/2)
  }
  confusionMatrix(table(predicho,Boston_train$precio), positive="TRUE")
  recall = 
    sum(predicho == Boston_train$precio & Boston_train$precio == TRUE) / sum(Boston_train$precio == TRUE)
  precision = 
    sum(predicho == Boston_train$precio & predicho == TRUE) / sum(predicho== TRUE)
  f1score[j] = 2*precision*recall/(precision+recall)
}
resultados = as.data.frame(cbind(peso,f1score))
ggplot(data=resultados,aes(x=peso,y=f1score)) + 
  geom_point(colour="orange4",fill="orange3") +
  geom_line() +
  ggtitle("F1_score, peso de la variable sq_lstat respecto a log_crim") +
  geom_vline(aes(xintercept=peso[which.max(f1score)]),col=2,lwd=2)

# peso con f1 maximo
peso[which.max(f1score)]

# Para TEST
distancia_train_test = as.matrix(distances(rbind(Boston_train,Boston_test),
                                           dist_variables=c("sq_lstat","log_crim"),
                                           weights=c(peso[which.max(f1score)],1-peso[which.max(f1score)])))
n_test = length(indices_test)
distancia_test = distancia_train_test[(n_train+1):(n_train+n_test),(1:n_train)]
predicho = rep("FALSE",n_test)

for (i in 1:n_test){
  predicho[i] = sum(Boston_train$precio[sort(distancia_test[i,],index.return=TRUE)$ix[2:(k+1)]])  > floor(k/2)
}
confusionMatrix(table(predicho,Boston_test$precio), positive="TRUE")

recall = sum(predicho == Boston_test$precio & Boston_test$precio == TRUE) / sum(Boston_test$precio == TRUE)
precision = sum(predicho == Boston_test$precio & predicho == TRUE) / sum(predicho== TRUE)
f1score = 2*precision*recall/(precision+recall)
f1score
## para obtener F1
confusionMatrix(table(predicho,Boston_test$precio), positive="TRUE",mode = "prec_recall")


