#k-fold cross validation

bh<-read.csv("tema 4/r-course-master/data/tema4/BostonHousing.csv")

#validacion cruzada para regresion

kfold.crossval.reg<-function(df,nfolds){
  fold<-sample(1:nfolds,nrow(df),replace=T)
  cat(fold)
  cat(nfolds)
  
  mean.sqr.errs<-sapply(1:nfolds,
                        kfold.cval.reg.iter,
                        df,fold)
  list("MSE"= mean.sqr.errs,
       "Overall_Mean_sqr_error"= mean(mean.sqr.errs),
       "std_mean_sqr_error"=sd(mean.sqr.errs))
}

kfold.cval.reg.iter<-function(k,df,fold){
  
  t.ids<-!fold%in%c(k) #los que no estan en el conjunto de entrenamiento
  test.ids<-fold%in%c(k)
  mod<-lm(MEDV~.,data = df[t.ids,])
  pred<-predict(mod,df[test.ids,])
  sqr.errs<-(pred-df[test.ids,"MEDV"])^2
  mean(sqr.errs)
  
  as.data.frame.data.frame(pred)

}

#hay que tener en cuenta que el error cuadratico de la validación
#cruzada va a ser el promedio de errores de las validaciones hechas,
#es decir, Overall_Mean_sqr_error.


#cross validation con 5 divisiones

res<-kfold.crossval.reg(bh,4)
res
predict<-kfold.cval.reg.iter(1,bh,4)
bh$predicciones = predict$mod$fitted.values
#vemos que cada elemento esta siempre en el conjunto de entrenamiento
#salvo en un caso que esta en el de validación ya que asi trabaja la
#validación cruzada.

#La primera función se usa para crear aleatoriamente un muestreo desde 1 hasta el numero
# de divisones que se quieren llevar a cabo, es decir, luego se tienen tantos subconjuntos
#como divisiones del conjunto original.


#La segunda función hace un modelo lineal muy completo porque se usa
#la variable del propio df , se calculan lo errores cuadraticos,y otros valores


#AQUI ME HACE FALTA AGREGAR LAS PREDICCIONES AL DATAFRAME!!!!1

 ###################################


#IMPLEMENTACIÓN DE LOOCV PARA REGRESIÓN 

#la idea es tener una buena herramienta para la validacion
#cruzada dejando uno afuera en cada caso

#esta lleva a cabo tantas ejecuciones como elementos se tengan. 

loocv.reg <-function(df){
  mean.sqr.errors<-sapply(1:nrow(df),
                          loocv.reg.iter,df)
  
  list("MSE"=mean.sqr.errors,
       "Overall_square_errors"=mean(mean.sqr.errors),
       "sd_mean_sqr_errors"=sd(mean.sqr.errors))
  
}

loocv.reg.iter<-function(k,df){
  mod <- lm(MEDV~.,data = df[-k,])
  pred <- predict(mod,df[k,])
  sqr.error<-(pred-df[k,"MEDV"])^2
  sqr.error
}

res<-loocv.reg(bh)
res

#506 modelos lineales con su error cuadratico 
#y tambien el promedio y desviaciones estandar.

#estos dos metodos son muy muy utiles para evitar el
#overfitting o que el modelo se ajuste demasiado al 
#conjunto de entrenamiento y luego no lo haga al de validación

