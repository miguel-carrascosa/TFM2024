# Modelización y evaluación "Barcos2"

# Cargamos librerías
# library(openxlsx)
library(tidyverse)
library(caret)
library(caretEnsemble)
library(parallel)
library(doParallel)
library(randomForest)
library(randomForestExplainer)
library(reshape)
library(rpart)
library(rpart.plot)
library(vip)

# Cargamos funciones
source("./funciones/cruzadas avnnet y lin.R")
source("./funciones/cruzada arbol continua.R")
source("./funciones/cruzada gbm continua.R")
source("./funciones/cruzada xgboost continua.R")
source("./funciones/cruzada rf continua.R")
source("./funciones/cruzada SVM continua lineal.R")
source("./funciones/cruzada SVM continua polinomial.R")

# Cargamos los datos ya procesados, explorados y modificados
barcos <- read.csv(file="./datos/Barcos2.csv",
                   sep=",")

nombres1 <- barcos |>
  select(-price) |> 
  colnames()
vardep <- "price"
data <- barcos

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

# Definimos los 2 mejores conjuntos de variables (el 1 y el 7)
cvar1 <- c("gt", "length", "beam", "crew", "build_year")
cvar7 <- c("gt", "length", "build_year", "displacement_tonnage",
           "water_capacity", "total_power", "engine_make.Caterpillar",
           "hull_number")



#### TUNEADO DE REDES NEURONALES ####

control<-trainControl(method = "repeatedcv",number=4,repeats=5,
                      savePredictions = "all") 
set.seed(12345)
nnetgrid <-  expand.grid(size=c(2,3,4,5),
                         decay=c(0.2,0.15,0.1,0.05),
                         bag=F)
completo<-data.frame()
listaiter<-c(1700,2000,2300,3000,3500,4000)

for (iter in listaiter){
  rednnet<- train(price~
                    gt+length+beam+crew+build_year,
                  data=data,method="avNNet",linout = TRUE,
                  maxit=iter,trControl=control,repeats=5,
                  tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(itera), y=RMSE, 
                     color=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#### TUNEADO DE ÁRBOLES DE DECISIÓN ####

set.seed(12345)
treegrid <- expand.grid(cp = c(0.005,0.001,0.0001))
control <- trainControl(method="repeatedcv",number=4,repeats=5, 
                        savePredictions="all")
completo <- data.frame()

maxdepth_list <- c(4,5,6,7)
minsplit_list <- c(10,15,20,25)

for (maxdepth in maxdepth_list) {
  for (minsplit in minsplit_list) {
    set.seed(12345)
    tree <- train(price~gt+length+beam+crew+build_year,
                  data=data,method="rpart",
                  trControl=control,tuneGrid=treegrid,
                  control=rpart.control(maxdepth=maxdepth,
                                        minsplit=minsplit))
    # Comprobamos si el entrenamiento tuvo éxito
    if (!any(is.na(tree$results$RMSE))) {
      # Añadimos las columnas de los hiperparámetros
      tree$results$maxdepth <- maxdepth
      tree$results$minsplit <- minsplit
      # Voy incorporando los resultados a completo
      completo <- rbind(completo,tree$results)
    } else {
      warning(paste("Training failed for maxdepth =",maxdepth,
                    "and minsplit =",minsplit))
    }
  }
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(maxdepth), y=RMSE, 
                     color=factor(cp))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#### TUNEADO DE RANDOM FORESTS ####

set.seed(12345)
rfgrid <- expand.grid(mtry=c(3,4,5))
control <- trainControl(method="repeatedcv",number=4,repeats=5, 
                        savePredictions="all")
completo <- data.frame()

ntree_list <- c(50,80,100,120,150,200)
nodesize_list <- c(3,4,5,6,7)
sampsize_list <- c(200,300,400)

for (ntree in ntree_list) {
  for (nodesize in nodesize_list) {
    for (sampsize in sampsize_list) {
      set.seed(12345)
      rf <- train(price~gt+length+beam+crew+build_year,
                  data=data,method="rf",trControl=control,
                  tuneGrid=rfgrid,linout=FALSE,ntree=ntree,
                  nodesize=nodesize,replace=TRUE,importance=TRUE,
                  sampsize=sampsize,repeats=5)
      # Añadimos las columnas de los hiperparámetros
      rf$results$ntree <- ntree
      rf$results$nodesize <- nodesize
      rf$results$sampsize <- sampsize
      # Voy incorporando los resultados a completo
      completo <- rbind(completo, rf$results)
    }
  }
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(ntree), y=RMSE, 
                     color=factor(mtry))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#### TUNEADO DE GRADIENT BOOSTING ####

set.seed(12345)
gbmgrid<-expand.grid(shrinkage=c(0.15,0.1,0.05),
                     n.minobsinnode=c(5,10,15,25,50),
                     n.trees=c(100,300,500,700,1000),
                     interaction.depth=c(3,4,5))

control<-trainControl(method = "repeatedcv",number=4,
                      repeats=5,savePredictions = "all")

gbm <- tryCatch(
  {
    train(price~gt+length+beam+crew+build_year,data=data,
      method="gbm",trControl=control,tuneGrid=gbmgrid,
      distribution="gaussian",bag.fraction=1,verbose=FALSE)
  },
  error = function(e) {
    warning("An error occurred during model training: ",conditionMessage(e))
    return(NULL)
  }
)

res <- gbm$results
res <- res[order(res$RMSE, decreasing = FALSE),]

ggplot(res, aes(x=factor(n.trees), y=RMSE,
                color=factor(shrinkage))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#### TUNEADO DE XGBOOST ####

set.seed(12345)
xgbmgrid<-expand.grid(min_child_weight=c(5,7,9),
                      eta=c(0.05,0.03,0.01),
                      nrounds=c(200,300,400,800,2000),
                      max_depth=c(4,5,6),gamma=c(0,1),
                      colsample_bytree=c(0.7),subsample=c(0.7))

control<-trainControl(method="repeatedcv",number=4,
                      repeats=5,savePredictions="all") 
xgbm <- tryCatch(
  {
    train(price~gt+length+beam+crew+build_year,data=data,
          method="xgbTree",trControl=control,
          tuneGrid=xgbmgrid,verbose=FALSE)
  },
  error = function(e) {
    warning("An error occurred during model training: ",conditionMessage(e))
    return(NULL)
  }
)

res <- xgbm$results
res <- res[order(res$RMSE, decreasing = FALSE),]

ggplot(res, aes(x=factor(nrounds), y=RMSE,
                color=factor(eta))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#### TUNEADO DE SVM LINEAL ####

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,50,100))

control<-trainControl(method="repeatedcv",number=4,repeats=5,
                      savePredictions="all") 

# SVM con conjunto cvar1
SVM<- train(price~gt+length+beam+crew+build_year,data=data,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

# Guardamos resultados
res2 <- res
res2$C <- paste("cvar1, ",res2$C)

# SVM con conjunto cvar7
set.seed(12345)
SVM<- train(price~gt+length+build_year+displacement_tonnage+
              water_capacity+total_power+hull_number+
              engine_make.Caterpillar,data=data,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

res$C <- paste("cvar7, ",res$C)
res <- rbind(res,res2)
res <- res[order(res$RMSE, decreasing = FALSE),]



#### TUNEADO DE SVM POLINÓMICO ####

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,50,100),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method="repeatedcv",number=4,repeats=5,
                      savePredictions="all") 

SVM<- train(price~gt+length+beam+crew+build_year,data=data,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

# Guardamos resultados
res2 <- res
res2$conj <- "cvar1"

# SVM con conjunto cvar7
set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,50,100),
                     degree=c(2),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method="repeatedcv",number=4,repeats=5,
                      savePredictions="all") 

SVM<- train(price~gt+length+build_year+displacement_tonnage+
              water_capacity+total_power+hull_number+
              engine_make.Caterpillar,data=data,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

res$conj <- "cvar7"
res <- rbind(res,res2)
res <- res[order(res$RMSE, decreasing = FALSE),]



#### TUNEADO DE SVM RADIAL ####

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,50,100),
                     sigma=c(0.001,0.01,0.05,0.1,0.2,0.5,1,5,10))

control<-trainControl(method="repeatedcv",number=4,repeats=5,
                      savePredictions="all")  

SVM<- train(price~gt+length+beam+crew+build_year,data=data,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

# Guardamos resultados
res2 <- res
res2$conj <- "cvar1"

# SVM con conjunto cvar7
set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,50,100),
                     sigma=c(0.001,0.01,0.05,0.1,0.2,0.5,1,5,10))

control<-trainControl(method="repeatedcv",number=4,repeats=5,
                      savePredictions="all")  

SVM<- train(price~gt+length+build_year+displacement_tonnage+
              water_capacity+total_power+hull_number+
              engine_make.Caterpillar,data=data,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

plot(SVM)

res <- SVM$results
res <- res[order(res$RMSE, decreasing = FALSE),]

res$conj <- "cvar7"
res <- rbind(res,res2)
res <- res[order(res$RMSE, decreasing = FALSE),]



#### COMPARACIÓN DE MEJORES MODELOS ####

# Cargamos paquetes y funciones
library(caretEnsemble)
source("./funciones/cruzadas avnnet y lin.R")
source("./funciones/cruzada arbol continua.R")
source("./funciones/cruzada gbm continua.R")
source("./funciones/cruzada xgboost continua.R")
source("./funciones/cruzada rf continua.R")
source("./funciones/cruzada SVM continua lineal.R")
source("./funciones/cruzada SVM continua polinomial.R")
source("./funciones/cruzada SVM continua RBF.R")

# Creamos modelos
medias1<-cruzadalin(data=data,
                    vardep=vardep,
                    listconti=cvar7,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=5)

medias1$modelo="Rlin"

medias2<-cruzadaavnnet(data=data,vardep=vardep,
                       listconti=cvar1,listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,size=c(2),
                       decay=c(0.2),repeticiones=5,itera=1700)

medias2$modelo<-"Red"

medias3<-cruzadaarbol(data=data,vardep=vardep,
                      listconti=cvar1,listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,
                      cp=0.001,minbucket=20)

medias3$modelo<-"Arbol"

medias4<-cruzadarf(data=data,vardep=vardep,
                   listconti=cvar1,listclass=c(""),
                   grupos=4,sinicio=1234,repe=5,
                   nodesize=4,replace=TRUE,ntree=200,
                   mtry=4,sampsize=300)

medias4$modelo<-"Rf"

medias5<-cruzadagbm(data=data,vardep=vardep,
                    listconti=cvar1,
                    listclass=c(""),
                    grupos=4,sinicio=1234,repe=5,
                    n.minobsinnode=10,shrinkage=0.1,
                    n.trees=100,interaction.depth=5)

medias5$modelo<-"Gbm"

medias6<-cruzadaxgbm(data=data,vardep=vardep,
                     listconti=cvar1,listclass=c(""),
                     grupos=4,sinicio=1234,repe=5,nrounds=400,
                     min_child_weight=7,eta=0.03,max_depth=4,
                     gamma=0,colsample_bytree=0.7,subsample=0.7)

medias6$modelo<-"Xgbm"

medias7<-cruzadaSVM(data=data,vardep=vardep,
                    listconti=cvar7,listclass=c(""),
                    grupos=4,sinicio=1234,repe=5,C=50)

medias7$modelo<-"SVMlin"

medias8<-cruzadaSVMpoly(data=data,vardep=vardep,
                        listconti=cvar7,listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        C=0.2,degree=2,scale=0.1)

medias8$modelo<-"SVMpoly"

medias9<-cruzadaSVMRBF(data=data,vardep=vardep,
                       listconti=cvar1,listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,
                       C=50,sigma=0.01)

medias9$modelo<-"SVMradial-1"

medias10<-cruzadaSVMRBF(data=data,vardep=vardep,
                        listconti=cvar7,listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        C=10,sigma=0.01)

medias10$modelo<-"SVMradial-7"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,
              medias7,medias8,medias9,medias10)

union1$modelo <- with(union1,reorder(modelo,error,mean))

par(cex.axis=0.8)
boxplot(data=union1,error~modelo,col="pink")



#### ENSAMBLADO ####

# Creamos grids para todos los modelos
stackControl <- trainControl(method="repeatedcv",number=4,
                             repeats=5,savePredictions=TRUE)
rfGrid <- expand.grid(mtry=c(4))
gbmGrid <- expand.grid(n.trees=c(100),interaction.depth=c(5),
                       shrinkage=c(0.1),n.minobsinnode=c(10))
xgbmgrid<-expand.grid(min_child_weight=c(7),eta=c(0.03),
                      nrounds=c(400),max_depth=4,gamma=0,
                      colsample_bytree=0.7,subsample=0.7)
svmlinGrid <- expand.grid(C=c(50))
svmPolyGrid <- expand.grid(C=c(0.2),degree=c(2),scale=c(0.1))
svmRadialGrid <- expand.grid(sigma=c(0.01),C=c(10))

# Creamos una lista de modelos
set.seed(1234)
models1 <-
  caretList(price~gt+length+build_year+displacement_tonnage+
              water_capacity+total_power+hull_number+
              engine_make.Caterpillar,data=data,trControl=stackControl,
            tuneList=list(
              lin=caretModelSpec(method="lm"),
              svmLinear=caretModelSpec(method="svmLinear",
                                       tuneGrid=svmlinGrid),
              svmPoly=caretModelSpec(method="svmPoly",
                                     tuneGrid=svmPolyGrid),
              svmRadial=caretModelSpec(method="svmRadial",
                                       tuneGrid=svmRadialGrid)
            ))

results <- resamples(models1)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ense <- caretEnsemble(models1)

summary(ense)

# Guardamos predicciones
predi1<-models1$lin$pred
predi5<-models1$svmLinear$pred
predi6<-models1$svmPoly$pred
predi7<-models1$svmRadial$pred

# Creamos otra lista de modelos
set.seed(1234)
models2 <-
  caretList(price~gt+length+beam+crew+build_year,
            data=data,trControl=stackControl,
            tuneList=list(
              rf=caretModelSpec(method="rf",n.trees=200,nodesize=4,
                                sampsize=300,tuneGrid=rfGrid),
              gbm=caretModelSpec(method="gbm",tuneGrid=gbmGrid),
              xgbm=caretModelSpec(method="xgbTree",tuneGrid=xgbmgrid)
            ))

results <- resamples(models2)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ense <- caretEnsemble(models2)

summary(ense)

# Guardamos predicciones
predi2<-models2$rf$pred
predi3<-models2$gbm$pred
predi4<-models2$xgbm$pred


## CREAMOS UNA LISTA CON TODAS LAS PREDICCIONES ##

unipredi<-lst(predi1,predi2,predi3,predi4,
              predi5,predi6,predi7)

# Construimos modelos ensamblados combinando 
# los otros de 2 en 2

unipredi$predi8$pred<-(unipredi$predi1$pred+unipredi$predi2$pred)/2
unipredi$predi8$obs<-unipredi$predi1$obs
unipredi$predi8$Resample<-unipredi$predi1$Resample
predi8<-as.data.frame(unipredi$predi8)

unipredi$predi9$pred<-(unipredi$predi1$pred+unipredi$predi3$pred)/2
unipredi$predi9$obs<-unipredi$predi1$obs
unipredi$predi9$Resample<-unipredi$predi1$Resample
predi9<-as.data.frame(unipredi$predi9)

unipredi$predi10$pred<-(unipredi$predi1$pred+unipredi$predi4$pred)/2
unipredi$predi10$obs<-unipredi$predi1$obs
unipredi$predi10$Resample<-unipredi$predi1$Resample
predi10<-as.data.frame(unipredi$predi10)

unipredi$predi11$pred<-(unipredi$predi1$pred+unipredi$predi5$pred)/2
unipredi$predi11$obs<-unipredi$predi1$obs
unipredi$predi11$Resample<-unipredi$predi1$Resample
predi11<-as.data.frame(unipredi$predi11)

unipredi$predi12$pred<-(unipredi$predi1$pred+unipredi$predi6$pred)/2
unipredi$predi12$obs<-unipredi$predi1$obs
unipredi$predi12$Resample<-unipredi$predi1$Resample
predi12<-as.data.frame(unipredi$predi12)

unipredi$predi13$pred<-(unipredi$predi1$pred+unipredi$predi7$pred)/2
unipredi$predi13$obs<-unipredi$predi1$obs
unipredi$predi13$Resample<-unipredi$predi1$Resample
predi13<-as.data.frame(unipredi$predi13)

unipredi$predi14$pred<-(unipredi$predi2$pred+unipredi$predi3$pred)/2
unipredi$predi14$obs<-unipredi$predi1$obs
unipredi$predi14$Resample<-unipredi$predi1$Resample
predi14<-as.data.frame(unipredi$predi14)

unipredi$predi15$pred<-(unipredi$predi2$pred+unipredi$predi4$pred)/2
unipredi$predi15$obs<-unipredi$predi1$obs
unipredi$predi15$Resample<-unipredi$predi1$Resample
predi15<-as.data.frame(unipredi$predi15)

unipredi$predi16$pred<-(unipredi$predi2$pred+unipredi$predi5$pred)/2
unipredi$predi16$obs<-unipredi$predi1$obs
unipredi$predi16$Resample<-unipredi$predi1$Resample
predi16<-as.data.frame(unipredi$predi16)

unipredi$predi17$pred<-(unipredi$predi2$pred+unipredi$predi6$pred)/2
unipredi$predi17$obs<-unipredi$predi1$obs
unipredi$predi17$Resample<-unipredi$predi1$Resample
predi17<-as.data.frame(unipredi$predi17)

unipredi$predi18$pred<-(unipredi$predi2$pred+unipredi$predi7$pred)/2
unipredi$predi18$obs<-unipredi$predi1$obs
unipredi$predi18$Resample<-unipredi$predi1$Resample
predi18<-as.data.frame(unipredi$predi18)

unipredi$predi19$pred<-(unipredi$predi3$pred+unipredi$predi4$pred)/2
unipredi$predi19$obs<-unipredi$predi1$obs
unipredi$predi19$Resample<-unipredi$predi1$Resample
predi19<-as.data.frame(unipredi$predi19)

unipredi$predi20$pred<-(unipredi$predi3$pred+unipredi$predi5$pred)/2
unipredi$predi20$obs<-unipredi$predi1$obs
unipredi$predi20$Resample<-unipredi$predi1$Resample
predi20<-as.data.frame(unipredi$predi20)

unipredi$predi21$pred<-(unipredi$predi3$pred+unipredi$predi6$pred)/2
unipredi$predi21$obs<-unipredi$predi1$obs
unipredi$predi21$Resample<-unipredi$predi1$Resample
predi21<-as.data.frame(unipredi$predi21)

unipredi$predi22$pred<-(unipredi$predi3$pred+unipredi$predi7$pred)/2
unipredi$predi22$obs<-unipredi$predi1$obs
unipredi$predi22$Resample<-unipredi$predi1$Resample
predi22<-as.data.frame(unipredi$predi22)

unipredi$predi23$pred<-(unipredi$predi4$pred+unipredi$predi5$pred)/2
unipredi$predi23$obs<-unipredi$predi1$obs
unipredi$predi23$Resample<-unipredi$predi1$Resample
predi23<-as.data.frame(unipredi$predi23)

unipredi$predi24$pred<-(unipredi$predi4$pred+unipredi$predi6$pred)/2
unipredi$predi24$obs<-unipredi$predi1$obs
unipredi$predi24$Resample<-unipredi$predi1$Resample
predi24<-as.data.frame(unipredi$predi24)

unipredi$predi25$pred<-(unipredi$predi4$pred+unipredi$predi7$pred)/2
unipredi$predi25$obs<-unipredi$predi1$obs
unipredi$predi25$Resample<-unipredi$predi1$Resample
predi25<-as.data.frame(unipredi$predi25)

unipredi$predi26$pred<-(unipredi$predi5$pred+unipredi$predi6$pred)/2
unipredi$predi26$obs<-unipredi$predi1$obs
unipredi$predi26$Resample<-unipredi$predi1$Resample
predi26<-as.data.frame(unipredi$predi26)

unipredi$predi27$pred<-(unipredi$predi5$pred+unipredi$predi7$pred)/2
unipredi$predi27$obs<-unipredi$predi1$obs
unipredi$predi27$Resample<-unipredi$predi1$Resample
predi27<-as.data.frame(unipredi$predi27)

unipredi$predi28$pred<-(unipredi$predi6$pred+unipredi$predi7$pred)/2
unipredi$predi28$obs<-unipredi$predi1$obs
unipredi$predi28$Resample<-unipredi$predi1$Resample
predi28<-as.data.frame(unipredi$predi28)

# Ahora construimos modelos ensamblados combinando 
# los otros de 3 en 3

unipredi$predi29$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred)/3
unipredi$predi29$obs<-unipredi$predi1$obs
unipredi$predi29$Resample<-unipredi$predi1$Resample
predi29<-as.data.frame(unipredi$predi29)

unipredi$predi30$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi4$pred)/3
unipredi$predi30$obs<-unipredi$predi1$obs
unipredi$predi30$Resample<-unipredi$predi1$Resample
predi30<-as.data.frame(unipredi$predi30)

unipredi$predi31$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi5$pred)/3
unipredi$predi31$obs<-unipredi$predi1$obs
unipredi$predi31$Resample<-unipredi$predi1$Resample
predi31<-as.data.frame(unipredi$predi31)

unipredi$predi32$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi6$pred)/3
unipredi$predi32$obs<-unipredi$predi1$obs
unipredi$predi32$Resample<-unipredi$predi1$Resample
predi32<-as.data.frame(unipredi$predi32)

unipredi$predi33$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi7$pred)/3
unipredi$predi33$obs<-unipredi$predi1$obs
unipredi$predi33$Resample<-unipredi$predi1$Resample
predi33<-as.data.frame(unipredi$predi33)

unipredi$predi34$pred<-(unipredi$predi1$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred)/3
unipredi$predi34$obs<-unipredi$predi1$obs
unipredi$predi34$Resample<-unipredi$predi1$Resample
predi34<-as.data.frame(unipredi$predi34)

unipredi$predi35$pred<-(unipredi$predi1$pred+unipredi$predi3$pred+
                          unipredi$predi5$pred)/3
unipredi$predi35$obs<-unipredi$predi1$obs
unipredi$predi35$Resample<-unipredi$predi1$Resample
predi35<-as.data.frame(unipredi$predi35)

unipredi$predi36$pred<-(unipredi$predi1$pred+unipredi$predi3$pred+
                          unipredi$predi6$pred)/3
unipredi$predi36$obs<-unipredi$predi1$obs
unipredi$predi36$Resample<-unipredi$predi1$Resample
predi36<-as.data.frame(unipredi$predi36)

unipredi$predi37$pred<-(unipredi$predi1$pred+unipredi$predi3$pred+
                          unipredi$predi7$pred)/3
unipredi$predi37$obs<-unipredi$predi1$obs
unipredi$predi37$Resample<-unipredi$predi1$Resample
predi37<-as.data.frame(unipredi$predi37)

unipredi$predi38$pred<-(unipredi$predi1$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred)/3
unipredi$predi38$obs<-unipredi$predi1$obs
unipredi$predi38$Resample<-unipredi$predi1$Resample
predi38<-as.data.frame(unipredi$predi38)

unipredi$predi39$pred<-(unipredi$predi1$pred+unipredi$predi4$pred+
                          unipredi$predi6$pred)/3
unipredi$predi39$obs<-unipredi$predi1$obs
unipredi$predi39$Resample<-unipredi$predi1$Resample
predi39<-as.data.frame(unipredi$predi39)

unipredi$predi40$pred<-(unipredi$predi1$pred+unipredi$predi4$pred+
                          unipredi$predi7$pred)/3
unipredi$predi40$obs<-unipredi$predi1$obs
unipredi$predi40$Resample<-unipredi$predi1$Resample
predi40<-as.data.frame(unipredi$predi40)

unipredi$predi41$pred<-(unipredi$predi1$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred)/3
unipredi$predi41$obs<-unipredi$predi1$obs
unipredi$predi41$Resample<-unipredi$predi1$Resample
predi41<-as.data.frame(unipredi$predi41)

unipredi$predi42$pred<-(unipredi$predi1$pred+unipredi$predi5$pred+
                          unipredi$predi7$pred)/3
unipredi$predi42$obs<-unipredi$predi1$obs
unipredi$predi42$Resample<-unipredi$predi1$Resample
predi42<-as.data.frame(unipredi$predi42)

unipredi$predi43$pred<-(unipredi$predi1$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/3
unipredi$predi43$obs<-unipredi$predi1$obs
unipredi$predi43$Resample<-unipredi$predi1$Resample
predi43<-as.data.frame(unipredi$predi43)

unipredi$predi44$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred)/3
unipredi$predi44$obs<-unipredi$predi1$obs
unipredi$predi44$Resample<-unipredi$predi1$Resample
predi44<-as.data.frame(unipredi$predi44)

unipredi$predi45$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi5$pred)/3
unipredi$predi45$obs<-unipredi$predi1$obs
unipredi$predi45$Resample<-unipredi$predi1$Resample
predi45<-as.data.frame(unipredi$predi45)

unipredi$predi46$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi6$pred)/3
unipredi$predi46$obs<-unipredi$predi1$obs
unipredi$predi46$Resample<-unipredi$predi1$Resample
predi46<-as.data.frame(unipredi$predi46)

unipredi$predi47$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi7$pred)/3
unipredi$predi47$obs<-unipredi$predi1$obs
unipredi$predi47$Resample<-unipredi$predi1$Resample
predi47<-as.data.frame(unipredi$predi47)

unipredi$predi48$pred<-(unipredi$predi2$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred)/3
unipredi$predi48$obs<-unipredi$predi1$obs
unipredi$predi48$Resample<-unipredi$predi1$Resample
predi48<-as.data.frame(unipredi$predi48)

unipredi$predi49$pred<-(unipredi$predi2$pred+unipredi$predi4$pred+
                          unipredi$predi6$pred)/3
unipredi$predi49$obs<-unipredi$predi1$obs
unipredi$predi49$Resample<-unipredi$predi1$Resample
predi49<-as.data.frame(unipredi$predi49)

unipredi$predi50$pred<-(unipredi$predi2$pred+unipredi$predi4$pred+
                          unipredi$predi7$pred)/3
unipredi$predi50$obs<-unipredi$predi1$obs
unipredi$predi50$Resample<-unipredi$predi1$Resample
predi50<-as.data.frame(unipredi$predi50)

unipredi$predi51$pred<-(unipredi$predi2$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred)/3
unipredi$predi51$obs<-unipredi$predi1$obs
unipredi$predi51$Resample<-unipredi$predi1$Resample
predi51<-as.data.frame(unipredi$predi51)

unipredi$predi52$pred<-(unipredi$predi2$pred+unipredi$predi5$pred+
                          unipredi$predi7$pred)/3
unipredi$predi52$obs<-unipredi$predi1$obs
unipredi$predi52$Resample<-unipredi$predi1$Resample
predi52<-as.data.frame(unipredi$predi52)

unipredi$predi53$pred<-(unipredi$predi2$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/3
unipredi$predi53$obs<-unipredi$predi1$obs
unipredi$predi53$Resample<-unipredi$predi1$Resample
predi53<-as.data.frame(unipredi$predi53)

unipredi$predi54$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred)/3
unipredi$predi54$obs<-unipredi$predi1$obs
unipredi$predi54$Resample<-unipredi$predi1$Resample
predi54<-as.data.frame(unipredi$predi54)

unipredi$predi55$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi6$pred)/3
unipredi$predi55$obs<-unipredi$predi1$obs
unipredi$predi55$Resample<-unipredi$predi1$Resample
predi55<-as.data.frame(unipredi$predi55)

unipredi$predi56$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi7$pred)/3
unipredi$predi56$obs<-unipredi$predi1$obs
unipredi$predi56$Resample<-unipredi$predi1$Resample
predi56<-as.data.frame(unipredi$predi56)

unipredi$predi57$pred<-(unipredi$predi3$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred)/3
unipredi$predi57$obs<-unipredi$predi1$obs
unipredi$predi57$Resample<-unipredi$predi1$Resample
predi57<-as.data.frame(unipredi$predi57)

unipredi$predi58$pred<-(unipredi$predi3$pred+unipredi$predi5$pred+
                          unipredi$predi7$pred)/3
unipredi$predi58$obs<-unipredi$predi1$obs
unipredi$predi58$Resample<-unipredi$predi1$Resample
predi58<-as.data.frame(unipredi$predi58)

unipredi$predi59$pred<-(unipredi$predi3$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/3
unipredi$predi59$obs<-unipredi$predi1$obs
unipredi$predi59$Resample<-unipredi$predi1$Resample
predi59<-as.data.frame(unipredi$predi59)

unipredi$predi60$pred<-(unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred)/3
unipredi$predi60$obs<-unipredi$predi1$obs
unipredi$predi60$Resample<-unipredi$predi1$Resample
predi60<-as.data.frame(unipredi$predi60)

unipredi$predi61$pred<-(unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi7$pred)/3
unipredi$predi61$obs<-unipredi$predi1$obs
unipredi$predi61$Resample<-unipredi$predi1$Resample
predi61<-as.data.frame(unipredi$predi61)

unipredi$predi62$pred<-(unipredi$predi4$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/3
unipredi$predi62$obs<-unipredi$predi1$obs
unipredi$predi62$Resample<-unipredi$predi1$Resample
predi62<-as.data.frame(unipredi$predi62)

unipredi$predi63$pred<-(unipredi$predi5$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/3
unipredi$predi63$obs<-unipredi$predi1$obs
unipredi$predi63$Resample<-unipredi$predi1$Resample
predi63<-as.data.frame(unipredi$predi63)

# Ahora construimos modelos ensamblados combinando 
# los otros de 4 en 4

unipredi$predi64$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred)/4
unipredi$predi64$obs<-unipredi$predi1$obs
unipredi$predi64$Resample<-unipredi$predi1$Resample
predi64<-as.data.frame(unipredi$predi64)

unipredi$predi65$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi5$pred)/4
unipredi$predi65$obs<-unipredi$predi1$obs
unipredi$predi65$Resample<-unipredi$predi1$Resample
predi65<-as.data.frame(unipredi$predi65)

unipredi$predi66$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi6$pred)/4
unipredi$predi66$obs<-unipredi$predi1$obs
unipredi$predi66$Resample<-unipredi$predi1$Resample
predi66<-as.data.frame(unipredi$predi66)

unipredi$predi67$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi7$pred)/4
unipredi$predi67$obs<-unipredi$predi1$obs
unipredi$predi67$Resample<-unipredi$predi1$Resample
predi67<-as.data.frame(unipredi$predi67)

unipredi$predi68$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi5$pred)/4
unipredi$predi68$obs<-unipredi$predi1$obs
unipredi$predi68$Resample<-unipredi$predi1$Resample
predi68<-as.data.frame(unipredi$predi68)

unipredi$predi69$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi6$pred)/4
unipredi$predi69$obs<-unipredi$predi1$obs
unipredi$predi69$Resample<-unipredi$predi1$Resample
predi69<-as.data.frame(unipredi$predi69)

unipredi$predi70$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi7$pred)/4
unipredi$predi70$obs<-unipredi$predi1$obs
unipredi$predi70$Resample<-unipredi$predi1$Resample
predi70<-as.data.frame(unipredi$predi70)

unipredi$predi71$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi6$pred)/4
unipredi$predi71$obs<-unipredi$predi1$obs
unipredi$predi71$Resample<-unipredi$predi1$Resample
predi71<-as.data.frame(unipredi$predi71)

unipredi$predi72$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi7$pred)/4
unipredi$predi72$obs<-unipredi$predi1$obs
unipredi$predi72$Resample<-unipredi$predi1$Resample
predi72<-as.data.frame(unipredi$predi72)

unipredi$predi73$pred<-(unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred+unipredi$predi7$pred)/4
unipredi$predi73$obs<-unipredi$predi1$obs
unipredi$predi73$Resample<-unipredi$predi1$Resample
predi73<-as.data.frame(unipredi$predi73)

# Ahora construimos modelos ensamblados combinando 
# los otros de 5 en 5

unipredi$predi74$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred)/5
unipredi$predi74$obs<-unipredi$predi1$obs
unipredi$predi74$Resample<-unipredi$predi1$Resample
predi74<-as.data.frame(unipredi$predi74)

unipredi$predi75$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi6$pred)/5
unipredi$predi75$obs<-unipredi$predi1$obs
unipredi$predi75$Resample<-unipredi$predi1$Resample
predi75<-as.data.frame(unipredi$predi75)

unipredi$predi76$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi7$pred)/5
unipredi$predi76$obs<-unipredi$predi1$obs
unipredi$predi76$Resample<-unipredi$predi1$Resample
predi76<-as.data.frame(unipredi$predi76)

unipredi$predi77$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred)/5
unipredi$predi77$obs<-unipredi$predi1$obs
unipredi$predi77$Resample<-unipredi$predi1$Resample
predi77<-as.data.frame(unipredi$predi77)

unipredi$predi78$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi7$pred)/5
unipredi$predi78$obs<-unipredi$predi1$obs
unipredi$predi78$Resample<-unipredi$predi1$Resample
predi78<-as.data.frame(unipredi$predi78)

unipredi$predi79$pred<-(unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/5
unipredi$predi79$obs<-unipredi$predi1$obs
unipredi$predi79$Resample<-unipredi$predi1$Resample
predi79<-as.data.frame(unipredi$predi79)

# Ahora construimos modelos ensamblados combinando 
# los otros de 6 en 6

unipredi$predi80$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi6$pred)/6
unipredi$predi80$obs<-unipredi$predi1$obs
unipredi$predi80$Resample<-unipredi$predi1$Resample
predi80<-as.data.frame(unipredi$predi80)

unipredi$predi81$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi7$pred)/6
unipredi$predi81$obs<-unipredi$predi1$obs
unipredi$predi81$Resample<-unipredi$predi1$Resample
predi81<-as.data.frame(unipredi$predi81)

unipredi$predi82$pred<-(unipredi$predi2$pred+unipredi$predi3$pred+
                          unipredi$predi4$pred+unipredi$predi5$pred+
                          unipredi$predi6$pred+unipredi$predi7$pred)/6
unipredi$predi82$obs<-unipredi$predi1$obs
unipredi$predi82$Resample<-unipredi$predi1$Resample
predi82<-as.data.frame(unipredi$predi82)

# Ahora construimos un último ensamblado combinando todos

unipredi$predi83$pred<-(unipredi$predi1$pred+unipredi$predi2$pred+
                          unipredi$predi3$pred+unipredi$predi4$pred+
                          unipredi$predi5$pred+unipredi$predi6$pred+
                          unipredi$predi7$pred)/7
unipredi$predi83$obs<-unipredi$predi1$obs
unipredi$predi83$Resample<-unipredi$predi1$Resample
predi83<-as.data.frame(unipredi$predi83)


unipredi <- lst(predi1,predi2,predi3,predi4,predi5,predi6,
                predi7,predi8,predi9,predi10,predi11,predi12,
                predi13,predi14,predi15,predi16,predi17,predi18,
                predi19,predi20,predi21,predi22,predi23,predi24,
                predi25,predi26,predi27,predi28,predi29,predi30,
                predi31,predi32,predi33,predi34,predi35,predi36,
                predi37,predi38,predi39,predi40,predi41,predi42,
                predi43,predi44,predi45,predi46,predi47,predi48,
                predi49,predi50,predi51,predi52,predi53,predi54,
                predi55,predi56,predi57,predi58,predi59,predi60,
                predi61,predi62,predi63,predi64,predi65,predi66,
                predi67,predi68,predi69,predi70,predi71,predi72,
                predi73,predi74,predi75,predi76,predi77,predi78,
                predi79,predi80,predi81,predi82,predi83)



#### MÉTRICAS DE LOS ENSAMBLADOS ####

# Cargamos librerías
library(dplyr)
library(Metrics)

# Función para calcular el RMSE
calc_rmse <- function(pred, obs) {
  sqrt(mean((pred - obs) ^ 2))
}
# Función para calcular el R-squared
calc_rsquared <- function(pred, obs) {
  cor(pred, obs) ^ 2
}
# Función para calcular el MAE
calc_mae <- function(pred, obs) {
  mean(abs(pred - obs))
}

# Función para calcular las métricas en validación cruzada
calculate_metrics <- function(df) {
  metrics_by_fold <- df |> 
    group_by(Resample) |> 
    summarise(
      RMSE_ = calc_rmse(pred, obs),
      Rsquared_ = calc_rsquared(pred, obs),
      MAE_ = calc_mae(pred, obs)
    )
  metrics_sd <- metrics_by_fold |> 
    summarise(
      RMSE = mean(RMSE_),
      Rsquared = mean(Rsquared_),
      MAE = mean(MAE_),
      RMSESD = sd(RMSE_),
      RsquaredSD = sd(Rsquared_),
      MAESD = sd(MAE_)
    )
  return(metrics_sd)
}

# Aplicamos nuestra función en la lista de modelos "unipredi"
results_list <- lapply(unipredi, calculate_metrics)

# Recopilamos los resultados en un único dataframe
final_results <- bind_rows(results_list, .id = "DataFrameID")

# Mostramos los resultados
print(final_results)



# Creamos boxplots de comparación de modelos
calculate_error <- function(df) {
  metrics_by_fold <- df |> 
    group_by(Resample) |> 
    summarise(
      error = calc_rmse(pred, obs)
    )
  return(metrics_by_fold)
}

error_list <- lapply(unipredi, calculate_error)
error_results <- bind_rows(error_list, .id = "DataFrameID")
error_results$DataFrameID <- 
  with(error_results,reorder(DataFrameID,error,mean))


par(cex.axis=0.8)
boxplot(data=error_results,error~DataFrameID,col="pink")



#### INTERPRETACIÓN DE LM, ÁRBOL DE DECISIÓN Y RANDOM FOREST ####

# Cargamos el dataset con las variables sin normalizar
barcos <- read.csv(file="./datos/Barcos.csv",
                   sep=",")
# Ejecutamos los cambios del archivo "Exploracion y Modificacion dataset.R"
# desde la línea 46 hasta la 812, luego ejecutamos el código:
# barcos<-cbind(barcos[,c(var_num,var_bin,var_cuali,vardep)])
# barcos <- dummy.data.frame(barcos, var_cuali, sep = ".")
# data <- barcos

# Regresión lineal
rlin <-
  lm(price~gt+length+build_year+displacement_tonnage+
       water_capacity+total_power+hull_number+
       engine_make.Caterpillar,
     data = data |> select(all_of(cvar7),price))

summary(rlin)
broom::glance(rlin)

# Árbol de decisión
arbol <- rpart(price~gt+length+beam+crew+build_year, data = data,
                method = "anova",maxdepth=6,minsplit=15,
                parms=list(split="gini"),cp=0.0001)
summary(arbol)
rpart.plot(arbol,nn=TRUE,tweak = 1.2)

# Random forest

# Gráfico OOB
rfbis<-randomForest(price~gt+length+beam+crew+build_year,
                    data=data, mtry=4,ntree=200,sampsize=300,
                    nodesize=4,replace=TRUE)
plot(rfbis$mse)

# Uso de random forest explainer
library(randomForestExplainer)
forest<-randomForest(price~gt+length+beam+crew+build_year,
                     data=data, mtry=4,ntree=200,sampsize=300,
                     nodesize=4,replace=TRUE,localImp = TRUE)
explain_forest(forest, interactions = TRUE, data = data)

# Importancia de variables con VIP
vip(forest)


