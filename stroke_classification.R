setwd("C:/Users/ADALBERRTO CERRILLO/Documents/MDD")
library(naniar)
library(caret)
library(magrittr)
library(pROC)
library(dplyr)
library(ggplot2)
library(Boruta)         
library(caret)
library(randomForest)
library(ROSE)

#importamos los datos
data <- read.csv("stroke.csv")
head(data)

#removemos la columna ineccesaria (id)
data$id <- NULL
#Realizando la imputacion de los datos faltantes

#bmi
data2 <- data %>%  impute_mean_if(is.numeric)
data$bmi <- round(data2$bmi,1)
head(data)

#Realizando el tratamiento de las variables categoricas
head(data)
#gender
levelsGender <- c('Male', 'Female',"Other")
data$gender <- match(data$gender, levelsGender)
head(data)

#evermarried
levelsEverMarried <- c("Yes","No")
data$ever_married <- match(data$ever_married, levelsEverMarried)
head(data)

#worktype
levesWork <- c("Self-employed", "Private","Govt_job","children","Never_worked")
data$work_type <- match(data$work_type, levesWork)
head(data)

#residencetype
levelsResidence <- c("Urban","Rural")
data$Residence_type <- match(data$Residence_type, levelsResidence)
head(data)

#smoke
levelsSmoking <- c("formerly smoked","smokes","never smoked","Unknown")
data$smoking_status <- match(data$smoking_status, levelsSmoking)
head(data)

any(is.na(data))

#--------------Analisis de Boruta-----------------------------------------

set.seed(2304)
boruta_analysis = Boruta(stroke ~ ., data= data, maxRuns=200)
plot(boruta_analysis, las = 2, cex.axis = 0.5)

as.data.frame(boruta_analysis$finalDecision)
getSelectedAttributes(boruta_analysis, withTentative = FALSE)

discard <- c("age","hypertension","heart_disease","ever_married","work_type","bmi","stroke")
data <- data[ , (names(data) %in% discard)]
head(data)


#-------------Creamos el modelo random forest------------------------------------

#la variable respuesta tiene solo dos clases
table(data$stroke)

#vemos que los datos estan desbalanceados, debemos balancearlos
data <- ovun.sample(stroke ~ .,data = data,method = "over",N = table(data$stroke)[1]*2)$data
head(data)
table(data$stroke)

#creamos las particiones
set.seed(666)
train <- createDataPartition(data$stroke, p = 0.8, list = FALSE)
training <- data[train, ]
testing <- data[-train, ]
table(training$stroke)
table(testing$stroke)

#modelo
trainctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
train_fit <- train(factor(stroke) ~., data = training, method = "rf", trControl=trainctrl)
train_fit
predtrain <- train_fit$pred

roc_train=roc(response = train_fit$pred$obs, predictor= factor(train_fit$pred$pred,ordered = TRUE))
auc_train<-auc(roc_train)
plot(roc(predictor = factor(train_fit$pred$pred,ordered = TRUE), response = train_fit$pred$obs))

conf_matrix_train<-table(factor(train_fit$pred$pred,ordered = TRUE),train_fit$pred$obs)
conf_matrix_train

train_fit
confusionMatrix(data = factor(predtrain$pred,ordered = TRUE), reference = predtrain$obs)
auc_train

testctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
test_fit <- train(factor(stroke) ~., data = testing, method = "rf", trControl=testctrl)
test_fit
predtest <- test_fit$pred

roc_test=roc(response = test_fit$pred$obs, predictor= factor(test_fit$pred$pred,ordered = TRUE))
auc_test<-auc(roc_test)
plot(roc(predictor = factor(test_fit$pred$pred,ordered = TRUE), response = test_fit$pred$obs))

conf_matrix_tst<-table(factor(test_fit$pred$pred,ordered = TRUE),test_fit$pred$obs)
conf_matrix_tst

test_fit
confusionMatrix(data = factor(predtest$pred,ordered = TRUE), reference = predtest$obs)
auc_test

#probando eliminar la variable "ever married"
discard <- c("age","hypertension","heart_disease","work_type","bmi","stroke")
data <- data[ , (names(data) %in% discard)]
head(data)

#la variable respuesta tiene solo dos clases
table(data$stroke)

#vemos que los datos estan desbalanceados, debemos balancearlos
data <- ovun.sample(stroke ~ .,data = data,method = "over",N = table(data$stroke)[1]*2)$data
head(data)
table(data$stroke)

#creamos las particiones
set.seed(666)
train <- createDataPartition(data$stroke, p = 0.8, list = FALSE)
training <- data[train, ]
testing <- data[-train, ]
table(training$stroke)
table(testing$stroke)

#modelo
trainctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
train_fit <- train(factor(stroke) ~., data = training, method = "rf", trControl=trainctrl)
train_fit
predtrain <- train_fit$pred

roc_train=roc(response = train_fit$pred$obs, predictor= factor(train_fit$pred$pred,ordered = TRUE))
auc_train<-auc(roc_train)
plot(roc(predictor = factor(train_fit$pred$pred,ordered = TRUE), response = train_fit$pred$obs))

conf_matrix_train<-table(factor(train_fit$pred$pred,ordered = TRUE),train_fit$pred$obs)
conf_matrix_train

train_fit
confusionMatrix(data = factor(predtrain$pred,ordered = TRUE), reference = predtrain$obs)
auc_train

testctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
test_fit <- train(factor(stroke) ~., data = testing, method = "rf", trControl=testctrl)
test_fit
predtest <- test_fit$pred

roc_test=roc(response = test_fit$pred$obs, predictor= factor(test_fit$pred$pred,ordered = TRUE))
auc_test<-auc(roc_test)
plot(roc(predictor = factor(test_fit$pred$pred,ordered = TRUE), response = test_fit$pred$obs))

conf_matrix_tst<-table(factor(test_fit$pred$pred,ordered = TRUE),test_fit$pred$obs)
conf_matrix_tst

test_fit
confusionMatrix(data = factor(predtest$pred,ordered = TRUE), reference = predtest$obs)
auc_test