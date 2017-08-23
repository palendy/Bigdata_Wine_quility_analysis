#package install
install.packages("readxl")
install.packages('MASS')
install.packages('sqldf')
install.packages('plyr')
install.packages('ggplot2')
install.packages('party')
install.packages('ROCR')
install.packages('caret')
install.packages('nnet')
install.packages('devtools')
install.packages('xlsx')

#package load
library('xlsx')
library('readxl')
library('MASS')
library('sqldf')
library('plyr')
library('ggplot2')
library('party')
library('ROCR')
library('caret')
library('nnet')
library(devtools)

install.packages("nnet")
library(nnet)

install.packages('devtools')
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')



#data import 
WhiteWine <- read_excel("~/Wine_data.xlsx",1)
attach(WhiteWine)
summary(WhiteWine)

## step 1. data preparation -- raw data analysis, EDA ##

#histogram
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist( `fixed acidity` , h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(`volatile acidity`, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(`citric acid`, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)


truehist(`residual sugar`, h = 0.5, col="slategray3")
mtext("residual sugar", side=1, outer=F, line=2, cex=0.8)


truehist(chlorides, h = 0.01, col="slategray3")
mtext("chlorides", side=1, outer=F, line=2, cex=0.8)

truehist(`free sulfur dioxide` , h = 0.5, col="slategray3")
mtext("free sulfur dioxide", side=1, outer=F, line=2, cex=0.8)

truehist(`total sulfur dioxide`, h = 0.5, col="slategray3")
mtext("total sulfur dioxider", side=1, outer=F, line=2, cex=0.8)

truehist(density, h = 0.005, col="slategray3")
mtext("density", side=1, outer=F, line=2, cex=0.8)

truehist(pH, h = 0.01, col="slategray3")
mtext("ph", side=1, outer=F, line=2, cex=0.8)

truehist(sulphates, h = 0.01, col="slategray3")
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)

truehist(alcohol, h = 0.1, col="slategray3")
mtext("alchohol", side=1, outer=F, line=2, cex=0.8)

# box plot
par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(`fixed acidity`, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)

boxplot(`volatile acidity`, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)

boxplot(`citric acid`, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)

boxplot(`residual sugar`, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)


#data refine
WhiteWine <- read_excel("~/Wine_data.xlsx",1)
summary(WhiteWine)
WhiteWine <- subset(WhiteWine, 
                    `fixed acidity` < 12 & `volatile acidity` < 1.0 
                    & `citric acid` < 1.2 & `residual sugar` < 30 
                    & chlorides < 0.25 & `free sulfur dioxide` < 150 &
                      `total sulfur dioxide` < 300 & density < 1.01 
)


## step 2,3 Model plan& build  a. Regression model ##

# k fold - 아직 안됨 
k <- 10
folds <- createFolds(WhiteWine$quality, k  )
sum <- 0
for( i in 1:k  ){
  train_idx <- folds[[i]]
  wwine.train <- WhiteWine[train_idx ,]
  wwine.validation <- WhiteWine[-train_idx,]
  
  ## model Test 
  model <- lm(quality ~ `fixed acidity`+ `volatile acidity` +
                `citric acid` + `residual sugar` + chlorides +
                `free sulfur dioxide` + `total sulfur dioxide` +
                density + pH + sulphates + alcohol , data = wwine.train)
  
  removal_model <- lm(quality ~ `fixed acidity`+
                        `citric acid` + `residual sugar` +
                        `free sulfur dioxide` + `total sulfur dioxide` +
                        pH + sulphates + alcohol ,data = wwine.train)
  
  summary(model)
  summary(removal_model)
  
  
  p <- predict(removal_model, wwine.validation)
  q<-round(p)
  
  
  a<-confusionMatrix(q, wwine.validation$quality)
  sum <- sum + a$overall[1]
}
accuracy.avg <- sum/k
accuracy.avg
a


## 그냥 구역 나눠서 한것 train test 만든것
WhiteWine <- read_excel("~/Wine_data.xlsx",1)
train <- WhiteWine[1:2449,]
test <- WhiteWine[2450:4898,]

removal_model <- lm(quality ~ `fixed acidity`+
                      `citric acid` + `residual sugar` +
                      `free sulfur dioxide` + `total sulfur dioxide` +
                      pH + sulphates + alcohol ,data = train)
summary(removal_model)
anova (removal_model, test = 'Chisq')


p <- predict(removal_model, test)
q<-round(p)
confusionMatrix(q, test$quality)


## step 3. model build  (b) C Tree 

# ctree evaluation
train <- WhiteWine[1:2449,]
test <- WhiteWine[2450:4898,]

attach(train)
tree_data <- ctree(quality ~  `fixed acidity`+ `volatile acidity` +
                     `citric acid` + `residual sugar` + chlorides +
                     `free sulfur dioxide` + `total sulfur dioxide` +
                     density + pH + sulphates + alcohol , data=train)

pred_train <- predict(tree_data, train)
q<-round(pred_train)
confusionMatrix(q, test$quality)
plot(tree_data , type ='simple')

## step 3. model build (C) Neural Network

WhiteWine <- read_excel("~/Wine_data.xlsx",1)

attach(WhiteWine)

names(WhiteWine)[names(WhiteWine) == 'fixed acidity'] <- c("fixed.acidity")
names(WhiteWine)[names(WhiteWine) == 'volatile acidity'] <- c("volatile.acidity")
names(WhiteWine)[names(WhiteWine) == 'citric acid'] <- c("citric.acid")
names(WhiteWine)[names(WhiteWine) == 'residual sugar'] <- c("residual.sugar")

head(WhiteWine)
nrow(WhiteWine)
ncol(WhiteWine)
summary(WhiteWine)



#Data Normalization
a<-WhiteWine[12]
str(a)
a$quality<-as.factor(a$quality)
WhiteWine.scale <- cbind(a, scale(WhiteWine[-12]))
head(WhiteWine.scale)
apply(WhiteWine.scale[-1], 2, sd)



# Partitioning the data into training and test data
k <- 10
folds <- createFolds(WhiteWine.scale$quality, k  )
sum <- 0
for( i in 1:k  ){
  train_idx <- folds[[i]]
  wwine.train <- WhiteWine.scale[train_idx ,]
  wwine.validation <- WhiteWine.scale[-train_idx,]
  
  # Fitting the neural network for the training data
  model.nnet <- nnet(quality ~ ., data = wwine.train, size = 1, decay = 5e-04, maxit = 200)
  summary(model.nnet)
  names(model.nnet)
  
  
  model.nnet$wts
  head(model.nnet$fitted.values)
  head(model.nnet$residuals)
  
  
  p <- predict(model.nnet, wwine.validation, type = 'class')
  
  a <- confusionMatrix(p, wwine.validation$quality)
  sum <- sum + a$overall[1]
}
accuracy.avg <- sum/k
accuracy.avg
a



#######################Neural Network###############################

#Data Normalization
a<-WhiteWine[12]
str(a)
a$quality<-as.factor(a$quality)
WhiteWine.scale <- cbind(a, scale(WhiteWine[-12]))
head(WhiteWine.scale)
apply(WhiteWine.scale[-1], 2, sd)

#k-fold Cross Validation
k <- 10
folds <- createFolds(WhiteWine.scale$quality, k  )
sum <- 0
for( i in 1:k  ){
  train_idx <- folds[[i]]
  wwine.train <- WhiteWine.scale[train_idx ,]
  wwine.validation <- WhiteWine.scale[-train_idx,]
  
  # Fitting the neural network for the training data
  model.nnet <- nnet(quality ~ ., data = wwine.train, size = 6, decay = 5e-04, maxit = 200)
  summary(model.nnet)
  names(model.nnet)
  
  
  model.nnet$wts
  head(model.nnet$fitted.values)
  head(model.nnet$residuals)
  
  
  p <- predict(model.nnet, wwine.validation, type = 'class')
  
  a<-confusionMatrix(p, wwine.validation$quality)
  sum <- sum + a$overall[1]
}

accuracy.avg <- sum/k
accuracy.avg
a
plot.nnet(model.nnet)





