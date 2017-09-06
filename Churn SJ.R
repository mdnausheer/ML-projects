library(ggthemes)
library(gridExtra)
library(data.table)
library(broom)
library(caret)
library(ggthemes)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(caret)
library(ROCR)
library(grid)
library(pROC)
library(datasets)
library(car)
wd <- choose.dir()
setwd(file.path(wd))
churn <- read.csv("Churn (1).csv")
str(churn)
summary(churn)
colnames(churn)

churn <- churn[,c(1:7,9:20,8)]
View(churn)

churn$Area.Code <- as.factor(churn$Area.Code)
churn$Churn <- as.factor(churn$Churn)
str(churn)
table(churn$Int.l.Plan)
table(churn$VMail.Plan)
table(churn$Churn)

churn$Int.l.Plan <- as.factor(churn$Int.l.Plan)
churn$VMail.Plan <- as.factor(churn$VMail.Plan)

str(churn)
summary(churn)

set.seed(400)
#train=sample(1:nrow(churn),round(nrow(churn)*0.70),replace = F)
#churn_train<-churn[train,]
#churn_test<-churn[-train,]
#table(churn_train$Churn)

install.packages('ROSE')
library(ROSE)

#__________________________________________________________________________________________________________________
#UNDERSAMPLING TECHNIQUE 

data_balanced_under<- ovun.sample(Churn ~ ., data = churn, method = "under", N = 1000, seed = 1)$data
table(data_balanced_under$Churn)
set.seed(400)
train=sample(1:nrow(data_balanced_under),round(nrow(data_balanced_under)*0.70),replace = F)
churn_train<-data_balanced_under[train,]
churn_test<-data_balanced_under[-train,]
table(data_balanced_under$Churn)

#Building the first model
model_lr1<- glm (Churn ~., data = churn_train, family = binomial(link = logit))
summary(model_lr1)
#AIC:768.87

churn_train$prediction <- predict( model_lr1, newdata = churn_train, type = "response" )
pred_lm1<- predict( model_lr1, newdata = churn_test , type = "response" )

pred_lm1_output <- prediction(pred_lm1,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm1_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm1_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm1<- auc(churn_test$Churn,pred_lm1)
#AUC:0.7432



#Building the second model
model_lr2<- glm (Churn ~CustServ.Calls + Day.Charge + Int.l.Plan + VMail.Plan + Day.Mins*CustServ.Calls, data = churn_train, family = binomial(link = logit))
summary(model_lr2)
#AIC:699.94

churn_train$prediction <- predict( model_lr2, newdata = churn_train, type = "response" )
pred_lm2  <- predict( model_lr2, newdata = churn_test , type = "response" )

pred_lm2_output <- prediction(pred_lm2,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm2_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm2_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm2 <- auc(churn_test$Churn,pred_lm2)
#AUC:0.82377

View(churn_train)

#Building the 3rd model
model_lr3<- glm (Churn~CustServ.Calls + Int.l.Plan + VMail.Plan +Day.Mins*CustServ.Calls+Int.l.Plan*Day.Mins, data = churn_train, family = binomial(link = logit))
summary(model_lr3)
#AIC:673.3

churn_train$prediction <- predict( model_lr3, newdata = churn_train, type = "response" )
pred_lm3  <- predict( model_lr3, newdata = churn_test , type = "response" )

pred_lm3_output <- prediction(pred_lm3,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm3_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm3_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm3 <- auc(churn_test$Churn,pred_lm3)
#AUC:0.8275

#_________________________________________________________________________________________________________________
## OVERSAMPLING TECHNIQUE

data_balanced_over<- ovun.sample(Churn ~ ., data = churn, method = "over", N = 5500, seed = 1)$data

table(data_balanced_over$Churn)
set.seed(400)
train=sample(1:nrow(data_balanced_over),round(nrow(data_balanced_over)*0.70),replace = F)
churn_train<-data_balanced_over[train,]
churn_test<-data_balanced_over[-train,]
table(churn_train$Churn)

#Building the first model
model_lr1<- glm (Churn ~., data = churn_train, family = binomial(link = logit))
summary(model_lr1)
#AIC:3927.8

churn_train$prediction <- predict( model_lr1, newdata = churn_train, type = "response" )
pred_lm1<- predict( model_lr1, newdata = churn_test , type = "response" )

pred_lm1_output <- prediction(pred_lm1,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm1_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm1_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm1<- auc(churn_test$Churn,pred_lm1)
#AUC:0.8396


#Building the second model
model_lr2<- glm (Churn ~CustServ.Calls + Day.Charge + Int.l.Plan + VMail.Plan + Day.Mins*CustServ.Calls, data = churn_train, family = binomial(link = logit))
summary(model_lr2)
#AIC:3852.7

churn_train$prediction <- predict( model_lr2, newdata = churn_train, type = "response" )
pred_lm2  <- predict( model_lr2, newdata = churn_test , type = "response" )

pred_lm2_output <- prediction(pred_lm2,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm2_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm2_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm2 <- auc(churn_test$Churn,pred_lm2)
#AUC:0.8575

#Building the 3rd model
model_lr3<- glm (Churn~CustServ.Calls + Int.l.Plan + VMail.Plan +Day.Mins*CustServ.Calls+Int.l.Plan*Day.Mins, data = churn_train, family = binomial(link = logit))
summary(model_lr3)
#AIC:3753.1

churn_train$prediction <- predict( model_lr3, newdata = churn_train, type = "response" )
pred_lm3  <- predict( model_lr3, newdata = churn_test , type = "response" )

pred_lm3_output <- prediction(pred_lm3,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm3_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm3_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm3 <- auc(churn_test$Churn,pred_lm3)
#AUC:0.8619
View(churn_train)

#_________________________________________________________________________________________________________________
## Mixed SAMPLING TECHNIQUE

data_balanced_over<- ovun.sample(Churn ~ ., data = churn, method = "both", N = 8000)$data

table(data_balanced_over$Churn)
set.seed(400)
train=sample(1:nrow(data_balanced_over),round(nrow(data_balanced_over)*0.70),replace = F)
churn_train<-data_balanced_over[train,]
churn_test<-data_balanced_over[-train,]
table(churn_train$Churn)

#Building the first model
model_lr1<- glm (Churn ~., data = churn_train, family = binomial(link = logit))
summary(model_lr1)
#AIC:5578.4

churn_train$prediction <- predict( model_lr1, newdata = churn_train, type = "response" )
pred_lm1<- predict( model_lr1, newdata = churn_test , type = "response" )

pred_lm1_output <- prediction(pred_lm1,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm1_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm1_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm1<- auc(churn_test$Churn,pred_lm1)
#AUC:0.8392


#Building the second model
model_lr2<- glm (Churn ~CustServ.Calls + Day.Charge + Int.l.Plan + VMail.Plan + Day.Mins*CustServ.Calls, data = churn_train, family = binomial(link = logit))
summary(model_lr2)
#AIC:5504

churn_train$prediction <- predict( model_lr2, newdata = churn_train, type = "response" )
pred_lm2  <- predict( model_lr2, newdata = churn_test , type = "response" )

pred_lm2_output <- prediction(pred_lm2,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm2_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm2_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm2 <- auc(churn_test$Churn,pred_lm2)
#AUC:0.8493

#Building the 3rd model
model_lr3<- glm (Churn~CustServ.Calls + Int.l.Plan + VMail.Plan +Day.Mins*CustServ.Calls+Int.l.Plan*Day.Mins, data = churn_train, family = binomial(link = logit))
summary(model_lr3)
#AIC:5379.1

churn_train$prediction <- predict( model_lr3, newdata = churn_train, type = "response" )
pred_lm3  <- predict( model_lr3, newdata = churn_test , type = "response" )

pred_lm3_output <- prediction(pred_lm3,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm3_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm3_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm3 <- auc(churn_test$Churn,pred_lm3)
#AUC:0.8566

#_________________________________________________________________________________________________________________
## SMOTE
library(DMwR)


data_balanced_over<- SMOTE(Churn ~ ., churn, k=3, perc.over = 350,perc.under=150)

table(data_balanced_over$Churn)
set.seed(400)
train=sample(1:nrow(data_balanced_over),round(nrow(data_balanced_over)*0.70),replace = F)
churn_train<-data_balanced_over[train,]
churn_test<-data_balanced_over[-train,]
table(churn_train$Churn)

#Building the first model
model_lr1<- glm (Churn ~., data = churn_train, family = binomial(link = logit))
summary(model_lr1)
#AIC:2810.1

churn_train$prediction <- predict( model_lr1, newdata = churn_train, type = "response" )
pred_lm1<- predict( model_lr1, newdata = churn_test , type = "response" )

pred_lm1_output <- prediction(pred_lm1,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm1_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm1_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm1<- auc(churn_test$Churn,pred_lm1)
#AUC:0.8525


#Building the second model
model_lr2<- glm (Churn ~CustServ.Calls + Day.Charge + Int.l.Plan + VMail.Plan + Day.Mins*CustServ.Calls, data = churn_train, family = binomial(link = logit))
summary(model_lr2)
#AIC:2849.4

churn_train$prediction <- predict( model_lr2, newdata = churn_train, type = "response" )
pred_lm2  <- predict( model_lr2, newdata = churn_test , type = "response" )

pred_lm2_output <- prediction(pred_lm2,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm2_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm2_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm2 <- auc(churn_test$Churn,pred_lm2)
#AUC:0.8403

#Building the 3rd model
model_lr3<- glm (Churn~CustServ.Calls + Int.l.Plan + VMail.Plan +Day.Mins*CustServ.Calls+Int.l.Plan*Day.Mins, data = churn_train, family = binomial(link = logit))
summary(model_lr3)
#AIC:2822.2

churn_train$prediction <- predict( model_lr3, newdata = churn_train, type = "response" )
pred_lm3  <- predict( model_lr3, newdata = churn_test , type = "response" )

pred_lm3_output <- prediction(pred_lm3,churn_test$Churn)
perfspec <- performance(prediction.obj = pred_lm3_output, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_lm3_output, measure="sens", x.measure="cutoff")

plot(perfsens)

auc_lm3 <- auc(churn_test$Churn,pred_lm3)
#AUC:0.8457
