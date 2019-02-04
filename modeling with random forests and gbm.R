#### random forests 


# Feature Scaling
#training_set[-3] = scale(training_set[-3])
#test_set[-3] = scale(test_set[-3])

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')

head(training_set[13])
library(randomForest)
set.seed(123)

#SMOTE before doing RF as data predictor class is highly unfiormly distributed 

# see trend 
table(training_set$fraud)

library(DMwR)
#smote_train <- SMOTE(fraud ~., data = training_set)

#install.packages("unbalanced")
library(unbalanced)

class(training_set$fraud)
table(training_set$fraud)

#smote_train<-ubBalance(X= training_set[,-13], Y=training_set$fraud, type="ubSMOTE", 
#                percOver=290,# percUnder = 100,
#                verbose=TRUE)
smote_train <- SMOTE(fraud~., data = training_set, 
                     perc.over = 190, 
                     perc.under = 580)

#smote_train<-cbind(smote_train$X,smote_train$Y)
#colnames(smote_train)[16] <- "fraud"
table(smote_train$fraud)

table(smote_train$fraud)[2]/(table(smote_train$fraud)[1]+table(smote_train$fraud)[2])

dim(smote_train)
head(smote_train)
head(smote_train[13])

head(smote_train[,-c(1,13)])

classifier = randomForest(x = smote_train[,-c(1,13)],
                          y = smote_train$fraud,
                          
                          ntree = 50
                          )

str(training_set)
varImpPlot(classifier)
varImp(classifier)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-13])

# Making the Confusion Matrix
cm = table(test_set[, 13], y_pred)
cm

confusionMatrix(test_set$fraud,y_pred)

installed.packages("pROC")
library("pROC")
AUC(test_set[,13],y_pred)

######fine tune | method 1 


library(caret)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search="random")
dtree_fit <- train(fraud ~., data = smote_train, method = "rf",
                   metric="Accuracy",
                   trControl=trctrl)
dim(smote_train)                  
print(dtree_fit)
plot(dtree_fit)
?trainControl

head(test_set[13])

y_pred = predict(dtree_fit, newdata = test_set[-13])
y_pred


cm = table(test_set[,13], y_pred)
cm

confusionMatrix(y_pred,test_set$fraud)



##### mehtod 2 

library(caret)

trctrl <- trainControl(method = "repeatedcv", 
                       number = 10, 
                       repeats = 3, 
                       sampling = "smote")
dtree_fit <- train(fraud ~., data = training_set, method = "rf",
                   metric="Accuracy",
                   trControl=trctrl)
dim(smote_train)                  
print(dtree_fit)
plot(dtree_fit)
?trainControl

head(test_set[13])

y_pred = predict(dtree_fit, newdata = test_set[-13])
y_pred


cm = table(test_set[,13], y_pred)
cm

confusionMatrix(y_pred,test_set$fraud)




# bagging

bag_claims <- randomForest(fraud~.,data = training_set,ntree=30)
bag_claims

head(test_set[13])
bag_pred = predict(bag_claims, newdata = test_set[-13])
bag_pred


bag_cm = table(test_set[,13], bag_pred)
bag_cm

importance(bag_claims)


# boosting 


# boosting



smote_train<-ubBalance(X= training_set[,-13], Y=training_set$fraud, type="ubSMOTE", 
                       percOver=190, percUnder=580, 
                       verbose=TRUE)
smote_train<-cbind(smote_train$X,smote_train$Y)
colnames(smote_train)[17] <- "fraud"
table(smote_train$fraud)


trctrl_val <- trainControl(method = "repeatedcv", 
                       number = 5, 
                       repeats = 1, 
                       search = "random"
                       )

dtree_fit_val <- caret::train(fraud ~. -claim_number, 
                              data = smote_train, 
                              method = "rf",
                              trControl=trctrl_val, 
                              tuneLength = 15)


plot(dtree_fit_val)
varImp(dtree_fit_val)
plot(varImp(dtree_fit_val))


y_pred_val = predict(dtree_fit_val, newdata = test_set[-13])


cm = table(test_set[,13], y_pred_val)
cm

confusionMatrix(y_pred_val,test_set$fraud)

AUC(test_set[,13],y_pred_val)

