### modeling with decision tree 

claimsFinalDecisionTrees <- claimsFull


claimsFinalTrain <- subset(claimsFinalDecisionTrees,claimsFinalDecisionTrees$set=="train")
dim(claimsFinalTrain)
head(claimsFinalTrain)

claimsFinalTrain = subset(claimsFinalTrain,select=-c(set,
                                                     channel,
                                                     vehicle_weight,
                                                     vehicle_color,
                                                     vehicle_price,
                                                     liab_prct,
                                                     claim_day_of_week,
                                                     vehicle_category,
                                                     policy_report_filed_ind,
                                                     claim_est_payout,
                                                     claim_date,
                                                     month_of_claim,
                                                     city,
                                                     zip,
                                                     age_group,
                                                     day_of_week,
                                                     age_of_driver
                                                     
))

split = sample.split(claimsFinalTrain$fraud,SplitRatio = 0.70)
training_set <- subset(claimsFinalTrain,split == TRUE)
test_set <- subset(claimsFinalTrain, split == FALSE)

# Feature Scaling
#training_set[-3] = scale(training_set[-3])
#test_set[-3] = scale(test_set[-3])


# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
?rpart
ctrl = rpart.control(minsplit=2,maxdepth=3)
classifier = rpart(formula = fraud ~ .,
                   data = training_set,
                    minsplit=15,
                   method = 'class',
                   control = ctrl)



plot(classifier)

rpart.plot(classifier,extra =  5,fallen.leaves = T)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-12],type = 'p')
y_pred


y_pred

# Making the Confusion Matrix
cm = table(test_set[,12], y_pred)
cm

confusionMatrix(y_pred,test_set$fraud)



----- 
  
library(caret)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit <- train(fraud ~., data = training_set, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

print(dtree_fit)

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

head(test_set[13])

y_pred = predict(dtree_fit, newdata = test_set[-13])
y_pred


cm = table(test_set[,13], y_pred)
cm




