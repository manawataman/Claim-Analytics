#### building the model on test set


training_set_claims <- subset(claimsFull,claimsFull$set=="train")
dim(training_set_claims)
test_set_claims <- subset(claimsFull,claimsFull$set=="test")
dim(test_set_claims)

str(training_set_claims)

training_set_claims = subset(training_set_claims,select=-c(set,
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


test_set_claims = subset(test_set_claims,select=-c(set,
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


summary(is.na(training_set_claims$fraud))

classifier_claims = glm(formula = fraud ~.  -claim_number,
                        family = binomial,
                        data = training_set_claims)



pred_claims <- predict(classifier_claims,type = 'response',newdata = test_set_claims[-13])

predicted <- ifelse(pred_claims>0.2,1,0)

length(which(predicted==1))

predicted_final = as.data.frame(pred_claims)
final_set <- data.frame(test_set_claims$claim_number,predicted_final)

colnames(final_set) = c("claim_number","fraud")
head(final)
write.csv(final_set,file="samplesubmission20.csv")



## random forest 

#smote_train_set <- SMOTE(fraud ~., data = training_set_claims, rate = 2)
table(training_set_claims$fraud)
table(smote_train_set$fraud)
smote_train_set<-ubBalance(X= training_set_claims[,-13], Y=training_set_claims$fraud, type="ubSMOTE", 
                       percOver=310, percUnder=150
                       )
smote_train_set<-cbind(smote_train_set$X,smote_train_set$Y)
colnames(smote_train_set)[16] <- "fraud"

library(randomForest)

classifier = randomForest(x = smote_train_set[,-16],
                          y = smote_train_set$fraud,
                          ntree = 25)

classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set_claims[-13],type="prob")

length(which(y_pred>0.2))

predicted_final = as.data.frame(y_pred[,2])
final_set <- data.frame(test_set_claims$claim_number,predicted_final)

colnames(final_set) = c("claim_number","fraud")
length(which(final_set$fraud>0.2))

write.csv(final_set,file="samplesubmission17.csv")




#### Boosting and gbm 

?gbm
smote_train_set<-ubBalance(X= training_set_claims[,-13], Y=training_set_claims$fraud, type="ubSMOTE", 
                           percOver=310, percUnder=150
)
smote_train_set<-cbind(smote_train_set$X,smote_train_set$Y)
colnames(smote_train_set)[16] <- "fraud"

table(smote_train_set$fraud)

trctrl <- trainControl(method = "repeatedcv", 
                       number = 10, 
                       repeats = 3
)

dtree_fit <- caret::train(fraud ~., data = smote_train_set, method = "gbm",
                          trControl=trctrl, verbose = FALSE)
plot(dtree_fit)

y_pred = predict(dtree_fit, newdata = test_set[-13],type="prob")

length(which(y_pred>0.2))

predicted_final = as.data.frame(y_pred[,2])
final_set <- data.frame(test_set_claims$claim_number,predicted_final)

colnames(final_set) = c("claim_number","fraud")
length(which(final_set$fraud>0.2))

write.csv(final_set,file="samplesubmission19.csv")


