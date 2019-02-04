#### building the model on training set and testing on validation set 

# putting class weights by finding them using cv


claimsFinal <- claimsFull


claimsFinalTrain <- subset(claimsFinal,claimsFinal$set=="train")

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


#### glm model ####

classifier = glm(formula = fraud ~. -claim_number,
                 family = binomial,
                 data = training_set)

varImp(classifier)

pred <- predict(classifier,type = 'response',newdata = test_set[-13])

claim_prediction_val <- ifelse(pred>0.2,1,0)

# confusion matrix

cm <- table(test_set[,13],claim_prediction_val>0.2)
cm

pred <- as.factor(claim_prediction_val)
confusionMatrix(test_set$fraud,pred)
AUC(test_set[,13],pred)

