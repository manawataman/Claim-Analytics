# install libraries 

library(tidyverse)
library(stringr)
library(forcats)
library(caTools)

# data visulisation 

#install.packages("DT")
library(DT)
#install.packages("data.table")
library(data.table)
#install.packages("pander")
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
#install.packages("VIM")
library(VIM) 
library(knitr)
library(vcd)
#install.packages("caret")
library(caret)

# algorithms 

#install.packages("xgboost")
library(xgboost)
#install.packages("MLmetrics")
library(MLmetrics)
#install.packages("randomForest")
library('randomForest') 
library('rpart')
#install.packages("rpart.plot")
library('rpart.plot')
library('car')
library('e1071')
library(vcd)
library(ROCR)
#install.packages("pROC")
library(pROC)
library(VIM)
#install.packages("glmnet")
library(glmnet)

# zipcode 
#install.packages("zipcode")
library(zipcode)


#### PREPARING THE DATA SET ####
claimsTrain$set <- "train"
claimsTest$set <- "test"
claimsTest$fraud <- NA
claimsFullage <- rbind(claimsTrain,claimsTest)
head(claimsFull)
str(claimsFull)
dim(claimsFull)


#### correlation plot ####
tbl_corr <- claimsFull %>%
  
  filter(set=="train") %>%
  
  select(-claim_number) %>%
  
  select_if(is.numeric) %>%
  
  cor(use="complete.obs") %>%
  
  corrplot.mixed(tl.cex=0.85)


#### EDA #### 

# checking for unique values in columns 

lapply(claimsFull, function(x) length(unique(x)))
summary(is.na(claimsFull$marital_status))



#### age_of_driver ####

# checking for missing values in all columns 
summary(is.na(claimsFull$age_of_driver))

# no missing value

# replacing extreme values above 80 with round of mean

claimsFull$age_of_driver<- claimsFullage$age_of_driver

max(claimsFull$age_of_driver)

claimsFull$age_of_driver <- claimsFull$age_of_driver %>% replace(
  which(claimsFull$age_of_driver>80),round(mean(claimsFull$age_of_driver)))

str(claimsFull)
max(claimsFull$age_of_driver)

# bucketing 
# mutate in dplyr package add news variable and keep exsisting one 
claimsFull <- claimsFull %>%
  
  mutate(
    age_group = case_when(age_of_driver <= 60 ~ "below_60", 
                          
                          #age_of_driver > 50 & age_of_driver <=58 ~ "mid_age",
                          
                          age_of_driver > 60 ~ "above_60"))

claimsFull$age_group <- as.factor(claimsFull$age_group)

# relationship with fraud and age group density

ggplot(claimsFull %>% filter(set=="train" & !is.na(age_of_driver)), aes(age_group, fill=fraud)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("fraud Rate") +
  
  ggtitle("fraud by Age Group") + 
  
  theme_minimal()


#### gender ####
summary(is.na(claimsFull$gender))
#no missing value

#### marital status ####

summary(is.na(claimsFull$marital_status))
# has missing value

# replacing missing values with mode 
str(claimsFull$marital_status)

marry <- claimsFull$marital_status


# write own mode function, no package in R 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

x <- unique(marry)
x[which.max(tabulate(match(marry, unique(marry))))]

marry <- marry %>% replace(which(is.na(marry)),getmode(marry))

summary(is.na(marry))

claimsFull$marital_status <- marry

claimsFull$marital_status <- as.factor(claimsFull$marital_status)

str(claimsFull$marital_status)

#### safty_rating ####
summary(is.na(claimsFull$safty_rating))
# has no missing values

str(claimsFull$safty_rating)

#### annual_income #####
summary(is.na(claimsFull$annual_income))

# has no missing values, but has -1 in it 

mean(claimsFull$annual_income)

claimsFull$annual_income <- claimsFull$annual_income %>% replace(which(claimsFull$annual_income==-1),mean(claimsFull$annual_income))

str(claimsFull$annual_income)

#### high education indicator ####
summary(is.na(claimsFull$high_education_ind))
# has no missing values 

str(claimsFull$high_education_ind)

claimsFull$high_education_ind <- as.factor(claimsFull$high_education_ind)

#### address change indicator ####
summary(is.na(claimsFull$address_change_ind))
# no missing values 

str(claimsFull$address_change_ind)

claimsFull$address_change_ind <- as.factor(claimsFull$address_change_ind)


#### zip code ####
summary(is.na(claimsFull$zip_code))

# no missing values, but has 0 in it: how to handle that? - replaced with mode
# check how many unique values we have 

str(claimsFull$zip_code)

length(claimsFull$zip_code)

length(unique(claimsFull$zip_code))

length(which(claimsFull$zip_code==0)) 

# replace states having 0 zipcode with mode 

getmodezipcode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

claimsFull$zip_code <- claimsFull$zip_code %>% replace(which(claimsFull$zip_code==0),getmodezipcode(claimsFull$zip_code))

#install.packages("DMwR")
library(DMwR)


#### feature engineering ####
# making group based on states  

#backup 
#claimsFullBackup <- claimsFull
dim(claimsFullBackup)

claimsFull <- claimsFullBackup 

#claimsFull <- claimsFull[!(is.na(claimsFull$zip_code) | claimsFull$zip_code==0), ]

colnames(claimsFull)[10] <- "zip"

head(claimsFull[10])

head(zipcode)
newZipdf <- merge(claimsFull,zipcode,by="zip")
dim(newZipdf)

dim(subset(newZipdf, zip==80006))
str(newZipdf)
claimsFull <- subset(newZipdf,select =-c(latitude,longitude))

str(claimsFull)

claimsFull$state <- as.factor(claimsFull$state)

claimsFull$city <- as.factor(claimsFull$city)
length(which(zip==0))

#claimsFull$state <- as.character(claimsFull$state)
#claimsFull$state[claimsFull$state=="IA"] <- "others"
#claimsFull$state[claimsFull$state=="CO"] <- "others"
#claimsFull$state <- as.factor(claimsFull$state)
str(claimsFull$state)
write.csv(claimsFull,file="testing.csv")



#### CLAIM_DATE ####
summary(is.na(claimsFull$claim_date))
# no missing values, converted to date formart from factor  


class(Sys.Date())

claim_dt<-strptime(claimsFull$claim_date,format="%m/%d/%Y")
claim_dt<-as.Date(claim_dt,format="%Y-%m-%d") 
claimsFull$claim_date <- claim_dt

# making a new feature and bucketing date into months 

date_of_claim <- claimsFull$claim_date
class(date_of_claim)

month_of_claim <- as.factor(months(date_of_claim))
head(month_of_claim)
str(month_of_claim)
class(month_of_claim)
claimsFull$month_of_claim <- month_of_claim
head(claimsFull)

year_of_claim = as.factor(year(date_of_claim))
class(year_of_claim)
str(year_of_claim)

claimsFull$year_of_claim <- year_of_claim


#### claim_day_of_week ####

summary(is.na(claimsFull$claim_day_of_week))
# no missing values

str(claimsFull$claim_day_of_week)


# group weekdays and weekends 
str(claimsFull$claim_day_of_week)

day_of_week <- as.data.frame(claimsFull$claim_day_of_week)
class(day_of_week)
head(day_of_week)

colnames(day_of_week) <- "day"
class(day_of_week$day)
day_of_week$day = as.character(day_of_week$day)

day_of_week$day[day_of_week$day=="Monday"] <- "Weekday"
day_of_week$day[day_of_week$day=="Tuesday"] <- "Weekday"
day_of_week$day[day_of_week$day=="Wednesday"] <- "Weekday"
day_of_week$day[day_of_week$day=="Thursday"] <- "Weekday"
day_of_week$day[day_of_week$day=="Friday"] <- "Weekday"
day_of_week$day[day_of_week$day=="Saturday"] <- "Weekend"
day_of_week$day[day_of_week$day=="Sunday"] <- "Weekend"

unique(day_of_week$day)

day_of_week$day <- as.factor(day_of_week$day)
str(day_of_week)

claimsFull$day_of_week <- day_of_week$day
str(claimsFull)



####################### liab_prct#######################
summary(is.na(claimsFull$liab_prct))
#no missing values
str(claimsFull$liab_prct)

# grouping

str(claimsFull)

claimsFull <- claimsFull %>%
  
  mutate(
    liab_group = case_when(liab_prct <= 25 ~ "below25", 
                          
                           liab_prct > 25 & liab_prct <=75 ~ "mid",
                          
                           liab_prct > 75 ~ "above_75"))

claimsFull$liab_group <- as.factor(claimsFull$liab_group)


## relationship with fraud and age group density

ggplot(claimsFull %>% filter(set=="train" & !is.na(liab_group)), aes(liab_group, fill=fraud)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("fraud Rate") +
  
  ggtitle("fraud by liab_prct Group") + 
  
  theme_minimal()


#### age of vehicle ####

summary(is.na(claimsFull$age_of_vehicle))
# has missing values 

str(claimsFull$age_of_vehicle)

mean(claimsFull$age_of_vehicle,na.rm=TRUE)

age_vehicle <- claimsFull$age_of_vehicle %>% replace(which(is.na(claimsFull$age_of_vehicle)),mean(claimsFull$age_of_vehicle,na.rm=TRUE))

summary(is.na(age_vehicle))

claimsFull$age_of_vehicle <- age_vehicle


#### check for -1 value in fraud

summary(claimsFull$fraud)

which(claimsFull$fraud==(-1))

str(claimsFull$fraud)

getmodefraud <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

claims
claimsFull$fraud <- claimsFull$fraud %>% replace(which(claimsFull$fraud==(-1)),getmodefraud(claimsFull$fraud))


frd <- claimsFull$fraud

claimsFull$fraud <- frd

claimsFull$fraud <- as.factor(claimsFull$fraud)
