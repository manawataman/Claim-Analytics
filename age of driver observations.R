### checking for age of driver 

claimsFullage <- claimsFull

### thoughts 
#1. replace extreme age with mean of 0 and check result and replace with mean of 1 and check results
#2. check in extreme values the tendensity to fraud 
#3. frequency graphs shows best buckets should be of 20 width 


tbl_age <- claimsFullage %>%
  
  filter(set=="train") %>%
  
  select(age_of_driver, fraud) %>%
  
  group_by(fraud) %>%
  
  summarise(mean.age_of_driver = mean(age_of_driver, na.rm=TRUE))


tbl_age


## density 
ggplot(claimsFullage %>% filter(set=="train"), aes(age_of_driver, fill=fraud)) +
  
  geom_histogram(aes(y=..density..), alpha=0.5) +
  
  geom_density(alpha=.2, aes(colour=fraud)) +
  
  geom_vline(data=tbl_age, aes(xintercept=mean.age_of_driver, colour=fraud), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Density") +
  
  ggtitle("fraud Rate by Age") + 
  
  theme_minimal()




## frequency 

# age 

ggplot(claimsFullage %>% filter(set=="train"), aes(age_of_driver, fill=fraud)) +
  
  geom_histogram(aes(y=..count..), alpha=0.5) +
  
  geom_vline(data=tbl_age, aes(xintercept=mean.age_of_driver, colour=fraud), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=comma) +
  
  ylab("Density") +
  
  ggtitle("fraud by Age") + 
  
  theme_minimal()


## checking extreme values and their relationship with fraud rate 

#age_extreme <- claimsFullage$age_of_driver[claimsFullage$age_of_driver>75]

#class(claimsFullage$age_of_driver)

age_extreme <- subset(claimsFullage,age_of_driver>75)
dim(age_extreme)

mean(claimsFull$age_of_driver)


tbl_age_extreme <- age_extreme %>%
  
  filter(set=="train") %>%
  
  select(age_of_driver, fraud) %>%
  
  group_by(fraud) %>%
  
  summarise(mean.age_of_driver = mean(age_of_driver, na.rm=TRUE))


tbl_age_extreme


ggplot(age_extreme %>% filter(set=="train"), aes(age_of_driver, fill=fraud)) +
  
  geom_histogram(aes(y=..count..), alpha=0.5) +
  
  geom_vline(data=tbl_age_extreme, aes(xintercept=mean.age_of_driver, colour=fraud), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=comma) +
  
  ylab("Density") +
  
  ggtitle("fraud by Age") + 
  
  theme_minimal()



### replacing extreme values above 80 with round of mean


age_extreme$age_of_driver <- age_extreme$age_of_driver %>% replace(
                                                                   which(age_extreme$age_of_driver>80),
                                                                   round(mean(age_extreme$age_of_driver)))

age_extreme$age_group 

# mutate in dplyr package add news variable and keep exsisting one 
age_extreme <- age_extreme %>%
  
  mutate(
     age_of_driver <- age_extreme$age_of_driver,
     age_group = case_when(age_of_driver <= 30 ~ "below 30", 
                            
                            age_of_driver > 30 & age_of_driver <=60 ~ "middle age",
                          
                            age_of_driver >= 60 ~ "above 60"))


age_extreme$age_group
