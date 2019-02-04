#### outlier detection ####

#### vehicle weight ####
tbl_weight <- claimsFull %>%
  
  filter(set=="train") %>%
  
  select(vehicle_weight, fraud) %>%
  
  group_by(fraud) %>%
  
  summarise(mean.vehicle_weight = mean(vehicle_weight, na.rm=TRUE))


tbl_weight


## density 
ggplot(claimsFull %>% filter(set=="train"), aes(vehicle_weight, fill=fraud)) +
  
  geom_histogram(aes(y=..density..), alpha=0.5) +
  
  geom_density(alpha=.2, aes(colour=fraud)) +
  
  geom_vline(data=tbl_weight, aes(xintercept=mean.vehicle_weight, colour=fraud), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Density") +
  
  ggtitle("fraud Rate by weight") + 
  
  theme_minimal()




## frequency 

# age 

ggplot(claimsFull %>% filter(set=="train"), aes(vehicle_weight, fill=fraud)) +
  
  geom_histogram(aes(y=..count..), alpha=0.5) +
  
  geom_vline(data=tbl_weight, aes(xintercept=mean.vehicle_weight, colour=fraud), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=comma) +
  
  ylab("Density") +
  
  ggtitle("fraud by weight") + 
  
  theme_minimal()





## checking for outliers 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

length(which(is_outlier(claimsWeight)==TRUE))


claimsFull %>%
#  group_by(fraud) %>%
  mutate(outlier = ifelse(is_outlier(vehicle_weight), vehicle_weight, as.numeric(NA))) %>%
  ggplot(., aes(x = "vehicle", y = vehicle_weight)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)


## treating outliers values 

claimsWeight <- claimsFull$vehicle_weight

# replacing with mean
#claimsWeight <- claimsWeight %>% replace(which(is_outlier(claimsWeight)==TRUE),mean(claimsWeight))

# capping the values 


qnt <- quantile(claimsWeight, probs=c(.25, .75), na.rm = T)
caps <- quantile(claimsWeight, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(claimsWeight, na.rm = T)
claimsWeight[claimsWeight < (qnt[1] - H)] <- caps[1]
claimsWeight[claimsWeight > (qnt[2] + H)] <- caps[2]


## group according to vehicle weight and plotting relationships 


claimsFull <- claimsFull %>%
  
  mutate(
    weight_type = case_when( vehicle_weight < 10500 ~ "light_weight_vehicle", 
                          
                          vehicle_weight >= 10500 & vehicle_weight <=30000 ~ "mid_weight_vehicle",
                          
                          vehicle_weight >30000  ~ "heavy_weight_vehicle"))

claimsFull$weight_type <- as.factor(claimsFull$weight_type)

## relationship with fraud and age group density

ggplot(claimsFull %>% filter(set=="train" & !is.na(vehicle_weight)), aes(weight_type, fill=fraud)) +
  
  geom_bar(position = "stack") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=comma) +
  
  ylab("no of observations") +
  
  ggtitle("fraud by weight Group") + 
  
  theme_minimal()

#denstiy 
ggplot(claimsFull %>% filter(set=="train" & !is.na(vehicle_weight)), aes(weight_type, fill=fraud)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("fraud rate") +
  
  ggtitle("fraud by weight Group") + 
  
  theme_minimal()

