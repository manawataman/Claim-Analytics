## backward elimination 


# install.packages("leaps")
library(leaps)

claimsfinalbackward = subset(claimsFinal,select=-c(set,city))

regfit.bwd=regsubsets(fraud~.,
                      data=claimsfinalbackward,
                      nvmax =19,
                      method ="forward")

summary(regfit.bwd)
